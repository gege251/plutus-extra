{-# LANGUAGE Trustworthy #-}

{- |
 Module: Test.Tasty.Plutus.Script.Unit
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 A unit-test-like interface for validator and minting policy testing.

 = Example usage

 > validatorTests :: TestTree
 > validatorTests = withTestScript "Testing my spending" myValidator $ do
 >    shouldValidate "Valid case" validData validContext
 >    shouldn'tValidate "Invalid context" validData invalidContext
 >    shouldn'tValidate "Invalid data" invalidData validContext
 >    shouldn'tValidate "Everything is bad" invalidData invalidContext
 >    shouldValidateTracing "Gotta get good messages" tracePred validData validContext
 >    shouldn'tValidateTracing "Oh damn" tracePred invalidData validContext
 >    ...
-}
module Test.Tasty.Plutus.Script.Unit (
  -- * Testing API
  shouldValidate,
  shouldn'tValidate,
  shouldValidateTracing,
  shouldn'tValidateTracing,
) where

import Control.Arrow ((>>>))
import Control.Monad.Reader (Reader, asks, runReader)
import Control.Monad.Writer (tell)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Sequence qualified as Seq
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Plutus.V1.Ledger.Api (ScriptContext)
import Plutus.V1.Ledger.Scripts (
  ScriptError (
    EvaluationError,
    EvaluationException,
    MalformedScript
  ),
 )
import Test.Plutus.ContextBuilder (
  ContextBuilder,
  Naming,
  Purpose (ForMinting, ForSpending),
  TransactionConfig,
 )
import Test.Tasty.Options (
  OptionDescription (Option),
  OptionSet,
  lookupOption,
 )
import Test.Tasty.Plutus.Internal.Env (
  SomeScript (SomeMinter, SomeSpender),
  getContext,
  getScriptContext,
  getScriptResult,
  prepareConf,
 )
import Test.Tasty.Plutus.Internal.Feedback (
  didn'tLog,
  doPass,
  dumpState,
  internalError,
  malformedScript,
  noOutcome,
  noParse,
  scriptException,
  unexpectedFailure,
  unexpectedSuccess,
 )
import Test.Tasty.Plutus.Internal.Run (
  ScriptResult (
    InternalError,
    NoOutcome,
    ParseFailed,
    ScriptFailed,
    ScriptPassed
  ),
 )
import Test.Tasty.Plutus.Internal.TestScript (
  TestScript,
  getTestMintingPolicy,
  getTestValidator,
 )
import Test.Tasty.Plutus.Internal.WithScript (
  WithScript (WithMinting, WithSpending),
 )
import Test.Tasty.Plutus.Options (
  Fee,
  PlutusTracing,
  ScriptInputPosition,
  TestCurrencySymbol,
  TestTxId,
  TestValidatorHash,
  TimeRange,
 )
import Test.Tasty.Plutus.TestData (
  Outcome (Fail, Pass),
  TestData (MintingTest, SpendingTest),
 )
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  Result,
  singleTest,
  testFailed,
  testPassed,
 )
import Text.PrettyPrint (Doc)
import Type.Reflection (Typeable)

{- | Specify that, given this test data and context, the validation should
 succeed.

 @since 9.0
-}
shouldValidate ::
  forall (p :: Purpose) (n :: Naming).
  (Typeable p, Typeable n) =>
  String ->
  TestData p ->
  ContextBuilder p n ->
  WithScript p ()
shouldValidate = addUnitTest Pass Nothing

{- | Specify that, given this test data and context, as well as a predicate on
 the entire trace:

 * The validation should succeed; and
 * The trace that results should satisfy the predicate.

 @since 9.0
-}
shouldValidateTracing ::
  forall (p :: Purpose) (n :: Naming).
  (Typeable p, Typeable n) =>
  String ->
  (Vector Text -> Bool) ->
  TestData p ->
  ContextBuilder p n ->
  WithScript p ()
shouldValidateTracing name f = addUnitTest Pass (Just f) name

{- | Specify that, given this test data and context, the validation should fail.

 @since 9.0
-}
shouldn'tValidate ::
  forall (p :: Purpose) (n :: Naming).
  (Typeable p, Typeable n) =>
  String ->
  TestData p ->
  ContextBuilder p n ->
  WithScript p ()
shouldn'tValidate = addUnitTest Fail Nothing

{- | Specify that, given this test data and context, as well as a predicate on
 the entire trace:

 * The validation should fail; and
 * The resulting trace should satisfy the predicate.

 @since 9.0
-}
shouldn'tValidateTracing ::
  forall (p :: Purpose) (n :: Naming).
  (Typeable p, Typeable n) =>
  String ->
  (Vector Text -> Bool) ->
  TestData p ->
  ContextBuilder p n ->
  WithScript p ()
shouldn'tValidateTracing name f = addUnitTest Fail (Just f) name

-- Helpers

addUnitTest ::
  forall (p :: Purpose) (n :: Naming).
  (Typeable p, Typeable n) =>
  Outcome ->
  Maybe (Vector Text -> Bool) ->
  String ->
  TestData p ->
  ContextBuilder p n ->
  WithScript p ()
addUnitTest out trace name td cb = case td of
  SpendingTest {} -> WithSpending $ do
    tt <- asks (singleTest name . Spender out trace td cb)
    tell . Seq.singleton $ tt
  MintingTest {} -> WithMinting $ do
    tt <- asks (singleTest name . Minter out trace td cb)
    tell . Seq.singleton $ tt

data ScriptTest (p :: Purpose) (n :: Naming) where
  Spender ::
    forall (d :: Type) (r :: Type) (n :: Naming).
    Outcome ->
    Maybe (Vector Text -> Bool) ->
    TestData ( 'ForSpending d r) ->
    ContextBuilder ( 'ForSpending d r) n ->
    TestScript ( 'ForSpending d r) ->
    ScriptTest ( 'ForSpending d r) n
  Minter ::
    forall (r :: Type) (n :: Naming).
    Outcome ->
    Maybe (Vector Text -> Bool) ->
    TestData ( 'ForMinting r) ->
    ContextBuilder ( 'ForMinting r) n ->
    TestScript ( 'ForMinting r) ->
    ScriptTest ( 'ForMinting r) n

data UnitEnv (p :: Purpose) (n :: Naming) = UnitEnv
  { envOpts :: OptionSet
  , envScriptTest :: ScriptTest p n
  }

getShouldChat ::
  forall (p :: Purpose) (n :: Naming).
  UnitEnv p n ->
  PlutusTracing
getShouldChat = lookupOption . envOpts

getConf ::
  forall (p :: Purpose) (n :: Naming).
  UnitEnv p n ->
  TransactionConfig
getConf = prepareConf envOpts

getCB ::
  forall (p :: Purpose) (n :: Naming).
  UnitEnv p n ->
  ContextBuilder p n
getCB =
  envScriptTest >>> \case
    Spender _ _ _ cb _ -> cb
    Minter _ _ _ cb _ -> cb

getTestData ::
  forall (p :: Purpose) (n :: Naming).
  UnitEnv p n ->
  TestData p
getTestData =
  envScriptTest >>> \case
    Spender _ _ td _ _ -> td
    Minter _ _ td _ _ -> td

getScript ::
  forall (p :: Purpose) (n :: Naming).
  UnitEnv p n ->
  SomeScript p
getScript =
  envScriptTest >>> \case
    Spender _ _ _ _ val -> SomeSpender . getTestValidator $ val
    Minter _ _ _ _ mp -> SomeMinter . getTestMintingPolicy $ mp

getMPred ::
  forall (p :: Purpose) (n :: Naming).
  UnitEnv p n ->
  Maybe (Vector Text -> Bool)
getMPred =
  envScriptTest >>> \case
    Spender _ mPred _ _ _ -> mPred
    Minter _ mPred _ _ _ -> mPred

getExpected ::
  forall (p :: Purpose) (n :: Naming).
  UnitEnv p n ->
  Outcome
getExpected =
  envScriptTest >>> \case
    Spender expected _ _ _ _ -> expected
    Minter expected _ _ _ _ -> expected

getSC ::
  forall (p :: Purpose) (n :: Naming).
  UnitEnv p n ->
  ScriptContext
getSC = getScriptContext getConf getCB getTestData

getDumpedState ::
  forall (p :: Purpose) (n :: Naming).
  [Text] ->
  UnitEnv p n ->
  Doc
getDumpedState = dumpState getConf getCB getTestData

instance (Typeable p, Typeable n) => IsTest (ScriptTest p n) where
  run opts vt _ = pure $ case result of
    Left err -> runReader (handleError err) env
    Right (logs, sr) -> runReader (deliverResult logs sr) env
    where
      env :: UnitEnv p n
      env =
        UnitEnv
          { envOpts = opts
          , envScriptTest = vt
          }
      result :: Either ScriptError ([Text], ScriptResult)
      result = getScriptResult getScript getTestData (getContext getSC) env
  testOptions =
    Tagged
      [ Option @Fee Proxy
      , Option @TimeRange Proxy
      , Option @TestTxId Proxy
      , Option @TestCurrencySymbol Proxy
      , Option @TestValidatorHash Proxy
      , Option @PlutusTracing Proxy
      , Option @ScriptInputPosition Proxy
      ]

handleError ::
  forall (p :: Purpose) (n :: Naming).
  ScriptError ->
  Reader (UnitEnv p n) Result
handleError = \case
  EvaluationError logs msg ->
    asks getExpected >>= \case
      Pass -> asks (testFailed . unexpectedFailure (getDumpedState logs) msg)
      Fail -> asks getMPred >>= (`tryPass` logs)
  EvaluationException name msg -> pure . testFailed $ scriptException name msg
  MalformedScript msg -> pure . testFailed $ malformedScript msg

deliverResult ::
  forall (p :: Purpose) (n :: Naming).
  [Text] ->
  ScriptResult ->
  Reader (UnitEnv p n) Result
deliverResult logs result = do
  expected <- asks getExpected
  case (expected, result) of
    (_, NoOutcome) -> asks (testFailed . noOutcome state)
    (Fail, ScriptPassed) -> asks (testFailed . unexpectedSuccess state)
    (Fail, ScriptFailed) -> asks getMPred >>= (`tryPass` logs)
    (Pass, ScriptPassed) -> asks getMPred >>= (`tryPass` logs)
    (Pass, ScriptFailed) -> asks (testFailed . unexpectedFailure state mempty)
    (_, InternalError t) -> asks (testFailed . internalError state t)
    (_, ParseFailed t) -> asks (testFailed . noParse state t)
  where
    state :: UnitEnv p n -> Doc
    state = getDumpedState logs

tryPass ::
  forall (p :: Purpose) (n :: Naming).
  Maybe (Vector Text -> Bool) ->
  [Text] ->
  Reader (UnitEnv p n) Result
tryPass mPred logs = case mPred of
  Nothing -> asks (testPassed . doPass getShouldChat logs)
  Just f ->
    let logs' = Vector.fromList logs
     in if f logs'
          then asks (testPassed . doPass getShouldChat logs)
          else asks (testFailed . didn'tLog (getDumpedState logs))
