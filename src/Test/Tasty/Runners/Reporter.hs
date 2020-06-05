{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- An ingredient for tasty that prints a summary and outputs junit xml that works with jenkins.
--
-- Usage:
--
-- @
--     import Test.Tasty
--     import Test.Tasty.HUnit
--     import qualified Test.Tasty.Runners.Reporter as Reporter
--
--     main = defaultMainWithIngredients [Reporter.ingredient] tests
--
--     tests :: TestTree
-- @
--
-- Example output:
--
-- @
--
--     λ cabal test --test-show-details=always
--     ...
--     Test suite spec: RUNNING...
--     ↓ Unit tests
--     ✗ List comparison (smaller length)
--
--     test/Main.hs:19:
--     expected: LT
--      but got: GT
--
--
--     ↓ Unit tests
--     ↓ sub group
--     ✗ foo
--
--     Test threw an exception
--     user error (asdf)
--
--
--
--     TEST RUN FAILED
--     ^^^^^^^^^^^^^^^
--
--     Duration:  0.000s
--     Passed:    1
--     Failed:    2
-- @
module Test.Tasty.Runners.Reporter
  ( ingredient,

    -- * Support for skipping tests (example usage coming soon)
    SkippingTests (TestSkipped, TestOnly),

    -- * Support for running only one test (example usage coming soon)
    OnlyTestResult (OnlyTestPassed, OnlyTestFailed),
  )
where

import qualified Control.Concurrent.STM as STM
import qualified Control.Exception.Safe as Exception
import Control.Exception.Safe (displayException)
import Control.Monad (Monad)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.State as State
import Data.Function ((&))
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid (mappend, mempty), Sum (getSum))
import Data.Proxy (Proxy (Proxy))
import Data.Semigroup
import Data.String (fromString)
import Data.Tagged (Tagged (Tagged))
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Typeable (Typeable)
import Numeric (showFFloat)
import Prelude hiding (unlines)
import System.Console.ANSI (hSupportsANSIColor)
import System.Console.Concurrent (outputConcurrent, withConcurrentOutput)
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.FilePath (FilePath, takeDirectory)
import System.IO (stdout)
import Test.Console.Color (Style, Styled, black, green, grey, red, styled, underlined, unlines, unstyled, yellow)
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Options as Tasty
import qualified Test.Tasty.Runners as Tasty
import qualified Text.XML.JUnit as JUnit

-- | Ingredient for `Tasty.defaultMainWithIngredients`
-- Runs all tests and outputs a summary as well as the failing tests.
-- Optionally takes `--xml=report.xml` and outputs junit xml.
ingredient :: Tasty.Ingredient
ingredient =
  Tasty.TestReporter optionDescription $ \options testTree ->
    Tasty.lookupOption options
      & runner options testTree
      & Just

-- | Types for skipping tests. (example coming soon)
data SkippingTests = TestSkipped | TestOnly OnlyTestResult deriving (Show)

instance Exception.Exception SkippingTests

-- | Types for running only one test. (example coming soon)
data OnlyTestResult
  = OnlyTestPassed String
  | OnlyTestFailed String
  deriving (Show)

newtype JunitXMLPath = JunitXMLPath FilePath
  deriving (Typeable)

instance Tasty.IsOption (Maybe JunitXMLPath) where

  defaultValue = Nothing

  parseValue = Just . Just . JunitXMLPath

  optionName = Tagged "xml"

  optionHelp = Tagged "A file path to store the test results in JUnit-compatible XML"

data Summary
  = Summary
      { failures :: Sum Int,
        errors :: Sum Int,
        successes :: Sum Int,
        skipped :: Sum Int,
        hasOnly :: Bool,
        testSuites :: [JUnit.TestSuite]
      }

instance Monoid Summary where
  mempty =
    Summary
      { failures = mempty,
        errors = mempty,
        successes = mempty,
        skipped = mempty,
        hasOnly = False,
        testSuites = mempty
      }

instance Semigroup Summary where
  a <> b =
    Summary
      { failures = failures a <> failures b,
        errors = errors a <> errors b,
        successes = successes a <> successes b,
        skipped = skipped a <> skipped b,
        hasOnly = hasOnly a || hasOnly b,
        testSuites = testSuites a <> testSuites b
      }

-- TraversalT is a newtype that allows us to have a monoid instance for a monad.
newtype TraversalT m a = TraversalT {appTraversalT :: m a}

instance (Monad m, Monoid a) => Monoid (TraversalT m a) where
  mempty = TraversalT (pure mempty)

instance (Monad m, Semigroup a) => Semigroup (TraversalT m a) where
  TraversalT f1 <> TraversalT f2 = TraversalT $ do
    a <- f1
    b <- f2
    pure (a <> b)

type StateIO = State.StateT IntMap.Key IO

newtype GroupNames = GroupNames [Text]
  deriving (Monoid, Semigroup)

optionDescription :: [Tasty.OptionDescription]
optionDescription = [Tasty.Option (Proxy :: Proxy (Maybe JunitXMLPath))]

runner ::
  Tasty.OptionSet ->
  Tasty.TestTree ->
  Maybe JunitXMLPath ->
  IntMap.IntMap (STM.TVar Tasty.Status) ->
  IO (Tasty.Time -> IO Bool)
runner options testTree path statusMap = withConcurrentOutput $ do
  (summary, _) <-
    Tasty.foldTestTree
      Tasty.trivialFold
        { Tasty.foldSingle = runTest statusMap,
          Tasty.foldGroup = runGroup
        }
      options
      testTree
      & (\x -> x mempty)
      & appTraversalT
      & (\x -> State.runStateT x 0)
  pure (createOutputs summary path)

createOutputs :: Summary -> Maybe JunitXMLPath -> Tasty.Time -> IO Bool
createOutputs summary@Summary {errors, failures, testSuites, hasOnly} maybePath elapsedTime = do
  printSummary summary elapsedTime
  case maybePath of
    Nothing -> pure ()
    Just (JunitXMLPath path) -> do
      createPathDirIfMissing path
      JUnit.writeXmlReport path testSuites
  pure (getSum (failures `mappend` errors) == 0 && not hasOnly)

runTest ::
  IntMap.IntMap (STM.TVar Tasty.Status) ->
  o ->
  Tasty.TestName ->
  t ->
  GroupNames ->
  TraversalT StateIO Summary
runTest statusMap _ testName_ _ groupNames = TraversalT $ do
  let testName = Text.pack testName_
  index <- State.get
  result <- liftIO $ STM.atomically $ do
    status <-
      IntMap.lookup index statusMap
        & fromMaybe (error "Attempted to lookup test by index outside bounds")
        & STM.readTVar
    case status of
      Tasty.Done result -> pure result
      _ -> STM.retry
  _ <- State.modify (+ 1)
  liftIO (resultToSummary groupNames testName result)

resultToSummary :: GroupNames -> Text -> Tasty.Result -> IO Summary
resultToSummary groupNames testName Tasty.Result {Tasty.resultOutcome, Tasty.resultTime, Tasty.resultDescription} =
  case resultOutcome of
    Tasty.Success ->
      mempty
        { testSuites =
            [ JUnit.passed testName
                & JUnit.time resultTime
                & inSuite groupNames
            ],
          successes = Sum 1
        }
        & pure
    Tasty.Failure (Tasty.TestThrewException err) ->
      case Exception.fromException err of
        Just TestSkipped -> do
          printLines
            [ prettyPath [yellow] testName groupNames_,
              "Test was skipped",
              "\n"
            ]
          mempty
            { testSuites =
                [ JUnit.skipped testName
                    & inSuite groupNames
                ],
              skipped = Sum 1
            }
            & pure
        Just (TestOnly (OnlyTestPassed _)) -> do
          let errorMessage =
                unlines
                  [ "This test passed, but there is a `Test.only` in your test.",
                    "I failed the test, because it's easy to forget to remove `Test.only`."
                  ]
          printLines
            [ prettyPath [red] testName groupNames_,
              errorMessage,
              "\n"
            ]
          mempty
            { testSuites =
                [ JUnit.errored testName
                    & JUnit.time resultTime
                    & JUnit.errorMessage (unstyled errorMessage)
                    & inSuite groupNames
                ],
              hasOnly = True,
              successes = Sum 1
            }
            & pure
        Just (TestOnly (OnlyTestFailed str)) -> do
          printLines
            [ prettyPath [red] testName groupNames_,
              fromString str,
              "\n"
            ]
          mempty
            { testSuites =
                [ JUnit.failed testName
                    & JUnit.failureMessage "This test failed and contains a `Test.only. `Test.only` will also fail your build even if the test passes."
                    & JUnit.stderr (Text.pack str)
                    & JUnit.time resultTime
                    & inSuite groupNames
                ],
              errors = Sum 1
            }
            & pure
        _ -> do
          let errorMessage = "Test threw an exception"
          printLines
            [ prettyPath [red] testName groupNames_,
              errorMessage,
              fromString (displayException err),
              "\n"
            ]
          mempty
            { testSuites =
                [ JUnit.errored testName
                    & JUnit.stderr (Text.pack (displayException err))
                    & JUnit.errorMessage (unstyled errorMessage)
                    & JUnit.time resultTime
                    & inSuite groupNames
                ],
              errors = Sum 1
            }
            & pure
    Tasty.Failure (Tasty.TestTimedOut _) ->
      mempty
        { testSuites =
            [ JUnit.errored testName
                & JUnit.errorMessage "Test timed out"
                & JUnit.time resultTime
                & inSuite groupNames
            ],
          errors = Sum 1
        }
        & pure
    Tasty.Failure _ -> do
      printLines
        [ prettyPath [red] testName groupNames_,
          fromString resultDescription,
          "\n"
        ]
      mempty
        { testSuites =
            [ JUnit.failed testName
                & JUnit.stderr ("Test result:\n\n" <> Text.pack resultDescription)
                & JUnit.time resultTime
                & inSuite groupNames
            ],
          failures = Sum 1
        }
        & pure
  where
    (GroupNames groupNames_) = groupNames

runGroup ::
  String ->
  (GroupNames -> TraversalT StateIO Summary) ->
  GroupNames ->
  TraversalT StateIO Summary
runGroup groupName children (GroupNames groupNames) =
  Text.pack groupName : groupNames
    & GroupNames
    & children
    & appTraversalT
    & TraversalT

printSummary :: Summary -> Tasty.Time -> IO ()
printSummary Summary {failures, errors, successes, skipped, hasOnly} duration = do
  color <- hSupportsANSIColor stdout
  [ -- Title "TEST RUN ..."
    if hasOnly
      then
        styled [yellow, underlined] "TEST RUN INCOMPLETE"
          <> styled [yellow] " because there is an `only` in your tests."
      else
        if failedTestsTotal > 0
          then styled [red, underlined] "TEST RUN FAILED"
          else
            if skippedTestsTotal > 0
              then
                styled [yellow, underlined] "TEST RUN INCOMPLETE"
                  <> if skippedTestsTotal == 1
                    then styled [yellow] (" because there was " <> fromInt skippedTestsTotal <> " test skipped.")
                    else styled [yellow] (" because there were " <> fromInt skippedTestsTotal <> " tests skipped.")
              else styled [green, underlined] "TEST RUN PASSED",
    "\n\n",
    -- Infos
    -- Duration:  0.001s
    -- Passed:    23
    -- Skipped:   1
    -- Failed:    0
    styled [black] ("Duration:  " <> showTime duration <> "s"),
    "\n",
    styled [black] ("Passed:    " <> fromInt (getSum successes)),
    "\n",
    if skippedTestsTotal > 0
      then styled [black] ("Skipped:   " <> fromInt skippedTestsTotal <> "\n")
      else "",
    styled [black] ("Failed:    " <> fromInt failedTestsTotal),
    "\n\n"
    ]
    & mconcat
    & if color then outputConcurrent else outputConcurrent . unstyled
  where
    failedTestsTotal = getSum (failures <> errors)
    skippedTestsTotal = getSum skipped

prettyPath :: Style -> Text -> [Text] -> Styled Text
prettyPath style name path =
  mconcat
    [ reverse path
        & map (styled [grey] . (<>) "↓ ")
        & unlines,
      styled style ("✗ " <> name) <> "\n"
    ]

inSuite :: GroupNames -> JUnit.TestReport outcome -> JUnit.TestSuite
inSuite (GroupNames groupNames) = JUnit.inSuite (Text.intercalate "." groupNames)

printLines :: [Styled Text] -> IO ()
printLines ts = do
  color <- hSupportsANSIColor stdout
  if color
    then outputConcurrent (unlines ts)
    else outputConcurrent (unstyled $ unlines ts)

timeDigits :: Num p => p
timeDigits = 3

showTime :: Tasty.Time -> Text
showTime time = Text.pack (showFFloat (Just timeDigits) time "")

fromInt :: Int -> Text
fromInt = Text.pack . Prelude.show

createPathDirIfMissing :: FilePath -> IO ()
createPathDirIfMissing path = do
  dirPath <- fmap takeDirectory (canonicalizePath path)
  createDirectoryIfMissing True dirPath
