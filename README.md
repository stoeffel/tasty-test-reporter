# tasty-test-reporter

[![Build Status](https://travis-ci.com/stoeffel/tasty-test-reporter.svg?branch=master)](https://travis-ci.com/stoeffel/tasty-test-reporter)

An ingredient for tasty that prints a summary and outputs junit xml that works with jenkins.

![output from reporter](https://github.com/stoeffel/tasty-test-reporter/blob/master/output.png)

## Setting up this ingredient for tasty.

```hs
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.Runners.Reporter as Reporter

main = defaultMainWithIngredients [Reporter.ingredient] tests

tests :: TestTree
```

## Running tests with cabal

```bash
$ cabal test --test-show-details=always --test-options "--xml=report.xml"
```
