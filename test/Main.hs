{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.Runners.Reporter as Reporter

main = defaultMainWithIngredients [Reporter.ingredient] tests

tests :: TestTree
tests =
  testGroup
    "Unit tests"
    [ testCase "List comparison (smaller length)" $ [1, 2] `compare` [1, 2, 3] @?= LT
    ]
-- Example for failing tests
-- tests :: TestTree
-- tests =
--   testGroup
--     "Unit tests"
--     [ testCase "List comparison (smaller length)" $
--         [1, 2, 3] `compare` [1, 2] @?= LT,
--       testCase "List comparison (longer length)" $
--         [1, 2] `compare` [1, 2, 3] @?= GT,
--       testCase "List comparison (EQ length)" $
--         [1, 2, 3] `compare` [1, 2, 3] @?= EQ,
--       testGroup
--         "sub group"
--         [testCase "foo" $ fail "asdf"]
--     ]
