module Main where

import Test.HUnit
import TodoFileSystemSpecs
import TodoTaskSpec

-- Compose all tests
allTests :: Test   
allTests = TestList
  [ "Todo Task Tests" ~: todoTaskTests,
    "File System Tests" ~: fileSystemTests
  ]
-- Main function to run the tests
main :: IO ()
main = do
  _ <- runTestTT allTests
  return ()