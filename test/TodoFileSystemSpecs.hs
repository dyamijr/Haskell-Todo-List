{-# LANGUAGE OverloadedStrings #-}

module TodoFileSystemSpecs where

import Test.HUnit
import TodoFileSystem
import TodoTask
import System.Directory (removeFile)
import qualified Data.ByteString.Lazy as B

-- Define a file path for test purposes
testFilePath :: FilePath
testFilePath = "test_todolist.json"

-- Test cases for TodoFileSystem
fileSystemTests :: Test
fileSystemTests = TestList
  [ "Save and Load Valid Todo List With Path" ~: do
      let todos = [Task {description = "Task 1", completed = False, priority = High, dueDate = Nothing, categories = []}, 
                    Task {description = "Task 2", completed = True, priority = Medium, dueDate = Nothing, categories = []}]
      saveTodoList todos (Just testFilePath)
      loadedTodos <- loadTodoList (Just testFilePath)
      removeFile testFilePath -- Cleanup
      assertEqual "The loaded todos should match the saved todos" todos loadedTodos

  , "Handle Non-Existent File Gracefully" ~: do
      let nonExistentFile = "non_existent_file.json"
      todos <- loadTodoList (Just nonExistentFile)
      assertEqual "Loading a non-existent file should return an empty list" [] todos

  , "Handle Invalid JSON File" ~: do
      -- Write invalid JSON to the test file
      B.writeFile testFilePath "{invalid json}"
      todos <- loadTodoList (Just testFilePath)
      removeFile testFilePath -- Cleanup
      assertEqual "Loading an invalid JSON file should return an empty list" [] todos

  , "Handle Empty File" ~: do
      -- Write empty content to the test file
      B.writeFile testFilePath ""
      todos <- loadTodoList (Just testFilePath)
      removeFile testFilePath -- Cleanup
      assertEqual "Loading an empty file should return an empty list" [] todos
  ]
