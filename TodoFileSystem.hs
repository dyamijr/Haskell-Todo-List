{-# LANGUAGE DeriveGeneric #-}

module TodoFileSystem (
    loadTodoList,
    saveTodoList
) where
import TodoTask
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import Control.Exception (catch, IOException)

instance ToJSON Task
instance FromJSON Task
instance ToJSON Priority
instance FromJSON Priority

-- File path to store the todo list
todoFilePath :: Maybe String -> FilePath
todoFilePath Nothing = "todoList.json"
todoFilePath (Just x) = x

-- Function to load the todo list from a file
loadTodoList :: Maybe String -> IO TodoList
loadTodoList fp = catch action handler
  where
    action = do
      content <- B.readFile (todoFilePath fp)
      case decode content of
        Just todoList -> return todoList
        Nothing -> do
          putStrLn "Error: Failed to parse todo list file. Starting with an empty list."
          return [] -- Default to an empty list if decoding fails
    handler :: IOException -> IO TodoList
    handler e = do
      putStrLn $ "Error loading todo list: " ++ show e
      return []

-- Function to save the todo list to a file
saveTodoList :: TodoList -> Maybe String -> IO ()
saveTodoList todoList fp = catch action handler
  where
    action = B.writeFile (todoFilePath fp) (encode todoList)
    handler :: IOException -> IO ()
    handler e = do
      putStrLn $ "Error saving todo list: " ++ show e