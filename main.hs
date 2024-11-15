module Main where

import TodoTask
import TodoState
import TodoParser
import TodoUI
import Control.Monad
import Control.Monad.IO.Class

main :: IO ()
main = do
  putStrLn "Todo App"
  mainLoop (S (const ((), [])))


mainLoop :: TodoListState TodoList () -> IO ()
mainLoop state = do
  putStrLn "Enter command:"
  input <- getLine
  case parseCommand input of
    Just command -> do
      if command == Quit
        then putStrLn "Exiting..."
        else do
          let newState = executeCommand command
          printState newState
          mainLoop newState
    Nothing -> do
      putStrLn "Invalid command, please try again."
      mainLoop state

executeCommand :: Command -> TodoListState TodoList ()
executeCommand (AddTask desc prio dueDate) = do
  addTask desc prio dueDate

executeCommand (RemoveTask idx) = do
  removeTaskByIdx idx

executeCommand (CompleteTask idx) = do
  markTaskCompleteByIdx idx

executeCommand ListTasks = do
  list <- getTodoList
  return ()

printState :: TodoListState TodoList () -> IO ()
printState state = do
  let ((), list) = runState state []
  liftIO $ putStrLn "Current Todo List:"
  liftIO $ mapM_ print list

