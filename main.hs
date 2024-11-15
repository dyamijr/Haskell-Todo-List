module Main where

import TodoTask
import TodoState
import TodoParser
import TodoUI
import Control.Monad
import Control.Monad.IO.Class

main :: IO ()
main = do
  printWelcomeMessage
  mainLoop (S (const ((), [])))


mainLoop :: TodoListState TodoList () -> IO ()
mainLoop state = do
  printPrompt
  input <- getLine
  case parseCommand input of
    Just command -> do
      if command == Quit
        then printExitMessage
        else do
          let newState = executeCommand command
          let ((), list) = runState newState []
          -- We should not be printing each time probably should make a command show todo or sum like that 
          printTodoList list
          printState newState
          mainLoop newState
    Nothing -> do
      printInvalidCommand
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

