module Main where

import TodoTask
import TodoState
import TodoParser
import TodoUI
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State

main :: IO ()
main = do
  printWelcomeMessage
  evalStateT mainLoop []


mainLoop :: TodoListState()
mainLoop = do
  liftIO printPrompt
  input <- liftIO getLine
  case parseCommand input of
    Just command -> 
      if command == Quit
        then liftIO printExitMessage
        else do
          executeCommand command
          mainLoop
    Nothing -> do
      liftIO printInvalidCommand
      mainLoop

executeCommand :: Command -> TodoListState()
executeCommand (AddTask desc prio dueDate) = 
  addTask desc prio dueDate

executeCommand (RemoveTask idx) =
  removeTaskByIdx idx

executeCommand (CompleteTask idx) =
  markTaskCompleteByIdx idx

executeCommand ListTasks = do
  list <- get
  liftIO $ printTodoList list
  return ()