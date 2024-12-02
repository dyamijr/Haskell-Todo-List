module Main where

import TodoTask
import TodoState
import TodoParser
import TodoUI
import TodoFileSystem
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import TodoParser (Command(SaveList, ShowCommands))

main :: IO ()
main = do
  printWelcomeMessage
  printPrompt
  evalStateT mainLoop []


mainLoop :: TodoListState()
mainLoop = do
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

executeCommand (SaveList fp) = do
  list <- get
  liftIO $ saveTodoList list fp
  return ()

executeCommand (LoadList fp) = do
  list <- liftIO $ loadTodoList fp
  put list
  return ()

executeCommand ShowCommands = do
  liftIO printPrompt
  return ()

executeCommand ListTasks = do
  list <- get
  liftIO $ printTodoList list
  return ()