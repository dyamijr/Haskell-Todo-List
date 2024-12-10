module Main where

import TodoTask
import TodoState
import TodoParser
import TodoUI
import TodoFileSystem
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State

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

executeCommand (EditTask idx edit desc prio dueDate) =
  editTaskbyIdx idx edit desc prio dueDate

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

executeCommand (SortList option) = 
  sortTasks option

executeCommand ShowCommands = do
  liftIO printPrompt
  return ()

executeCommand (ListTasks option) = case option of 
                                      Nothing -> do 
                                                  list <- get
                                                  liftIO $ printTodoList list
                                                  return ()
                                      Just "completed" -> do
                                                            list <- get
                                                            let filtered = filter completed list
                                                            liftIO $ printTodoList filtered
                                                            return ()
                                      Just "high priority" -> do
                                                            list <- get
                                                            let filtered = filter (\task -> priority task == High) list
                                                            liftIO $ printTodoList filtered
                                                            return ()
                                      Just "medium priority" -> do
                                                            list <- get
                                                            let filtered = filter (\task -> priority task == Medium) list
                                                            liftIO $ printTodoList filtered
                                                            return ()
                                      Just "low priority" -> do
                                                            list <- get
                                                            let filtered = filter (\task -> priority task == Low) list
                                                            liftIO $ printTodoList filtered
                                                            return ()
                                      Just category -> do
                                                            list <- get
                                                            let filtered = filter (\task -> category `elem` categories task) list
                                                            liftIO $ printTodoList filtered
                                                            return ()
