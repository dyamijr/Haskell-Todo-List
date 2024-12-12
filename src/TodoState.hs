module TodoState
  ( TodoListState (..),
    addTask,
    markTaskCompleteByIdx,
    removeTaskByIdx,
    getTodoList,
    sortTasks,
    editTaskbyIdx,
    addCategoryByIdx,
  )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State
import Data.Maybe (fromJust)
import Data.Time (Day)
import TodoTask

type TodoListState = StateT TodoList IO

-- Add a new task to the todo list
addTask :: String -> Priority -> Maybe Day -> TodoListState ()
addTask desc prio due = modify (\list -> list ++ [createTask desc prio due])

-- Mark a task as complete by index
markTaskCompleteByIdx :: Int -> TodoListState ()
markTaskCompleteByIdx idx =
  modify
    ( \list ->
        let (before, task : after) = splitAt idx list
         in before ++ [completeTask task] ++ after
    )

-- Add a category to a task by index
addCategoryByIdx :: Int -> String -> TodoListState ()
addCategoryByIdx idx category =
  modify
    ( \list ->
        let (before, task : after) = splitAt idx list
         in before ++ [categorizeTask task category] ++ after
    )

-- Remove a task by index
removeTaskByIdx :: Int -> TodoListState ()
removeTaskByIdx idx = modify (removeTaskByIndex idx)

-- Edit a task by index
editTaskbyIdx :: Int -> String -> Maybe String -> Maybe Priority -> Maybe Day -> TodoListState ()
editTaskbyIdx idx "desc" newDesc _ _ = do
  tasks <- get
  let (before, target : after) = splitAt idx tasks
      updatedTask = target {description = fromJust newDesc}
      updatedTasks = before ++ (updatedTask : after)
  put updatedTasks -- Update the state with the modified task list
editTaskbyIdx idx "priority" _ newPrio _ = do
  tasks <- get
  let (before, target : after) = splitAt idx tasks
      updatedTask = target {priority = fromJust newPrio}
      updatedTasks = before ++ (updatedTask : after)
  put updatedTasks -- Update the state with the modified task list
editTaskbyIdx idx "date" _ _ newDate = do
  tasks <- get
  let (before, target : after) = splitAt idx tasks
      updatedTask = target {dueDate = newDate}
      updatedTasks = before ++ (updatedTask : after)
  put updatedTasks -- Update the state with the modified task list

-- Get the current todo list
getTodoList :: State TodoList TodoList
getTodoList = get

-- Sort tasks by priority
sortTasks :: Maybe String -> TodoListState ()
sortTasks option = case option of
  Nothing -> modify sortByPriority
  Just "priority" -> modify sortByPriority
  Just "date" -> modify sortByDate
  Just "desc" -> modify sortByDesc