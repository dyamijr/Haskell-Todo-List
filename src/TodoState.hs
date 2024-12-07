module TodoState (
    TodoListState(..), 
    addTask, 
    markTaskCompleteByIdx, 
    removeTaskByIdx, 
    getTodoList, 
    sortTasks, 
) where

import Data.Time (Day)
import Control.Monad
import TodoTask
import Control.Monad.Trans.State
import Control.Monad.IO.Class (MonadIO)

type TodoListState = StateT TodoList IO

-- Add a new task to the todo list
addTask :: String -> Priority -> Maybe Day -> TodoListState ()
addTask desc prio due = modify (\list -> list ++ [createTask desc prio due])

-- Mark a task as complete by index
markTaskCompleteByIdx :: Int -> TodoListState ()
markTaskCompleteByIdx idx = modify (\list -> 
    let (before, task:after) = splitAt idx list 
    in before ++ [completeTask task] ++ after)

-- Add a category to a task by index
addCategoryByIdX :: Int -> String -> TodoListState()
addCategoryByIdX idx category = modify (\list -> 
    let (before, task:after) = splitAt idx list 
    in before ++ [categorizeTask task category] ++ after)

-- Remove a task by index
removeTaskByIdx :: Int -> TodoListState()
removeTaskByIdx idx = modify (removeTaskByIndex idx)

-- Get the current todo list
getTodoList :: State TodoList TodoList
getTodoList = get

-- Sort tasks by priority
sortTasks :: Maybe String -> TodoListState()
sortTasks option = case option of 
                        Nothing -> modify sortByPriority
                        Just "priority" -> modify sortByPriority
                        Just "date" -> modify sortByDate
                        Just "desc" -> modify sortByDesc