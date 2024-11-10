module TodoState (

) where

import Data.Time (Day)
import Control.Monad.State
import TodoTask

-- A State monad for managing the todo list
type TodoListState = State TodoList

-- Add a new task to the todo list
addTask :: String -> Priority -> Maybe Day -> TodoListState ()
addTask desc prio due = modify (\list -> list ++ [createTask desc prio due])

-- Mark a task as complete by index
markTaskCompleteByIndex :: Int -> TodoListState ()
markTaskCompleteByIndex idx = modify (\list -> 
    let (before, task:after) = splitAt idx list 
    in before ++ [completeTask task] ++ after)

-- Remove a task by index
removeTaskByIdx :: Int -> TodoListState ()
removeTaskByIdx idx = modify (removeTask idx)

-- Get the current todo list
getTodoList :: TodoListState TodoList
getTodoList = get

-- Sort tasks by priority
sortTasks :: TodoListState ()
sortTasks = modify sortByPriority

-- Run a todo app with the initial state and actions
runTodoApp :: TodoListState a -> TodoList -> (a, TodoList)
runTodoApp = runState