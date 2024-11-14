module TodoState (
    TodoListState, 
    addTask, 
    markTaskCompleteByIdx, 
    removeTaskByIdx, 
    getTodoList, 
    sortTasks, 
    runTodoApp
) where

import Data.Time (Day)
import Control.Monad
import TodoTask

-- A State monad for managing the todo list
newtype TodoListState s a = S (TodoList -> (a, TodoList)) -- State TodoList

--  
runState :: TodoListState s a -> (TodoList -> (a,TodoList)) 
runState (S f) = f 

instance Monad (TodoListState s) where
    -- return :: a -> State s a
    --return x = S (x,)
    -- (>>=)
    st >>= f = S (\lis -> let (x,s') = runState st lis in runState (f x) s') 

instance Functor (TodoListState s) where
    -- fmap :: (a -> b) -> (State s a) -> (State s b)
    fmap f st = S (\s -> 
        let (a,s') = runState st s in (f a, s')) 

instance Applicative (TodoListState s) where
    -- pure :: a -> State s a
    pure = pure
    -- (<*>) :: State s (a->b) -> State s a -> State s b
    (<*>) = ap

get :: TodoListState TodoList TodoList
get = S $ \lis -> (lis, lis)

put :: TodoList -> TodoListState s ()
put lis = S $ const ((),lis)

modify :: (TodoList -> TodoList) -> TodoListState TodoList ()
modify f = get >>= \s -> put (f s)

-- Add a new task to the todo list
addTask :: String -> Priority -> Maybe Day -> TodoListState TodoList ()
addTask desc prio due = modify (\list -> list ++ [createTask desc prio due])

-- Mark a task as complete by index
markTaskCompleteByIdx :: Int -> TodoListState TodoList ()
markTaskCompleteByIdx idx = modify (\list -> 
    let (before, task:after) = splitAt idx list 
    in before ++ [completeTask task] ++ after)

-- Add a category to a task by index
addCategoryByIdX :: Int -> String -> TodoListState TodoList ()
addCategoryByIdX idx category = modify (\list -> 
    let (before, task:after) = splitAt idx list 
    in before ++ [categorizeTask task category] ++ after)

-- Remove a task by index
removeTaskByIdx :: Int -> TodoListState TodoList ()
removeTaskByIdx idx = modify (removeTaskByIndex idx)

-- Get the current todo list
getTodoList :: TodoListState TodoList TodoList
getTodoList = get

-- Sort tasks by priority
sortTasks :: TodoListState TodoList ()
sortTasks = modify sortByPriority

-- Run a todo app with the initial state and actions
runTodoApp :: TodoListState s a -> TodoList -> (a, TodoList)
runTodoApp = runState