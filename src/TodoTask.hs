{-# LANGUAGE DeriveGeneric #-}
module TodoTask (
    Priority(..),
    Task(..),
    TodoList,
    createTask,
    categorizeTask,
    completeTask,
    removeTaskByIndex,
    sortByPriority,
    sortByDate,
    sortByDesc
) where 

import Data.Time (Day)
import Data.List
import Data.Ord
import GHC.Generics (Generic)

data Priority = High | Medium | Low deriving (Show, Eq, Ord, Generic)

data Task = Task {
    description :: String,
    completed   :: Bool,
    priority    :: Priority,
    dueDate     :: Maybe Day,
    categories  :: [String]
} deriving (Show, Eq, Generic)

type TodoList = [Task]

-- Function to create a new task
createTask :: String -> Priority -> Maybe Day -> Task
createTask desc prio due = Task desc False prio due []

-- Function to add a category tag to a task 
categorizeTask :: Task -> String -> Task
categorizeTask task category = task { categories = category:categories task }

-- Function to mark a task as complete
completeTask :: Task -> Task
completeTask task = task { completed = True }

-- Function to remove a task
removeTaskByIndex :: Int -> TodoList -> TodoList
removeTaskByIndex idx list = take idx list ++ drop (idx + 1) list

-- Function to sort the todo list by priority
sortByPriority :: TodoList -> TodoList
sortByPriority = sortBy (comparing priority)

-- Function to sort the todo list by dueDate
sortByDate :: TodoList -> TodoList
sortByDate = sortBy compareTaskDueDate

-- Function to sort the todo list by description
sortByDesc :: TodoList -> TodoList
sortByDesc = sortBy (comparing description)

-- Custom comparison function for sorting by dueDate
compareTaskDueDate :: Task -> Task -> Ordering
compareTaskDueDate task1 task2 =
    -- Place Just values first, compare the dates inside the Just values
    case (dueDate task1, dueDate task2) of
        (Just _, Nothing) -> LT  -- task1 is Just, task2 is Nothing, so task1 comes first
        (Nothing, Just _) -> GT  -- task1 is Nothing, task2 is Just, so task2 comes first
        (Just date1, Just date2) -> compare date1 date2  -- Compare the dates
        (Nothing, Nothing) -> EQ  -- Both are Nothing, so they are equal