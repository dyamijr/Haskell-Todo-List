module TodoTask (
    Priority(..),
    Task(..),
    TodoList,
    createTask,
    categorizeTask,
    completeTask,
    removeTask,
    sortByPriority
) where 

import Data.Time (Day)
import Data.List
import Data.Ord

data Priority = High | Medium | Low deriving (Show, Eq, Ord)

data Task = Task {
    description :: String,
    completed   :: Bool,
    priority    :: Priority,
    dueDate     :: Maybe Day,
    categories  :: [String]
} deriving (Show, Eq)

type TodoList = [Task]

-- Function to create a new task
createTask :: String -> Priority -> Maybe Day -> Task
createTask desc prio due = Task desc False prio due []

-- Function to add a category tag to a task 
categorizeTask :: Task -> String-> Task
categorizeTask task category = task { categories = category:categories task }

-- Function to mark a task as complete
completeTask :: Task -> Task
completeTask task = task { completed = True }

-- Function to remove a task
removeTask :: Task -> TodoList -> TodoList
removeTask = delete

-- Function to sort the todo list by priority
sortByPriority :: TodoList -> TodoList
sortByPriority = sortBy (comparing priority)