module TodoTaskSpec where

import Test.HUnit
import TodoTask
import TodoParser (parseDate)

-- Sample data for testing
sampleTask :: Task
sampleTask =
  Task {description = "Task 1", completed = False, priority = High, dueDate = Nothing, categories = []}

sampleTasks :: TodoList
sampleTasks =
  [ Task {description = "Task 1", completed = False, priority = High, dueDate = Nothing, categories = []},
    Task {description = "Task 2", completed = True, priority = Low, dueDate = Nothing, categories = []},
    Task {description = "Task 3", completed = False, priority = Medium, dueDate = Nothing, categories = []},
    Task {description = "Task 4", completed = True, priority = Low, dueDate = Nothing, categories = []}]

-- Test: Creating a task
testCreateTask :: Test
testCreateTask = "Create Task" ~: do
    let task = createTask "Task 1" High Nothing
    assertEqual "New task should be created"
        sampleTask task 

-- Test: Creating a task with a date
testCreateTaskWithDate :: Test
testCreateTaskWithDate = "Create Task with date" ~: do
    let task = createTask "Task 1" High (parseDate "12/01/2024")
    assertEqual "New task should be created"
        sampleTask { dueDate = parseDate "12/01/2024" }
        task

-- Test: Adding a category to a task
testCategorizeTask :: Test
testCategorizeTask = "Categorize Task" ~: do
    let task = categorizeTask sampleTask "category"
    assertEqual "Task should have a category"
        sampleTask { categories = ["category"] }
        task

-- Test: Completing a task
testCompleteTask :: Test
testCompleteTask = "Complete Task" ~: do
  let updatedTask = completeTask sampleTask
  assertEqual "Task should be marked as complete"
    sampleTask { completed = True }
    updatedTask

-- Test: Removing task
testRemovingTask :: Test
testRemovingTask = "Remove Task" ~: do
  let removed = removeTaskByIndex 2 sampleTasks
  assertEqual "Should be list with index two removed"
    (take 2 sampleTasks ++ drop 3 sampleTasks)
    removed

-- Test: Removing out of bound task
testRemovingTaskInvalid :: Test
testRemovingTaskInvalid = "Remove Out of Bounds Task" ~: do
  let removed = removeTaskByIndex 5 sampleTasks
  assertEqual "Should be the same list"
    sampleTasks
    removed

-- Test: Sorting tasks by priority
testSortByPriority :: Test
testSortByPriority = "Sort By Priority" ~: do
  let sorted = sortByPriority sampleTasks
  assertEqual "Tasks should be sorted by priority (High > Medium > Low)"
    [ Task {description = "Task 1", completed = False, priority = High, dueDate = Nothing, categories = []},
    Task {description = "Task 3", completed = False, priority = Medium, dueDate = Nothing, categories = []},
    Task {description = "Task 2", completed = True, priority = Low, dueDate = Nothing, categories = []},
    Task {description = "Task 4", completed = True, priority = Low, dueDate = Nothing, categories = []}]
    sorted

-- Combine all tests
todoTaskTests :: Test
todoTaskTests = TestList
  [ testCreateTask,
    testCreateTaskWithDate,
    testCategorizeTask,
    testCompleteTask,
    testRemovingTask,
    testRemovingTaskInvalid,
    testSortByPriority
  ]