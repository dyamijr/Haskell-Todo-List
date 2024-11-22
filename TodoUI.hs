module TodoUI
  ( printTask,
    printTodoList,
    printWelcomeMessage,
    printInvalidCommand,
    printExitMessage,
    printPrompt,
  )
where

import Data.List
import Data.Time
import TodoTask

-- Print a single task
printTask :: Int -> Task -> IO ()
printTask idx (Task desc completed prio due categories) =
  putStrLn $
    show idx 
      ++ completedText
      ++ categoryText
      ++ dueText
  where
    completedText =
      ". ["
        ++ (if completed then "x" else " ")
        ++ "] "
        ++ desc
        ++ " ("
        ++ show prio
        ++ ")"
    categoryText
      | null categories = ""
      | length categories == 1 = " Category: " ++ head categories
      | otherwise = " Categories: " ++ intercalate ", " categories
    dueText = case due of
      Nothing -> ""
      Just d -> formatTime defaultTimeLocale " %m/%d/%y" d

-- Print the entire todo list
printTodoList :: TodoList -> IO ()
printTodoList todoList = mapM_ (uncurry printTask) (zip [0 ..] todoList)

-- Print welcome message
printWelcomeMessage :: IO ()
printWelcomeMessage = putStrLn "Welcome to the Todo List CLI!"

-- Print invalid command message
printInvalidCommand :: IO ()
printInvalidCommand = putStrLn "Invalid command, please try again."

-- should be adding some more like add a category to a task, sorting, print, print based on category etc.
-- Print all possible commands to the user
printPrompt :: IO ()
printPrompt = do
  putStrLn "Commands:"
  putStrLn " add [description] [priority] [date?]"
  putStrLn " remove [index]"
  putStrLn " complete [index]"
  putStrLn " list"
  putStrLn " quit"
  putStrLn "Enter command: "

-- Print exit message
printExitMessage :: IO ()
printExitMessage = putStrLn "Exiting..."
