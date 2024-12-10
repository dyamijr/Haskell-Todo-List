module TodoUI
  ( printTask,
    printTodoList,
    printWelcomeMessage,
    printInvalidCommand,
    printExitMessage,
    printCommands,
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
printTodoList [] = putStrLn "List is empty\n"
printTodoList todoList = do
  mapM_ (uncurry printTask) (zip [0 ..] todoList)
  putStrLn ""

-- Print welcome message
printWelcomeMessage :: IO ()
printWelcomeMessage = putStrLn "Welcome to the Todo List CLI!\n"

-- Print invalid command message
printInvalidCommand :: IO ()
printInvalidCommand = putStrLn "Invalid command, please try again.\n"

-- should be adding some more like add a category to a task, sorting, print, print based on category etc.
-- Print all possible commands to the user
printCommands :: IO ()
printCommands = do
  putStrLn "Commands:"
  putStrLn " add [description] [priority] [date?]"
  putStrLn "   priority = high | medium | low"
  putStrLn "   date = MM/DD/YY"
  putStrLn " remove [index]"
  putStrLn " edit [index] [element to edit] [description?] [priority?] [date?]"
  putStrLn " complete [index]"
  putStrLn " save [filePath?]"
  putStrLn " load [filePath?]"
  putStrLn " list [option?]"
  putStrLn "   option = completed | high priority | medium priority | low priority | <category>"
  putStrLn " sort [option?]"
  putStrLn "   option = priority | date | desc"
  putStrLn " commands"
  putStrLn " quit\n"

printPrompt :: IO ()
printPrompt = putStrLn "Enter command: "

-- Print exit message
printExitMessage :: IO ()
printExitMessage = putStrLn "Exiting...\n"
