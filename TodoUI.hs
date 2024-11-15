module TodoUI (
    printTask,
    printTodoList,
    printWelcomeMessage,
    printInvalidCommand,
    printExitMessage,
    printPrompt
) where
import TodoTask
import Data.List

-- Print a single task
printTask :: Int -> Task -> IO ()
printTask idx (Task desc completed prio due categories) = 
    putStrLn $ show idx ++ ". [" ++ (if completed then "x" else " ") ++ "] " ++ desc
            ++ " (" ++ show prio ++ ")" ++ 
            (if null categories
                then ""
                else if length categories == 1
                        then " Category: " 
                        else  " Categories: ")
            ++ intercalate ", " categories
-- Print the entire todo list
printTodoList :: TodoList -> IO ()
printTodoList todoList = mapM_ (uncurry printTask) (zip [0..] todoList)

-- Print welcome message
printWelcomeMessage :: IO ()
printWelcomeMessage = putStrLn "Welcome to the Todo List CLI!"

-- Print invalid command message
printInvalidCommand :: IO ()
printInvalidCommand = putStrLn "Invalid command, please try again."

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
