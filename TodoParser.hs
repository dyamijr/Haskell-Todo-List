module TodoParser where

import Data.Time (Day)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Text.Read (readMaybe)
import TodoState
import TodoTask

-- Define the Command type for different actions
data Command
  = AddTask String Priority (Maybe Day) -- Add a task with description, priority, and optional due date
  | RemoveTask Int -- Remove task by index
  | CompleteTask Int -- Mark task as complete by index
  | ListTasks -- List all tasks
  | Quit -- Quit the app
  deriving (Show, Eq)

-- Parse priority from a string
parsePriority :: String -> Maybe Priority
parsePriority "high" = Just High
parsePriority "medium" = Just Medium
parsePriority "low" = Just Low
parsePriority _ = Nothing

-- Parse date from a string in YYYY-MM-DD format
parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

-- Parse the command from a user input string
parseCommand :: String -> Maybe Command
parseCommand input =
  case words input of
    ["add", desc, prio, dateStr] ->
      case parsePriority prio of
        Just p -> Just (AddTask desc p (parseDate dateStr))
        Nothing -> Nothing
    ["add", desc, prio] ->
      case parsePriority prio of
        Just p -> Just (AddTask desc p Nothing)
        Nothing -> Nothing
    ["remove", taskId] ->
      case readMaybe taskId of
        Just id -> Just (RemoveTask id)
        Nothing -> Nothing
    ["complete", taskId] ->
      case readMaybe taskId of
        Just id -> Just (CompleteTask id)
        Nothing -> Nothing
    ["list"] -> Just ListTasks
    ["quit"] -> Just Quit
    _ -> Nothing
