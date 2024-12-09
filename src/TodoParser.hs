module TodoParser where

import Data.Time (Day)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import System.FilePath (isValid)
import Text.Read (readMaybe)
import TodoState
import TodoTask

-- Define the Command type for different actions
data Command
  = AddTask String Priority (Maybe Day) -- Add a task with description, priority, and optional due date
  | RemoveTask Int -- Remove task by index
  | CompleteTask Int -- Mark task as complete by index
  | SaveList (Maybe String) -- Save a list in the file system
  | LoadList (Maybe String) -- Loads a list from the file system
  | SortList (Maybe String) -- Sorts the list
  | ShowCommands -- Shows list of all possible commands
  | ListTasks (Maybe String) -- List all tasks with possible options
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
parseDate = parseTimeM True defaultTimeLocale "%m/%d/%Y"

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
    ["save", filePath] ->
      if isValid filePath then Just (SaveList (Just filePath)) else Nothing
    ["save"] -> Just (SaveList Nothing)
    ["load", filePath] ->
      if isValid filePath then Just (LoadList (Just filePath)) else Nothing
    ["load"] -> Just (LoadList Nothing)
    ["list"] -> Just (ListTasks Nothing)
    "list" : option -> Just (ListTasks (Just (unwords option)))
    ["sort", option] -> Just (SortList (Just option))
    ["sort"] -> Just (SortList Nothing)
    ["commands"] -> Just ShowCommands
    ["quit"] -> Just Quit
    ["exit"] -> Just Quit
    _ -> Nothing
