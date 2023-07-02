
module Main where

import Control.Monad.Trans.Writer
import Control.Monad.IO.Class (liftIO)
import System.IO

-- Data type to represent a task
data Task = Task
  { taskId :: Int,
    taskName :: String,
    taskCompleted :: Bool
  }
  deriving (Show)

-- Data type to represent the application state
data AppState = AppState
  { taskList :: [Task],
    nextId :: Int
  }

-- Monad transformer stack using WriterT to accumulate log messages
type App = WriterT [String] IO

-- Run the application
runApp :: App ()
runApp = do
  liftIO $ putStrLn "Welcome to the To-Do List Manager!"
  runAppLoop initialState

-- Initial application state
initialState :: AppState
initialState = AppState [] 1

-- Application loop
runAppLoop :: AppState -> App ()
runAppLoop state = do
  printMenu
  choice <- liftIO getLine
  case choice of
    "1" -> do
      liftIO $ putStrLn "Enter the task name:"
      taskName <- liftIO getLine
      let newTask = Task (nextId state) taskName False
      let newState = state {taskList = taskList state ++ [newTask], nextId = nextId state + 1}
      logMessage $ "Added task: " ++ taskName
      runAppLoop newState
    "2" -> do
      liftIO $ putStrLn "Enter the task ID to mark as complete:"
      taskIdStr <- liftIO getLine
      let taskId' = read taskIdStr
      let updatedTasks = map (markTaskComplete taskId') (taskList state)
      let newState = state {taskList = updatedTasks}
      logMessage $ "Marked task " ++ taskIdStr ++ " as complete"
      runAppLoop newState
    "3" -> do
      liftIO $ putStrLn "All tasks:"
      mapM_ (liftIO . print) (taskList state)
      runAppLoop state
    "4" -> do
      liftIO $ putStrLn "Enter the task ID to remove:"
      taskIdStr <- liftIO getLine
      let taskId' = read taskIdStr
      let updatedTasks = filter (\task -> taskId' /= taskId task) (taskList state)
      let newState = state {taskList = updatedTasks}
      logMessage $ "Removed task: " ++ taskIdStr
      runAppLoop newState
    "5" -> do
      liftIO $ putStrLn "Downloading all tasks..."
      liftIO $ writeFile "tasks.txt" (show (taskList state))
      logMessage "Downloaded all tasks"
      runAppLoop state
    "6" -> do
      liftIO $ putStrLn "Accumulated log file:"
      logFileContents <- liftIO $ readFile "log.txt"
      liftIO $ putStrLn logFileContents
      runAppLoop state
    "7" -> do
      liftIO $ putStrLn "Exiting the application. Goodbye!"
      logMessage "Application exited"
    _ -> do
      liftIO $ putStrLn "Invalid choice. Please try again."
      runAppLoop state

-- Print the main menu
printMenu :: App ()
printMenu = do
  liftIO $ putStrLn ""
  liftIO $ putStrLn "Main Menu:"
  liftIO $ putStrLn "1. Add task"
  liftIO $ putStrLn "2. Mark task as complete"
  liftIO $ putStrLn "3. List all tasks"
  liftIO $ putStrLn "4. Remove task"
  liftIO $ putStrLn "5. Download all tasks"
  liftIO $ putStrLn "6. View accumulated log file"
  liftIO $ putStrLn "7. Exit"
  liftIO $ putStrLn "Enter your choice:"

-- Mark a task as complete
markTaskComplete :: Int -> Task -> Task
markTaskComplete taskId' task =
  if taskId' == taskId task
    then task {taskCompleted = True}
    else task

-- Log a message using WriterT
logMessage :: String -> App ()
logMessage message = tell [message] >> liftIO (appendFile "log.txt" (message ++ "\n"))

-- Main function to run the application
main :: IO ()
main = runWriterT runApp >> return ()
