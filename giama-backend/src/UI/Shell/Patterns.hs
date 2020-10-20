module UI.Shell.Patterns (menu, wrongSelection) where

import           Data.List  (find)
import           Data.Maybe (fromJust)

menu :: String -> (String -> IO ()) -> IO ()
menu message selectionRoutes = do
  selection <- showMenuMessage message
  selectionRoutes selection

showMenuMessage :: String -> IO String
showMenuMessage message = do
  putStrLn message
  getLine

wrongSelection :: String -> IO ()
wrongSelection selection = putStrLn $ "ERROR: " ++ selection ++ " is no a valid option"
