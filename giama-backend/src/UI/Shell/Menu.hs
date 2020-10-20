module UI.Shell.Menu (menu) where

import           Data.List                   (find)
import           UI.Shell.CreateElementRoute (createElementRoute)
import           UI.Shell.RemoveElementRoute (removeElementRoute)
import           UI.Shell.SearchByNameRoute  (searchByNameRoute)
import           UI.Shell.ShowProjectRoute   (showProjectByModifiedDateRoute,
                                              showProjectRoute)

menu :: IO ()
menu = do
  selection <- init
  validateSelection selection selectionRoutes init
  where
    init :: IO String
    init = do
      putStr homepage
      putStr "> "
      getLine

homepage :: String
homepage = " ---------------------- Home Page ---------------------- \n \
           \-                                                     - \n \
           \-       Select an Option                              - \n \
           \-                                                     - \n \
           \- c   : Create a new Project/Scene/Act                - \n \
           \- sh  : Show projects                                 - \n \
           \- shmd: Show projects by modified date                - \n \
           \- r   : Remove a Project/Scene/Act                    - \n \
           \- srn : Search by name                                - \n \
           \- q   : exit                                          - \n \
           \-                                                     - \n \
           \------------------------------------------------------- \n "

selectionRoutes :: [(String, IO ())]
selectionRoutes = [
  ("c"    , createElementRoute             >> menu )
 ,("sh"   , showProjectRoute               >> menu )
 ,("shmd" , showProjectByModifiedDateRoute >> menu )
 ,("r"    , removeElementRoute             >> menu )
 ,("srn"  , searchByNameRoute              >> menu )
 ,("q"    , putStrLn "Goodbye! :) ")]

validateSelection :: String -> [(String, IO ())] -> IO String ->  IO ()
validateSelection selection validOptions fallback = do
  let
    maybeOperation :: Maybe (String, IO ())
    maybeOperation = find ((selection ==) . fst) validOptions
  foldl (\_ op -> snd op) (
    do
      putStrLn $ "ERROR: " ++ selection ++ " is no a valid option"
      newSelection <- fallback
      validateSelection newSelection validOptions fallback) maybeOperation
