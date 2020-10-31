module UI.Shell.MoveElementRoute (moveElementRoute) where

import           Controller.Router (moveActRoute, moveSceneRoute)
import           UI.Shell.Patterns (menu, wrongSelection)


moveElementRoute :: IO ()
moveElementRoute = menu moveMenu selectionRoutes

moveMenu :: String
moveMenu = " --------------------- Move Menu --------------------- \n \
             \-                                                     - \n \
             \-       Select an Option                              - \n \
             \-                                                     - \n \
             \- s   : move a new Scene                              - \n \
             \- a   : move a new Act                                - \n \
             \- b   : back to homepage                              - \n \
             \-                                                     - \n \
             \------------------------------------------------------- \n "

selectionRoutes :: String -> IO ()
selectionRoutes selection
 | "s" == selection = moveSceneRoute   >> moveElementRoute
 | "a" == selection = moveActRoute     >> moveElementRoute
 | "b" == selection = return ()
 | otherwise = wrongSelection selection >> moveElementRoute
