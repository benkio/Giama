module UI.Shell.RemoveElementRoute (removeElementRoute) where

import           Controller.Router (removeActRoute, removeProjectRoute,
                                    removeSceneRoute)
import           UI.Shell.Patterns (menu, wrongSelection)


removeElementRoute :: IO ()
removeElementRoute = menu removeMenu selectionRoutes

removeMenu :: String
removeMenu = " --------------------- Remove Menu --------------------- \n \
             \-                                                     - \n \
             \-       Select an Option                              - \n \
             \-                                                     - \n \
             \- p   : Remove a new Project                          - \n \
             \- s   : Remove a new Scene                            - \n \
             \- a   : Remove a new Act                              - \n \
             \- b   : back to homepage                              - \n \
             \-                                                     - \n \
             \------------------------------------------------------- \n "

selectionRoutes :: String -> IO ()
selectionRoutes selection
 | "p" == selection = removeProjectRoute >> removeElementRoute
 | "s" == selection = removeSceneRoute   >> removeElementRoute
 | "a" == selection = removeActRoute     >> removeElementRoute
 | "b" == selection = return ()
 | otherwise = wrongSelection selection >> removeElementRoute
