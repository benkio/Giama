module UI.Shell.CreateElementRoute (createElementRoute) where

import           Controller.Router (createActRoute, createProjectRoute,
                                    createSceneRoute)
import           UI.Shell.Patterns (menu, wrongSelection)


createElementRoute :: IO ()
createElementRoute = menu createMenu selectionRoutes

createMenu :: String
createMenu = " --------------------- Create Menu --------------------- \n \
             \-                                                     - \n \
             \-       Select an Option                              - \n \
             \-                                                     - \n \
             \- p   : Create a new Project                          - \n \
             \- s   : Create a new Scene                            - \n \
             \- a   : Create a new Act                              - \n \
             \- b   : back to homepage                              - \n \
             \-                                                     - \n \
             \------------------------------------------------------- \n "

selectionRoutes :: String -> IO ()
selectionRoutes selection
 | "p" == selection = createProjectRoute >> createElementRoute
 | "s" == selection = createSceneRoute   >> createElementRoute
 | "a" == selection = createActRoute     >> createElementRoute
 | "b" == selection = return ()
 | otherwise = wrongSelection selection >> createElementRoute
