module UI.Shell.Menu (homepageMenu) where

import           UI.Shell.CreateElementRoute (createElementRoute)
import           UI.Shell.MoveElementRoute   (moveElementRoute)
import           UI.Shell.Patterns           (menu, wrongSelection)
import           UI.Shell.RemoveElementRoute (removeElementRoute)
import           UI.Shell.SearchByNameRoute  (searchByNameRoute)
import           UI.Shell.ShowProjectRoute   (showProjectsByModifiedDateRoute,
                                              showProjectsRoute)
homepageMenu :: IO ()
homepageMenu = menu homepage selectionRoutes

homepage :: String
homepage = " ---------------------- Home Page ---------------------- \n \
           \-                                                     - \n \
           \-       Select an Option                              - \n \
           \-                                                     - \n \
           \- sh  : Show projects                                 - \n \
           \- shmd: Show projects by modified date                - \n \
           \- c   : Create a new Project/Scene/Act                - \n \
           \- r   : Remove a Project/Scene/Act                    - \n \
           \- m   : Move a Scene/Act                              - \n \
           \- srn : Search by name                                - \n \
           \- q   : exit                                          - \n \
           \-                                                     - \n \
           \------------------------------------------------------- \n "

selectionRoutes :: String -> IO ()
selectionRoutes selection
 | "sh"  == selection = showProjectsRoute               >> homepageMenu
 | "shmd"== selection = showProjectsByModifiedDateRoute >> homepageMenu
 | "c"   == selection = createElementRoute              >> homepageMenu
 | "r"   == selection = removeElementRoute              >> homepageMenu
 | "m"   == selection = moveElementRoute                >> homepageMenu
 | "srn" == selection = searchByNameRoute               >> homepageMenu
 | "q"   == selection = putStrLn "Goodbye! :) "
 | otherwise = wrongSelection selection >> homepageMenu
