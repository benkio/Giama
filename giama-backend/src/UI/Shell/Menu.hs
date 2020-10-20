module UI.Shell.Menu (homepageMenu) where

import           UI.Shell.CreateElementRoute (createElementRoute)
import           UI.Shell.Patterns           (menu, wrongSelection)
import           UI.Shell.RemoveElementRoute (removeElementRoute)
import           UI.Shell.SearchByNameRoute  (searchByNameRoute)
import           UI.Shell.ShowProjectRoute   (showProjectByModifiedDateRoute,
                                              showProjectRoute)
homepageMenu :: IO ()
homepageMenu = menu homepage selectionRoutes

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

selectionRoutes :: String -> IO ()
selectionRoutes selection
 | "c"   == selection = createElementRoute             >> homepageMenu
 | "sh"  == selection = showProjectRoute               >> homepageMenu
 | "shmd"== selection = showProjectByModifiedDateRoute >> homepageMenu
 | "r"   == selection = removeElementRoute             >> homepageMenu
 | "srn" == selection = searchByNameRoute              >> homepageMenu
 | "q"   == selection = putStrLn "Goodbye! :) "
 | otherwise = wrongSelection selection >> homepageMenu
