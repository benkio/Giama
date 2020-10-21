module UI.Shell.ShowProjectRoute (showProjectsRoute, showProjectsByModifiedDateRoute) where

import qualified Controller.Router as S (showProjectsByModifiedDateRoute,
                                         showProjectsRoute)

showProjectsRoute :: IO ()
showProjectsRoute = S.showProjectsRoute

showProjectsByModifiedDateRoute :: IO ()
showProjectsByModifiedDateRoute = S.showProjectsByModifiedDateRoute
