module UI.Shell.SearchByNameRoute (searchByNameRoute) where

import qualified Controller.Router as R (searchByNameRoute)

searchByNameRoute :: IO ()
searchByNameRoute = R.searchByNameRoute
