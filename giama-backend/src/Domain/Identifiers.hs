module Domain.Identifiers (ProjectId(..), SceneId(..), ActId(..), projectIdConstructor, sceneIdConstructor, actIdConstructor) where

import           Domain.HasName                    (HasName (..))

data ProjectId = MkProjectId String
data SceneId   = MkSceneId String (String, Int)
data ActId     = MkActId String (String, Int) (String, Int)

projectIdConstructor :: String -> ProjectId
projectIdConstructor = MkProjectId

sceneIdConstructor :: ProjectId -> String -> Int -> SceneId
sceneIdConstructor (MkProjectId prjn) scnn position = MkSceneId prjn (scnn, position)

actIdConstructor :: SceneId -> String -> Int -> ActId
actIdConstructor (MkSceneId prjn srnn) actn position = MkActId prjn srnn (actn, position)

-- instances -------------------------

instance Show ProjectId where
  show (MkProjectId prjn) = prjn

instance Show SceneId where
  show (MkSceneId prjn (scnn, scnp)) = "  |- (" ++ prjn ++ ") - " ++ scnn ++ " " ++ show scnp

instance Show ActId where
  show (MkActId prjn (scnn, _) (actn, position)) = "       |- (" ++ prjn ++ " - " ++ scnn ++ ") - " ++ actn ++ " " ++ show position

instance HasName ProjectId where
  getName (MkProjectId prjn) = prjn

instance HasName SceneId where
  getName (MkSceneId _ (scnn, _)) = scnn

instance HasName ActId where
  getName (MkActId _ _ (actn, _)) = actn
