module Domain.Identifiers (ProjectId
, SceneId
, ActId
, projectIdConstructor
, sceneIdConstructor
, actIdConstructor
, sceneIdPosition
, actIdPosition
, projectIdFromSceneId
, projectIdFromActId
, sceneIdFromActId
, sceneIdModifyPosition
, actIdModifyPosition
  , sceneIdModifyProject
  , actIdModifyProject
  , actIdModifyScene) where

import           Domain.HasName                    (HasName (..))

newtype ProjectId = MkProjectId String
data SceneId   = MkSceneId String (String, Int)
data ActId     = MkActId String (String, Int) (String, Int)

-- Constructors ----------------------------------------------

projectIdConstructor :: String -> ProjectId
projectIdConstructor = MkProjectId

sceneIdConstructor :: ProjectId -> String -> Int -> SceneId
sceneIdConstructor (MkProjectId prjn) scnn position = MkSceneId prjn (scnn, position)

actIdConstructor :: SceneId -> String -> Int -> ActId
actIdConstructor (MkSceneId prjn srnn) actn position = MkActId prjn srnn (actn, position)

-- Extractors ----------

projectIdFromSceneId :: SceneId -> ProjectId
projectIdFromSceneId (MkSceneId prjn _) = MkProjectId prjn

projectIdFromActId :: ActId -> ProjectId
projectIdFromActId (MkActId prjn _ _) = MkProjectId prjn

sceneIdFromActId :: ActId -> SceneId
sceneIdFromActId (MkActId prjn scnId _) = MkSceneId prjn scnId

sceneIdPosition :: SceneId -> Int
sceneIdPosition (MkSceneId _ (_, pos)) = pos

actIdPosition :: ActId -> Int
actIdPosition (MkActId _ _ (_, pos)) = pos

-- Modifiers ---------------------------

sceneIdModifyPosition :: (Int -> Int) -> SceneId -> SceneId
sceneIdModifyPosition f (MkSceneId prjn (scnn, pos)) =
  MkSceneId prjn (scnn, f pos)

sceneIdModifyProject :: ProjectId -> SceneId -> SceneId
sceneIdModifyProject (MkProjectId prjn) (MkSceneId _ scn) = MkSceneId prjn scn

actIdModifyPosition :: (Int -> Int) -> ActId -> ActId
actIdModifyPosition f (MkActId prjn scnId (an, pos)) =
  MkActId prjn scnId (an, f pos)

actIdModifyProject :: ProjectId -> ActId -> ActId
actIdModifyProject (MkProjectId prjn) (MkActId _ scn act) = MkActId prjn scn act

actIdModifyScene :: SceneId -> ActId -> ActId
actIdModifyScene (MkSceneId prjn scn) (MkActId _ _ act) = MkActId prjn scn act

-- instances -------------------------

instance Show ProjectId where
  show = getName

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
