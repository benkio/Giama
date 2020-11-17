module Domain.Identifiers (ProjectName, SceneName, ActName, Identifier, projectNameId, sceneNameId, actNameId) where

import           Domain.HasName                    (HasName (..))

data Identifier = ProjectName
                 | SceneName
                 | ActName

data ProjectName = MkProjectName String
data SceneName   = MkSceneName String String
data ActName     = MkActName String String String

projectNameId :: String -> ProjectName
projectNameId = MkProjectName

sceneNameId :: ProjectName -> String -> SceneName
sceneNameId (MkProjectName prjn) scnn = MkSceneName prjn scnn

actNameId :: SceneName -> String -> ActName
actNameId (MkSceneName prjn srnn) actn = MkActName prjn srnn actn

-- viewPattern -----------------------

data ViewIdentifier =   ProjectNameView String 
                      | SceneNameView   String String 
                      | ActNameView     String String String

view :: Identifier -> ViewIdentifier
view (MkProjectName prjn           )    = ProjectNameView prjn           
view (MkSceneName   prjn scnn      )    = SceneNameView   prjn scnn     
view (MkActName     prjn scnn actn   )  = ActNameView     prjn scnn actn

-- instances -------------------------

instance Show ProjectName where
  show (MkProjectName prjn) = prjn

instance Show SceneName where
  show (MkSceneName prjn scnn) = "  |- (" ++ prjn ++ ") - " ++ scnn ++ " "

instance Show ActName where
  show (MkActName prjn scnn actn) = "       |- (" ++ prjn ++ " - " ++ scnn ++ ") - " ++ actn ++ " "

instance HasName ProjectName where
  getName (MkProjectName prjn) = prjn

instance HasName SceneName where
  getName (MkSceneName _ scnn) = scnn

instance HasName ActName where
  getName (MkActName _ _ actn) = actn
