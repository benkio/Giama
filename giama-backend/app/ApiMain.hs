module ApiMain where

import           Network.Wai
import           Network.Wai.Handler.Warp
import           UI.Api.App               (app)

main :: IO ()
main = run 8080 app
