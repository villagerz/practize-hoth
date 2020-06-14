module Adapter.Http.Main where

import           ClassyPrelude    hiding (delete)
import           Web.Scotty.Trans

main ∷ IO ()
main = scottyT 3000 id routes

routes ∷ (MonadIO m) ⇒ ScottyT LText m ()
routes = get "/hello" $ text "Hello!"
