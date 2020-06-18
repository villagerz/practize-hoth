module Adapter.Http.Main where

import           ClassyPrelude    hiding (delete)
import           Web.Scotty.Trans

main ∷ IO ()
main = scottyT 3000 id routes

-- routes are processed in order till first hit
-- hence notFound at end
routes ∷ (MonadIO m) ⇒ ScottyT LText m ()
routes = do
  httpdo get "/" "home"
  httpdo get "/hello" "Hello!"
  get "/hello/:name" $ do
    name ← param "name"
    (text $ "Hello, " <> name) `rescue` \_ → text "you of no name"
  httpdo post "/users" "adding user"
  httpdo put "/users/:id" "updating user"
  httpdo patch "/users/:id" "partially updating users"
  httpdo delete "/users/:id" "deleting user"
  httpdo matchAny "/admin" "I don't care about your Http verb"
  httpdo options (regex ".*") "CORS usually use this"
  notFound $ text "404"

httpdo verb route text' = verb route $ text text'
