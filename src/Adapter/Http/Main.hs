module Adapter.Http.Main where

import qualified Adapter.Http.Api.Auth       as AuthApi
import           Adapter.Http.Common
import           ClassyPrelude               hiding (delete)
import           Data.Aeson                  hiding (json, (.:))
import           Domain.Auth
import           Katip
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Middleware.Gzip
import           Prelude.Unicode             ((∘), (∧), (≡), (⊥))
import           Text.Digestive.Form         ((.:))
import qualified Text.Digestive.Form         as DF
import           Web.Scotty.Trans

type HauthApp m = (MonadIO m, KatipContext m, AuthRepo m, EmailVerificationNotif m, SessionRepo m)

main ∷ HauthApp m ⇒ Int → (m Response → IO Response) → IO ()
main port runner = scottyT port runner routes


routes ∷ HauthApp m ⇒ ScottyT LText m ()
routes = do
  middleware $ gzip $ def {gzipFiles = GzipCompress}
  AuthApi.routes
  defaultHandler $ \e → do
    lift $ $(logTM) ErrorS $ mkmsg e
    status status500
    json _ise
  where
    mkmsg err = "Unhandled error: " <> ls (showError err)

_ise ∷ Text
_ise = "InternalServerError"


