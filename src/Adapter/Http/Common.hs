module Adapter.Http.Common where

import           Blaze.ByteString.Builder  (toLazyByteString)
import           ClassyPrelude
import           Data.Aeson                hiding (json)
import           Data.Time.Lens
import           Domain.Auth
import           Network.HTTP.Types.Status
import           Prelude.Unicode           ((∘), (∧), (≡))

import qualified Text.Digestive.Aeson      as DF
import qualified Text.Digestive.Form       as DF
import qualified Text.Digestive.Types      as DF
import           Web.Cookie
import           Web.Scotty.Trans

parseAuthJson ∷ (ScottyError e, MonadIO m, ToJSON v) ⇒
  DF.Form v m a → ActionT e m a
parseAuthJson form = do
  val ← jsonData `rescue` (\_ → return Null)
  result ← lift $ DF.digestJSON form val
  case result of
    (v, Nothing) → do
      status status400
      json $ DF.jsonErrors v
      finish
    (_, Just val) → return val

setSessionIdInCookie ∷ (MonadIO m, ScottyError e) ⇒ SessionId → ActionT e m ()
setSessionIdInCookie sid = do
  fromnow ← liftIO getCurrentTime
  setCookie $ def { setCookieName = encodeUtf8 _sidName
                  , setCookiePath = Just "/"
                  , setCookieValue = encodeUtf8 sid
                  , setCookieExpires = Just $ modL month (+ 1) fromnow
                  , setCookieHttpOnly = True
                  , setCookieSecure = False
                  , setCookieSameSite = Just sameSiteLax
                  }

reqCurrentUserId ∷ (SessionRepo m, ScottyError e) ⇒ ActionT e m UserId
reqCurrentUserId = do
  maybeUser ← getUserId
  case maybeUser of
    Nothing → do
      status status401
      json ("AuthRequired" ∷ Text)
      finish
    Just userId → return userId

getUserId ∷ (SessionRepo m, ScottyError e) ⇒ ActionT e m (Maybe UserId)
getUserId = do
  maybeSid ← getCookie _sidName
  case maybeSid of
    Nothing  → return Nothing
    Just sId → lift $ resolveSessionId sId

toResult ∷ Either e a → DF.Result e a
toResult = either DF.Error DF.Success

setCookie ∷ (ScottyError e, Monad m) ⇒ SetCookie → ActionT e m ()
setCookie = setHeader "Set-Cookie" ∘ decodeUtf8 ∘ toLazyByteString ∘ renderSetCookie

getCookie ∷ (ScottyError e, Monad m) ⇒ Text → ActionT e m (Maybe Text)
getCookie key = do
  cookies ← header "Cookie"
  return $ do
    cookie ← parseCookies ∘ encodeUtf8 ∘ toStrict <$> cookies
    let bskey = encodeUtf8 key
    val ← lookup bskey cookie
    return $ decodeUtf8 val

_sidName = "sId"
