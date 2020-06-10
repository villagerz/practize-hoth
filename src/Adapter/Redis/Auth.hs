module Adapter.Redis.Auth where

import           ClassyPrelude
import           Control.Monad.Catch  (MonadThrow)
-- ^ need explicit for LTS > 11.0 which I used, since ClassyPrelude no longer exports it
import           Control.Monad.Except
import           Data.Has
import qualified Database.Redis       as R
import qualified Domain.Auth          as D
import           Prelude.Unicode      ((∘), (∧), (≡))
import           Text.StringRandom

type State = R.Connection

withState ∷ String → (State → IO a) → IO a
withState url action = do
  case R.parseConnectInfo url of
    Left _ → throwString "Invalid url "
    Right connInfo → do
      conn ← R.checkedConnect connInfo
      action conn

type Redis r m = (Has State r, MonadReader r m, MonadIO m, MonadThrow m)

withConn ∷ Redis r m ⇒ R.Redis a → m a
withConn action = do
  conn ← asks getter
  liftIO $ R.runRedis conn action

newSession ∷ Redis r m ⇒ D.UserId → m D.SessionId
newSession userid = do
  sid ← liftIO $ stringRandomIO "[a-zA-Z0-9]{32}"
  result ← withConn $ R.set (encodeUtf8 sid) (fromString ∘ show $ userid)
  case result of
    Right R.Ok → return sid
    err        → throwString $ "Unexpected redis error: " <> show err

findUserBySession :: Redis r m ⇒ D.SessionId -> m (Maybe D.UserId)
findUserBySession sid = do
  result ← withConn $ R.get (encodeUtf8 sid)
  case result of
    Right (Just uidStr) → return $ readMay ∘ unpack ∘ decodeUtf8 $ uidStr
    err                 → throwString $ "Unexpected redis error: " <> show err
