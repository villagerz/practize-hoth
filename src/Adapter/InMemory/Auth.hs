module Adapter.InMemory.Auth where

import ClassyPrelude
import qualified Domain.Auth as D
import Data.Has
import Text.StringRandom

type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)
data State = State
  { stateAuths :: [(D.UserId, D.Auth)]
  , stateUnverifiedEmails :: Map D.VerificationCode D.Email
  , stateVerifiedEmails :: Set D.Email
  , stateUserIdCounter :: Int
  , stateNotifications :: Map D.Email D.VerificationCode
  , stateSessions :: Map D.SessionId D.UserId
  } deriving (Show, Eq)

initialState = State
  { stateAuths = []
  , stateUnverifiedEmails = mempty
  , stateVerifiedEmails = mempty
  , stateUserIdCounter = 0
  , stateNotifications = mempty
  , stateSessions = mempty
  }

frmt = "[A-Za-z0-9]{16}"



addAuth :: InMemory r m =>  D.Auth -> m (Either D.RegistrationError D.VerificationCode)
addAuth = error "TODO"

setEmailAsVerified :: InMemory r m =>  D.VerificationCode -> m (Either D.EmailVerfificationError ())
setEmailAsVerified = error "TODO"

findUserByAuth :: InMemory r m =>  D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth auth = error "TODO"


findEmailFromUser :: InMemory r m =>  D.UserId -> m (Maybe D.Email)
findEmailFromUser uid = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  let auths = map snd . find ((uid ==) . fst) $ stateAuths state
  return $ D.authEmail <$> auths

notifyEmailVerification :: InMemory r m =>  D.Email -> D.VerificationCode -> m ()
notifyEmailVerification email vcode = do
  tvar <- asks getter
  atomically $ do
    state <- readTVar tvar
    writeTVar tvar state {stateNotifications = (insertMap email vcode (stateNotifications state))}

getNotificationsForEmail :: InMemory r m => D.Email -> m (Maybe D.VerificationCode)
getNotificationsForEmail email = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  return $ lookup email $ stateNotifications state

newSession :: InMemory r m =>  D.UserId -> m D.SessionId
newSession uId = do
  tvar <- asks getter
  sId <- liftIO $ sessionid uId
  atomically $ do
    state <- readTVar tvar
    writeTVar tvar (insertSession state sId uId)
    return sId

findUserBySession :: InMemory r m =>  D.SessionId -> m (Maybe D.UserId)
findUserBySession sId = do
  tvar <- asks getter
  liftIO $ lookup sId . stateSessions <$> readTVarIO tvar

sessionid :: D.UserId -> IO D.SessionId
sessionid u = ((tshow u) <>) <$> stringRandomIO frmt

stateWithMap :: (Ord k) => (State -> Map k v) -> k -> v -> (Map k v -> State) -> State -> State
stateWithMap stm k v mts  = mts . insertMap k v . stm  

insertSession :: State -> D.SessionId -> D.UserId -> State
insertSession s sid uid = let oldsessions = stateSessions s
                          in  s {stateSessions = insertMap sid uid oldsessions}
