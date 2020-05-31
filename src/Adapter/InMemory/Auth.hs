module Adapter.InMemory.Auth where

import ClassyPrelude
import qualified Domain.Auth as D
import Data.Has
import Text.StringRandom
import Control.Monad.Except

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


-- | add Auth, need to check if the email already is in use.
-- if it isn't then generate a vcode, add it to stateUnverifiedEmails
-- generate a user id, map the userId to the auth through stateAuths
-- If the email is in use then throw a RegistrationError
addAuth :: InMemory r m =>  D.Auth -> m (Either D.RegistrationError (D.UserId, D.VerificationCode))
addAuth auth = do
  tvar <- asks getter
  vcode <- liftIO $ stringRandomIO frmt
  atomically . runExceptT $ do
    s <- lift $ readTVar tvar
    let duplicated = isEmailExists s auth
    when duplicated $ throwError D.RegistrationErrorEmailTaken
    let newid = stateUserIdCounter s + 1
        newauths = (newid, auth) : (stateAuths s)
        unverifieds = insertMap vcode (D.authEmail auth) (stateUnverifiedEmails s)
        news = s {stateAuths = newauths, stateUserIdCounter = newid, stateUnverifiedEmails = unverifieds}
    lift $ writeTVar tvar news
    return (newid, vcode)

isEmailExists :: State -> D.Auth -> Bool
isEmailExists s a = let as = stateAuths s
                        email = D.authEmail a
                    in any (email ==) . map (D.authEmail . snd) $ as

setEmailAsVerified :: InMemory r m =>  D.VerificationCode -> m (Either D.EmailVerfificationError ())
setEmailAsVerified vcode = do
  tvar <- asks getter
  atomically . runExceptT $ do
    s <- lift $ readTVar tvar
    let unverifieds = stateUnverifiedEmails s
        verifieds = stateVerifiedEmails s
        memail = lookup vcode unverifieds
    case memail of
      Nothing -> throwError D.EmailVerfificationErrorInvalidCode
      Just email -> do
        let newUnvs = deleteMap vcode unverifieds
            newVs = insertSet email verifieds
            newstate = s {stateUnverifiedEmails = newUnvs, stateVerifiedEmails = newVs}
        lift $ writeTVar tvar newstate

findUserByAuth :: InMemory r m =>  D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth auth = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  let uid = map fst . find ((auth ==).snd) $ stateAuths state
  return $ uid >>= (\u -> Just (u, elem (D.authEmail auth) (stateVerifiedEmails state) ))


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
