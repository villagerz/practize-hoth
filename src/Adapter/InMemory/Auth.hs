module Adapter.InMemory.Auth where

import ClassyPrelude
import qualified Domain.Auth as D
import Data.Has

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



addAuth :: InMemory r m =>  D.Auth -> m (Either D.RegistrationError D.VerificationCode)
addAuth = error "TODO"

setEmailAsVerified :: InMemory r m =>  D.VerificationCode -> m (Either D.EmailVerfificationError ())
setEmailAsVerified = error "TODO"

findUserByAuth :: InMemory r m =>  D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth = error "TODO"

findEmailFromUser :: InMemory r m =>  D.UserId -> m (Maybe D.Email)
findEmailFromUser = error "TODO"

notifyEmailVerification :: InMemory r m =>  D.Email -> D.VerificationCode -> m ()
notifyEmailVerification = error "TODO"

newSession :: InMemory r m =>  D.UserId -> m D.SessionId
newSession = error "TODO"

findUserBySession :: InMemory r m =>  D.SessionId -> m (Maybe D.UserId)
findUserBySession = error "TODO"
