module Domain.Auth
  (
    -- * Types
    Auth(..)
  , Email
  , rawEmail
  , Password
  , rawPassword
  , UserId
  , VerificationCode
  , SessionId
  , RegistrationError(..)
  , EmailVerfificationError(..)
  , LoginError(..)
  ,  mkEmail
  , mkPassword
  -- * Ports
  , AuthRepo(..)
  , EmailVerificationNotif (..)
  , SessionRepo(..)
  -- * Use cases
  , register
  , verifyEmail
  , login
  , resolveSessionId
  , getUser
  )
where

import ClassyPrelude
import Domain.Validation
import Text.Regex.PCRE.Heavy
import Control.Monad.Except
import Katip


data Auth = Auth
  { authEmail :: Email
  , authPassword :: Password
  } deriving (Show, Eq)
type UserId = Int 
type SessionId = Text

data RegistrationError = RegistrationErrorEmailTaken deriving (Show, Eq)
data EmailVerfificationError = EmailVerfificationErrorInvalidCode deriving (Show, Eq)
data LoginError = LoginInvalidAuth | LoginEmailNotVerified deriving (Show, Eq)

newtype Email = Email { emailRaw :: Text } deriving (Show, Eq, Ord)
newtype Password = Password { passwordRaw :: Text } deriving (Show, Eq)

type VerificationCode = Text

class Monad m => AuthRepo m where
  addAuth :: Auth -> m (Either RegistrationError (UserId,VerificationCode))
  setEmailAsVerified :: VerificationCode -> m (Either EmailVerfificationError (UserId, Email))
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
  findEmailFromUser :: UserId -> m (Maybe Email)

class Monad m => SessionRepo m where
  newSession :: UserId -> m SessionId
  findUserBySession :: SessionId -> m (Maybe UserId)

class Monad m => EmailVerificationNotif m where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

login :: (KatipContext m, AuthRepo m, SessionRepo m) => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
  result <- lift $ findUserByAuth auth
  case result of
    Nothing -> throwError LoginInvalidAuth
    Just (_, False) -> throwError LoginEmailNotVerified
    Just (uid, _) -> withUserContext uid . lift $ do
      sid <- newSession uid
      $(logTM) InfoS $ ls (rawEmail $ authEmail auth) <> " logged in successfully."
      return sid

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserBySession

register :: (KatipContext m, AuthRepo m, EmailVerificationNotif m) => Auth -> m (Either RegistrationError ())
register auth = runExceptT $ do
  (uId, vCode) <- ExceptT $ addAuth auth
  let email = authEmail auth
  lift $ notifyEmailVerification email vCode
  withUserContext uId $ $(logTM) InfoS $ ls (rawEmail email) <> " is registered successfully."

verifyEmail :: (KatipContext m, AuthRepo m) => VerificationCode -> m (Either EmailVerfificationError ())
verifyEmail vc = runExceptT $ do
  (uid, email) <- ExceptT $ setEmailAsVerified vc
  withUserContext uid $ $(logTM) InfoS $ ls (rawEmail email)  <> " is verified successfully."
  return ()

getUser :: AuthRepo m => UserId -> m (Maybe Email)
getUser = findEmailFromUser

mkEmail :: Text -> Either [Text] Email
mkEmail = validate Email [ regexMatches emailAddrRegex "Not a valid email" ]
  where emailAddrRegex = [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]

rawEmail :: Email -> Text
rawEmail = emailRaw

mkPassword :: Text -> Either [Text] Password
mkPassword = validate Password [ lengthBetween 5 50 "Should be between 5 and 50 characters."
                               , regexMatches [re|\d|] "Should have a number"
                               , regexMatches [re|[A-Z]|] "Should have a uppercase letter"
                               , regexMatches [re|[a-z]|] "Should have a lowercase letter"
                               ]
                                                

rawPassword :: Password -> Text
rawPassword = passwordRaw

withUserContext :: (KatipContext m) => UserId -> m a -> m a
withUserContext uid = katipAddContext (sl "userid" uid)

