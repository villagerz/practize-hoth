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

data Auth = Auth
  { authEmail :: Email
  , authPassword :: Password
  } deriving (Show, Eq)
data UserId = Int deriving (Show, Eq, Ord)
data SessionId = Text deriving (Show, Eq, Ord)

data RegistrationError = RegistrationErrorEmailTaken deriving (Show, Eq)
data EmailVerfificationError = EmailVerfificationErrorInvalidCode deriving (Show, Eq)
data LoginError = LoginInvalidAuth | LoginEmailNotVerified deriving (Show, Eq)

newtype Email = Email { emailRaw :: Text } deriving (Show, Eq, Ord)
newtype Password = Password { passwordRaw :: Text } deriving (Show, Eq)

type VerificationCode = Text

class Monad m => AuthRepo m where
  addAuth :: Auth -> m (Either RegistrationError VerificationCode)
  setEmailAsVerified :: VerificationCode -> m (Either EmailVerfificationError ())
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
  findEmailFromUser :: UserId -> m (Maybe Email)

class Monad m => SessionRepo m where
  newSession :: UserId -> m SessionId
  findUserBySession :: SessionId -> m (Maybe UserId)

class Monad m => EmailVerificationNotif m where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

login :: (AuthRepo m, SessionRepo m) => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
  result <- lift $ findUserByAuth auth
  case result of
    Nothing -> throwError LoginInvalidAuth
    Just (_, False) -> throwError LoginEmailNotVerified
    Just (uid, _) -> lift $ newSession uid

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserBySession

register :: (AuthRepo m, EmailVerificationNotif m) => Auth -> m (Either RegistrationError ())
register auth = runExceptT $ do
  vCode <- ExceptT $ addAuth auth
  let email = authEmail auth
  lift $ notifyEmailVerification email vCode

verifyEmail :: AuthRepo m => VerificationCode -> m (Either EmailVerfificationError ())
verifyEmail = setEmailAsVerified

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

