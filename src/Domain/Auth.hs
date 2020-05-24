module Domain.Auth
  ( mkEmail
  , mkPassword
  )
where

import ClassyPrelude

data Auth = Auth
  { authEmail :: Email
  , authPassword :: Password
  } deriving (Show, Eq)

data RegistrationError = RegistrationErrorEmailTaken deriving (Show, Eq)

newtype Email = Email { emailRaw :: Text } deriving (Show, Eq)
newtype Password = Password { passwordRaw :: Text } deriving (Show, Eq)

mkEmail :: Text -> Either [Text] Email
mkEmail = error "TODO: not yet defined"

rawEmail :: Email -> Text
rawEmail = emailRaw

mkPassword :: Text -> Either [Text] Password
mkPassword = error "TODO: net yet implemented"

rawPassword :: Password -> Text
rawPassword = passwordRaw
