module Domain.Auth
  ( mkEmail
  , mkPassword
  )
where

import ClassyPrelude
import Domain.Validation
import Text.Regex.PCRE.Heavy

data Auth = Auth
  { authEmail :: Email
  , authPassword :: Password
  } deriving (Show, Eq)

data RegistrationError = RegistrationErrorEmailTaken deriving (Show, Eq)

newtype Email = Email { emailRaw :: Text } deriving (Show, Eq)
newtype Password = Password { passwordRaw :: Text } deriving (Show, Eq)

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
