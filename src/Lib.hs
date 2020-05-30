module Lib
    ( someFunc,
      User (..)
    ) where

import ClassyPrelude
import Language.Haskell.TH.Syntax (nameBase)
import Data.Aeson
import Data.Aeson.TH
import qualified Adapter.InMemory.Auth as M
import Domain.Auth

type State = TVar M.State
newtype App a = App { unApp :: ReaderT State IO a } deriving (Applicative
                                                              ,Functor
                                                              , Monad
                                                              , MonadReader State
                                                              , MonadIO)
instance AuthRepo App where
  addAuth = M.addAuth
  setEmailAsVerified = M.setEmailAsVerified
  findUserByAuth = M.findUserByAuth
  findEmailFromUser = M.findEmailFromUser

instance SessionRepo App where
  newSession = M.newSession
  findUserBySession = M.findUserBySession

instance EmailVerificationNotif App where
  notifyEmailVerification = M.notifyEmailVerification

run :: State -> App a -> IO a
run s = flip runReaderT s . unApp



data User = User { userId :: Int
                 , userName :: Text
                 , userHobbies :: [Text]
                 } deriving (Show)
$(let strcutName = nameBase ''User
      lowercaseFirst (x : xs) = toLower [x] <> xs
      lowercaseFirst xs = xs
      options = defaultOptions { fieldLabelModifier = lowercaseFirst . drop (length strcutName) }
   in deriveJSON options ''User)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
