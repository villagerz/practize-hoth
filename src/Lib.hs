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
import Katip
import qualified Control.Monad.Fail as Fail

type State = TVar M.State
newtype App a = App {
  unApp :: ReaderT State (KatipContextT IO) a
  } deriving (Applicative, Functor, Monad, MonadReader State, MonadIO, KatipContext, Katip)

instance AuthRepo App where
  addAuth = M.addAuth
  setEmailAsVerified = M.setEmailAsVerified
  findUserByAuth = M.findUserByAuth
  findEmailFromUser = M.findEmailFromUser

instance Fail.MonadFail App where
  fail _ = error "this should not have happened"

instance SessionRepo App where
  newSession = M.newSession
  findUserBySession = M.findUserBySession

instance EmailVerificationNotif App where
  notifyEmailVerification = M.notifyEmailVerification

run :: LogEnv -> State -> App a -> IO a
run le s = runKatipContextT le () mempty . flip runReaderT s . unApp

withKatip :: (LogEnv -> IO a) -> IO a
withKatip app = bracket createLogEnv closeScribes app
  where createLogEnv = do
          le <- initLogEnv "HAuth" "prod"
          stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS)  V2
          registerScribe "stdout" stdoutScribe defaultScribeSettings le

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
someFunc = withKatip $ \le -> do
  state <- newTVarIO M.initialState
  run le state action

action :: App ()
action = do
      let e = either undefined id $ mkEmail "test@test.com"
          pswd = either undefined id $ mkPassword "12HJbvv"
          auth = Auth e pswd
      register auth
      Just v <- M.getNotificationsForEmail e -- I needed to implement an instance of Monaf.Fail for this to work
      verifyEmail  v
      Right session <- login auth
      Just uid <- resolveSessionId session
      Just registeredEmail <- getUser uid
      return ()
      
