module Lib
    ( someFunc,
      User (..)
    ) where

import qualified Adapter.InMemory.Auth      as M
import qualified Adapter.PostgreSQL.Auth    as PG
import           ClassyPrelude
import           Control.Monad.Catch        (MonadThrow)
import qualified Control.Monad.Fail         as Fail
import           Data.Aeson
import           Data.Aeson.TH
import           Domain.Auth
import           Katip
import           Language.Haskell.TH.Syntax (nameBase)

type State = (PG.State, TVar M.State)

newtype App a = App {
  unApp :: ReaderT State (KatipContextT IO) a
  } deriving (Applicative, Functor, Monad, MonadReader State, MonadIO, KatipContext, Katip, MonadThrow)

instance AuthRepo App where
  addAuth = PG.addAuth
  setEmailAsVerified = PG.setEmailAsVerified
  findUserByAuth = PG.findUserByAuth
  findEmailFromUser = PG.findEmailFromUser

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

data User = User
    { userId      :: Int
    , userName    :: Text
    , userHobbies :: [Text]
    }
    deriving (Show)
$(let strcutName = nameBase ''User
      lowercaseFirst (x : xs) = toLower [x] <> xs
      lowercaseFirst xs       = xs
      options = defaultOptions { fieldLabelModifier = lowercaseFirst . drop (length strcutName) }
   in deriveJSON options ''User)

someFunc :: IO ()
someFunc = withKatip $ \le -> do
  mstate <- newTVarIO M.initialState
  PG.withState pgCfg $ \pgState → run le (pgState, mstate) action
  where
    pgCfg = PG.Config
            { PG.configUrl = "postgresql://localhost/hauth"
            , PG.configStripeCount = 2
            , PG.configMaxOpenConnPerStripe = 5
            , PG.configIdleConnTimeout = 10
            }

action :: App ()
action = do
      let e = either invalidEmail id $ mkEmail "test@test.com"
          pswd = either invalidPswd id $ mkPassword "12HJbvv"
          auth = Auth e pswd
      register auth
      v <- PG.getNotificationsForEmail e
      verifyEmail  v
      Right session <- login auth
      Just uid <- resolveSessionId session
      Just registeredEmail <- getUser uid
      return ()

