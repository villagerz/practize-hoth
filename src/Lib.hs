module Lib
    ( main,
      User (..)
    ) where

import qualified Adapter.Http.Main          as Http
import qualified Adapter.InMemory.Auth      as M
import qualified Adapter.PostgreSQL.Auth    as PG
import qualified Adapter.RabbitMQ.Auth      as MQAuth
import qualified Adapter.RabbitMQ.Common    as MQ
import qualified Adapter.Redis.Auth         as Redis
import           ClassyPrelude
import           Control.Monad.Catch        (MonadCatch, MonadThrow)
import qualified Control.Monad.Fail         as Fail
import           Data.Aeson
import           Data.Aeson.TH
import           Domain.Auth
import           Katip
import           Language.Haskell.TH.Syntax (nameBase)
import           Text.StringRandom
import           Web.Scotty.Trans

type State = (PG.State, Redis.State, MQ.State, TVar M.State)


newtype App a = App {
  unApp :: ReaderT State (KatipContextT IO) a
  } deriving (Applicative, Functor, Monad, MonadReader State,
              MonadIO, KatipContext, Katip, MonadThrow,  MonadCatch )


instance AuthRepo App where
  addAuth = PG.addAuth
  setEmailAsVerified = PG.setEmailAsVerified
  findUserByAuth = PG.findUserByAuth
  findEmailFromUser = PG.findEmailFromUser

instance Fail.MonadFail App where
  fail _ = error "this should not have happened"

instance SessionRepo App where
  newSession = Redis.newSession
  findUserBySession = Redis.findUserBySession

instance EmailVerificationNotif App where
  notifyEmailVerification = MQAuth.notifyEmailVerification

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

main :: IO ()
main = withState $ \port le state@(_,_,mqState,_) -> do
  let runner = run le state
  MQAuth.init mqState runner
  Http.main port runner


withState ∷ (Int → LogEnv → State → IO ()) → IO ()
withState action =  withKatip $ \le → do
  mstate ← newTVarIO M.initialState
  PG.withState pgCfg $ \pgState →
    Redis.withState redisCfg $ \redisState →
     MQ.withState mqCfg 16 $ \mqState → do
        let state = (pgState, redisState, mqState, mstate)
        action port le state
    where
    port = 3300
    redisCfg = "redis://localhost:6379/0"
    pgCfg = PG.Config
            { PG.configUrl = "postgresql://localhost/hauth"
            , PG.configStripeCount = 2
            , PG.configMaxOpenConnPerStripe = 5
            , PG.configIdleConnTimeout = 10
            }
    mqCfg = "amqp://localhost:5672/%2F"



