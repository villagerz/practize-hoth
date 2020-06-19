module Config where

import qualified Adapter.PostgreSQL.Auth as PG
import qualified Adapter.RabbitMQ.Common as MQ
import           ClassyPrelude
import           System.Environment

data Config = Config
    { port  :: Int
    , redis :: String
    , mq    :: MQ.Config
    , pg    :: PG.Config
    }

envFromString ∷ (IsString a) ⇒ String → IO a
envFromString k = fromString <$> getEnv k

envRead ∷ Read a ⇒ String → IO a
envRead k = do
  raw ← getEnv k
  case readMay raw of
    Just val → return val
    Nothing  → throwString $ k <> ": Unable to parse [" <> raw <> "]"

fromenv ∷ IO Config
fromenv = Config <$> envRead "HAUTHPORT"
  <*> getEnv "REDIS_URL"
  <*> (MQ.Config <$> getEnv "MQ_URL" <*> pure 16)
  <*> (PG.Config <$> envFromString "PG_URL" <*> pure 2 <*> pure 5 <*> pure 10)


