module Adapter.PostgreSQL.Auth where

import           ClassyPrelude
import           Control.Monad.Catch                  (MonadThrow)
-- ^ need explicit for LTS > 11.0 which I used, since ClassyPrelude no longer exports it
import           Control.Monad.Except
import           Data.Has
import           Data.Pool
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Migration
import qualified Domain.Auth                          as D
import           Prelude.Unicode                      ((∘), (∧), (≡))
import           Text.StringRandom

type State = Pool Connection
type PG r m = (Has State r, MonadReader r m, MonadIO m, MonadThrow m)

data Config = Config
    { configUrl                  :: ByteString
    , configStripeCount          :: Int
    , configMaxOpenConnPerStripe :: Int
    , configIdleConnTimeout      :: NominalDiffTime
    }

addAuth ∷ PG r m ⇒ D.Auth → m (Either D.RegistrationError (D.UserId, D.VerificationCode))
addAuth (D.Auth e p) = do
  let rawEmail = D.rawEmail e
      rawPassw = D.rawPassword p
  vcode ← liftIO $ D.genvcode rawEmail
  result ← withConn $ \c →
    try $ query c qry (rawEmail, rawPassw, vcode)
  return $ eitherPGResult result vcode
  where
    qry = "insert into auths (email, pass, email_verification_code, is_email_verified) \
          \ values (?, crypt(?, gen_salt('bf')), ?, 'f') returning id"

eitherPGResult ∷ Either SqlError [Only D.UserId] → D.VerificationCode → Either D.RegistrationError (D.UserId, D.VerificationCode)
eitherPGResult result vc =
  case result of
    Right [Only uId] → Right (uId, vc)
    Right _ → throwError $ D.UnexpectedRegistration "Should not happend: PG didn't return user id"
    Left err@SqlError{sqlState = state, sqlErrorMsg = msg} →
      if state ≡ "23505" ∧ "auths_email_key" `isInfixOf` msg
      then Left D.RegistrationErrorEmailTaken
      else throwError $ D.UnexpectedRegistration $ "Unhandled PG exception:" <> show err

getNotificationsForEmail ∷ PG r m ⇒ D.Email → m D.VerificationCode
getNotificationsForEmail em = do
  result ← withConn $ \c → query c qry (Only ∘ D.rawEmail $ em)
  case result of
    [Only vc] → return vc
    _         → return "impossible" -- this function is used for testing only through Lib.someFunc, so loosey goosey
  where
    qry = "select email_verification_code from auths where email = ?"

findUserByAuth ∷ PG r m ⇒ D.Auth → m (Maybe (D.UserId, Bool))
findUserByAuth (D.Auth e p) = do
  let rawe = D.rawEmail e
      rawp = D.rawPassword p
  result ← withConn $ \c → query c qry (rawe, rawp)
  return $ case result of
    [(uid, isverified)] → Just (uid, isverified)
    _                   → Nothing
  where
    qry = "select id, is_email_verified \
          \from auths where email = ? and pass = crypt(?, pass)"

findEmailFromUser ∷ PG r m ⇒ D.UserId → m (Maybe D.Email)
findEmailFromUser uid = do
  result ← withConn $ \c → query c qry (Only uid)
  case result of
    [Only mail] → case D.mkEmail mail of
      Right email → return $ Just email
      _           → throwString $ "Should not happen: email in DB is not valid: " <> unpack mail
    _ → return Nothing
  where
    qry = "select cast(email as text) from auths where id = ?"

setEmailAsVerified ∷ PG r m ⇒ D.VerificationCode → m (Either D.EmailVerfificationError (D.UserId, D.Email))
setEmailAsVerified vc = do
  result ← withConn $ \c → query c qry (Only vc)
  case result of
    [(uid, mail)] → case D.mkEmail mail of
      Right email → return $ Right (uid, email)
      _           → throwString $ "Should not happen: email in DB is not valid: " <> unpack mail
    _ → return $ Left D.EmailVerfificationErrorInvalidCode
  where
    qry = "update auths set \
          \is_email_verified = 't' \
          \where email_verification_code = ? \
          \returning id, cast (email as text)"


withConn ∷ PG r m ⇒ (Connection → IO a) → m a
withConn action = do
  pool ← asks getter
  liftIO ∘ withResource pool $ \c → action c

withPool ∷ Config → (State → IO a) → IO a
withPool cfg action = bracket initPool cleanPool action
  where initPool = createPool openConn close
                   (configStripeCount cfg)
                   (configIdleConnTimeout cfg)
                   (configMaxOpenConnPerStripe cfg)
        cleanPool = destroyAllResources
        openConn = connectPostgreSQL (configUrl cfg)

withState ∷ Config → (State → IO a) → IO a
withState cfg action = withPool cfg $ \s → do
  migrate s
  action s

migrate :: State -> IO ()
migrate pool = withResource pool $ \conn -> do
  result <- withTransaction conn (runMigrations False conn cmds)
  case result of
    MigrationError err -> throwString err
    _                  -> return ()
    where
      cmds = [ MigrationInitialization
             , MigrationDirectory "src/Adapter/PostgreSQL/Migrations"
             ]


