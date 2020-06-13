module Adapter.RabbitMQ.Auth where

import qualified Adapter.InMemory.Auth   as M
import           Adapter.RabbitMQ.Common
import           ClassyPrelude
import           Control.Monad.Catch     (MonadCatch)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Has
import qualified Domain.Auth             as D
import           Katip
import           Network.AMQP
import           Prelude.Unicode         ((∘), (∧), (≡))


data EmailVerificationPayload = EmailVerificationPayload
    { email :: Text
    , vcode :: Text
    }

$(deriveJSON defaultOptions ''EmailVerificationPayload)


notifyEmailVerification :: (Rabbit r m) ⇒ D.Email -> D.VerificationCode -> m ()
notifyEmailVerification e v =
  let payload = EmailVerificationPayload (D.rawEmail e) v
  in publish "auth" "userRegistered" payload

consumeEmailVerification ∷ (M.InMemory r m, KatipContext m, MonadUnliftIO m, MonadCatch m)
  ⇒ (m Bool → IO Bool) → Message → IO Bool
consumeEmailVerification runner msg =
  runner $ consume msg handler
  where
    handler payload = do
      case D.mkEmail (email payload) of
        Left err → withMsgAndErr msg err $ do
          $(logTM) ErrorS "Email format is incorrect. rejecting"
          return False
        Right e → do
          let vc = vcode payload
          M.notifyEmailVerification e vc
          return True

init ∷ (M.InMemory r m, KatipContext m, MonadUnliftIO m, MonadCatch m)
 ⇒ State → (m Bool → IO Bool) → IO ()
init state runner = do
  initQueue state "verifyEmail" "auth" "userRegistered"
  initConsumer (sconsumerChan state) "verifyEmail" (consumeEmailVerification runner)
