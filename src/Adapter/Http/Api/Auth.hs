module Adapter.Http.Api.Auth where

import           Adapter.Http.Common
import           ClassyPrelude
import           Data.Aeson                hiding (json, (.:))
import           Domain.Auth
import           Katip
import           Network.HTTP.Types.Status
import           Prelude.Unicode           ((∘), (∧), (≡), (⊥))
import           Text.Digestive.Form       ((.:))
import qualified Text.Digestive.Form       as DF
import           Web.Scotty.Trans

type ScottyHauth e m = (ScottyError e, MonadIO m, KatipContext m, AuthRepo m, EmailVerificationNotif m, SessionRepo m)

routes ∷ ScottyHauth e m ⇒  ScottyT e m ()
routes = do
  post "/api/auth/register" postToRegister
  post "/api/auth/verifyemail" postToVerifyEmail
  post "/api/auth/login" postToLogin
  get "/api/users" getUserDetails

_userDetailErrStr = "Should not happen to a user with a cookie : SessionId mapped to invalid user:"
getUserDetails ∷ ScottyHauth e m ⇒ ActionT e m ()
getUserDetails = do
  uid ← reqCurrentUserId
  memail ← lift $ getUser uid
  case memail of
    Nothing    → raise $ stringError _userDetailErrStr
    Just email → json $ rawEmail email

postToRegister ∷ ScottyHauth e m  ⇒ ActionT e m ()
postToRegister = do
  input ← parseAuthJson authForm
  domainResult ← lift $ register input
  case domainResult of
    Left RegistrationErrorEmailTaken → do
      status status400
      json ("EmailTaken" ∷ Text)
    Right _ → return ()

postToVerifyEmail ∷ ScottyHauth e m ⇒ ActionT e m ()
postToVerifyEmail = do
  input ← parseAuthJson verifyForm
  domainResult ← lift $ verifyEmail input
  case domainResult of
    Left EmailVerfificationErrorInvalidCode → do
      status status400
      json ("InvalidCode" ∷ Text)
    Right _ → return ()

postToLogin ∷ ScottyHauth e m ⇒ ActionT e m ()
postToLogin = do
  input ← parseAuthJson authForm
  domainResult ← lift $ login input
  case domainResult of
    Left LoginInvalidAuth → do
      status status400
      json ("InvalidAuth" ∷ Text)
    Left LoginEmailNotVerified → do
      status status400
      json ("email unverified" ∷ Text)
    Right session → do
      setSessionIdInCookie session
      return ()


verifyForm ∷ (Monad m) ⇒ DF.Form [Text] m VerificationCode
verifyForm = DF.text Nothing

authForm ∷ (Monad m) ⇒ DF.Form [Text] m Auth
authForm =  Auth <$> "email" .: emailForm <*> "password" .: passwordForm
  where
    emailForm = DF.validate (toResult ∘ mkEmail) (DF.text Nothing)
    passwordForm = DF.validate (toResult ∘ mkPassword) (DF.text Nothing)
