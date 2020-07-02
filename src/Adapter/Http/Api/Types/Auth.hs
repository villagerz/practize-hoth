{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Adapter.Http.Api.Types.Auth where

import           Adapter.Http.Api.Types.AesonHelper
import           ClassyPrelude
import           Data.Aeson
import           Domain.Auth
import           Prelude.Unicode                    ((∘), (∧), (≡), (⊥))

instance FromJSON Email where
  parseJSON = withText "Email" $ withSmartConstructor mkEmail

instance FromJSON Password where
  parseJSON = withText "Password" $ withSmartConstructor mkPassword

-- I get "Duplicate instance declaration" when I add the following
-- $(map concat ∘ sequence $
--  [ deriveJSONRecord ''Auth
--  , deriveJSONUnwrap ''Email
--  , deriveJSONUnwrap ''Password
--  , deriveJSONSumType ''RegistrationError
--  , deriveJSONSumType ''EmailVerfificationError
--  , deriveJSONSumType ''LoginError
--  ])

