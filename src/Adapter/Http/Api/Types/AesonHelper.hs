module Adapter.Http.Api.Types.AesonHelper where

import           ClassyPrelude
import           Control.Monad.Fail
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Language.Haskell.TH.Syntax
import           Prelude.Unicode            ((∘), (∧), (≡), (⊥))

withSmartConstructor ∷ (a → Either [Text] b) → a → Parser b
withSmartConstructor f a =
  case f a of
    Left errs → fail $ intercalate ". " . map unpack $ errs
    Right val → return val

deriveJSONRecord ∷ Name → Q [Dec]
deriveJSONRecord record =
  let lowercase1 (y:ys) = toLower [y] <> ys
      lowercase1 ""     = ""
      structname = nameBase record
      opts = defaultOptions {
        fieldLabelModifier = lowercase1 ∘ drop (length structname)
        }
  in deriveJSON opts record

deriveJSONSumType ∷ Name → Q [Dec]
deriveJSONSumType record =
  let structname = nameBase record
      opts = defaultOptions {
        constructorTagModifier = drop (length structname)
        , tagSingleConstructors = True
        }
  in deriveJSON opts record

deriveJSONUnwrap ∷ Name → Q [Dec]
deriveJSONUnwrap record =
  let opts = defaultOptions {
                            unwrapUnaryRecords = True
                            }
  in deriveJSON opts record


