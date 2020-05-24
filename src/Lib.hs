module Lib
    ( someFunc,
      User (..)
    ) where

import ClassyPrelude
import Language.Haskell.TH.Syntax (nameBase)
import Data.Aeson
import Data.Aeson.TH

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
