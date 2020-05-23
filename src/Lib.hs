module Lib
    ( someFunc,
      User (..)
    ) where

import ClassyPrelude
import Data.Aeson

data User = User { userId :: Int
                 , userName :: Text
                 , userHobbies :: [Text]
                 } deriving (Show)

instance ToJSON User where
  toJSON (User id name hobs) = object [ "id" .= id
                                      , "name" .= name
                                      , "hobbies" .= hobs
                                      ]

instance FromJSON User where
  parseJSON = withObject "User" $ \v ->
    User <$> v .: "id"
         <*> v .: "name"
         <*> v .: "hobbies"
         
someFunc :: IO ()
someFunc = putStrLn "someFunc"
