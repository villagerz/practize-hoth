module Domain.Validation where

import ClassyPrelude
import Text.Regex.PCRE.Heavy

{- |
Receives input and returns Maybe an error,
if valid, then return Nothing
-}
type Validation e a = a -> Maybe e

-- | run array of validation on input. if valid
--   then apply the supplied function, if not
--   the list of error 
validate :: (a -> b) -> [Validation e a] -> a -> Either [e] b
validate f vs a = case concatMap (\g -> maybeToList $ g a) vs of
                    [] -> Right $ f a
                    errs -> Left errs

rangeBetween :: (Ord a) => a -> a -> e -> Validation e a
rangeBetween min max msg a
  | a >= min && a <= max = Nothing
  | otherwise = Just msg

-- ^ MonoFoldable because length in ClassyPrelude needs it  
lengthBetween :: (MonoFoldable a) => Int -> Int -> e -> Validation e a
lengthBetween min max msg a = rangeBetween min max msg (length a)

regexMatches :: Regex -> e -> Validation e Text
regexMatches rgx msg txt = if txt =~ rgx then Nothing else Just msg
