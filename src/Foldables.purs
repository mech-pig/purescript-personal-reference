module Foldables where

import Prelude ((<), (>), map, otherwise)
import Data.Array (filter)
import Data.Maybe (Maybe(..))

-- let's start with the basics: map
compress :: Number -> Number -> Array Number -> Maybe (Array Number)
compress minValue maxValue xs
  | minValue > maxValue = Nothing
  | otherwise = Just (map compressValue xs) -- otherwise is just an alias for `true`
  where
    compressValue value
      | value < minValue = minValue
      | value > maxValue = maxValue
      | otherwise = value

-- and filter
positiveOnly :: Array Number -> Array Number
positiveOnly = filter (\n -> n > 0.0)
