module Main where

import Effect (Effect)
import Effect.Class.Console (log)
import Prelude

import Reader as Reader


main :: Effect Unit
main = do
  Reader.main
