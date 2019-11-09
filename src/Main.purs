module Main where

import Effect (Effect)
import Effect.Console(log)
import Prelude

import Reader as Reader
import MonadTransformers as MonadTransformers


main :: Effect Unit
main = do
  log "# Reader"
  Reader.main
  log "\n"
  log "# MonadTransformers"
  MonadTransformers.main
  log "\n"
