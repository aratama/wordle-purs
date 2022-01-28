module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS

main :: Effect Unit
main = do
  log "🍝"
