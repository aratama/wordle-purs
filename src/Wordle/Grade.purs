module Wordle.Grade where

import Prelude

data Result
  = Wrong
  | Correct
  | Partial

derive instance eqResult :: Eq Result

resultToString :: Result -> String
resultToString = case _ of
  Wrong -> "Wrong"
  Correct -> "Correct"
  Partial -> "Partial"
