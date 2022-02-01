module Wordle.Grade where

import Prelude
import Data.Maybe (Maybe(..))

data Grade
  = Wrong
  | Correct
  | Partial

derive instance eqGrade :: Eq Grade

resultToString :: Grade -> String
resultToString = case _ of
  Wrong -> "Wrong"
  Correct -> "Correct"
  Partial -> "Partial"

instance semigroupGrade :: Semigroup Grade where
  append = case _, _ of
    Correct, _ -> Correct
    _, Correct -> Correct
    Partial, _ -> Partial
    _, Partial -> Partial
    _, _ -> Wrong

instance monoid :: Monoid Grade where
  mempty = Wrong
