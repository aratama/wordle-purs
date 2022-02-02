module Wordle.Grade where

import Prelude

data Grade
  = Correct
  | Partial
  | Wrong

derive instance eqGrade :: Eq Grade

instance showGradle :: Show Grade where
  show = case _ of
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

instance monoidGrade :: Monoid Grade where
  mempty = Wrong
