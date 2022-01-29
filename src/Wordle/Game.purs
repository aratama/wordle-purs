module Wordle.Game where

import Prelude
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String.CodeUnits (toCharArray)
import Data.String.CodeUnits as String
import Data.Tuple (Tuple(..))
import Wordle.Grade (Result(..))

type Game a
  = { words :: Array String
    , inputs :: Array String
    , answer :: String
    , input :: String
    | a
    }

validateWord :: String -> String -> Boolean
validateWord answer input =
  Array.all (\(Tuple _ r) -> r == Correct)
    (validate answer input)

validate :: String -> String -> Array (Tuple String Result)
validate answer input =
  mapWithIndex
    ( \i c ->
        let
          s = String.singleton c
        in
          Tuple s
            $ if String.charAt i answer == Just c then
                Correct
              else if String.contains (Pattern s) answer then Partial else Wrong
    )
    (toCharArray input)
