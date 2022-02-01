module Wordle.Game where

import Prelude
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String.CodeUnits (toCharArray)
import Data.String.CodeUnits as String
import Data.Tuple (Tuple(..))
import Wordle.Grade (Grade(..))
import Wordle.Grade as Grade

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

validate :: String -> String -> Array (Tuple String Grade)
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

getCharGrade :: String -> Array String -> String -> Maybe Grade
getCharGrade answer inputs char = Array.fold xs
  where
  xs :: Array (Maybe Grade)
  xs = map (\i -> getCharGrade_ answer i char) inputs

getCharGrade_ :: String -> String -> String -> Maybe Grade
getCharGrade_ answer input char =
  if Array.elem char collects then
    Just Correct
  else if Array.elem char partials then
    Just Partial
  else if Array.elem char wrongs then
    Just Wrong
  else
    Nothing
  where
  results = validate answer input

  collects :: Array String
  collects = Array.catMaybes $ map (\(Tuple s r) -> if r == Correct then Just s else Nothing) results

  partials :: Array String
  partials = Array.catMaybes $ map (\(Tuple s r) -> if r == Partial then Just s else Nothing) results

  wrongs :: Array String
  wrongs = Array.catMaybes $ map (\(Tuple s r) -> if r == Wrong then Just s else Nothing) results
