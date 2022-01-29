module Wordle.Util
  ( snoc, take
  ) where

import Prelude
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits as String

snoc :: String -> String
snoc s = fromMaybe s $ String.slice 0 (negate 1) s

take :: Int -> String -> String
take n s = fromMaybe s $ String.slice 0 n s
