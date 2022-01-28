module Main where

import Prelude
import Data.Array (filter)
import Data.String (Pattern(..), length, split, joinWith)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS

main :: Effect Unit
main =
  launchAff_ do
    file <- FS.readTextFile UTF8 "english-words-master/words_alpha.txt"
    let
      lines = filter (\s -> length s == 5) $ split (Pattern "\r\n") file
    -- Console.log $ joinWith "\n" lines
    FS.writeTextFile UTF8 "public/words5.txt" $ joinWith "\n" lines
