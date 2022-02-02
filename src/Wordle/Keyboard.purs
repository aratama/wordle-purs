module Wordle.Keyboard
  ( keyboardKeys, render
  ) where

import Prelude
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Wordle.Game (getCharGrade)
import Data.Maybe (Maybe(..))

render :: forall action m. String -> Array String -> (String -> action) -> H.ComponentHTML action () m
render answer inputs keyDown =
  HH.div [ HP.classes [ ClassName "key-rows" ] ]
    $ map
        ( \row ->
            HH.div [ HP.classes [ ClassName "key-row" ] ]
              $ map
                  ( \key ->
                      HH.button
                        [ HP.classes
                            [ case getCharGrade answer inputs key of
                                Just grade -> ClassName $ show grade
                                Nothing -> ClassName ""
                            ]
                        , HE.onClick $ \_ -> keyDown key
                        ]
                        [ HH.text key ]
                  )
                  row
        )
        keyboardKeys

keyboardKeys :: Array (Array String)
keyboardKeys =
  [ [ "Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P" ]
  , [ "A", "S", "D", "F", "G", "H", "J", "K", "L" ]
  , [ "Enter", "Z", "X", "C", "V", "B", "N", "M", "Backspace" ]
  ]
