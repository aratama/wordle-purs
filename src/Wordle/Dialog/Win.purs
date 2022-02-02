module Wordle.Dialog.Win where

import Halogen (AttrName(..), ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Wordle.Game (Game)

render :: forall action m a. Boolean -> Game a -> action -> H.ComponentHTML action () m
render visible _ closeDialog =
  HH.div [ HP.classes [ ClassName "dialog-outer" ], HP.style if visible then "" else "display: none" ]
    [ HH.dialog [ HP.attr (AttrName "open") "", HP.style "display: flex; flex-direction: column" ]
        [ HH.h2 [] [ HH.text "You Win!" ]
        , HH.button [ HE.onClick \_ -> closeDialog ] [ HH.text "Close" ]
        ]
    ]
