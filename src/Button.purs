module Example.Basic.Button (component) where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State
  = { words :: Array String }

data Action
  = Toggle

component :: forall q o m. H.Component q (Array String) o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: Array String -> State
initialState words = { words }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    []
    $ map (\word -> HH.div [] [ HH.text word ]) state.words

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Toggle -> H.modify_ \st -> st
