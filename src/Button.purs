module Example.Basic.Button (component, State, Query(..), Message, Action) where

import Prelude
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.String.CodeUnits as String
import Effect.Aff (Aff)
import Halogen (ClassName(..), HalogenQ(..), modify_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State
  = { words :: Array String
    , inputs :: Array String
    , answer :: String
    , input :: String
    }

data Query a
  = WindowKeyDown String a

data Action
  = Toggle

data Message
  = Toggled Boolean

component :: forall q o m. H.Component Query (Array String) Message Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction, handleQuery = handleQuery
              }
    }

initialState :: Array String -> State
initialState words =
  { words
  , inputs:
      [ "xxxxx"
      , "xxxxx"
      , "xxxxx"
      , "xxxxx"
      , "xxxxx"
      ]
  , input: ""
  , answer:
      case head words of
        Just x -> x
        Nothing -> ""
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div []
    [ HH.div
        [ HP.class_ (ClassName "inputs") ]
        $ map
            ( \word ->
                HH.div [ HP.class_ (ClassName "row") ]
                  ( map (\c -> HH.div [ HP.class_ (ClassName "cell") ] [ HH.text $ String.singleton c ])
                      $ toCharArray word
                  )
            )
            state.inputs
    , HH.div [] [ HH.text state.input ]
    ]

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Toggle -> H.modify_ \st -> st

handleQuery :: forall m a. Query a -> H.HalogenM State Action () Message m (Maybe a)
handleQuery = case _ of
  WindowKeyDown key a -> do
    modify_ (\state -> state { input = state.input <> key })
    pure (Just a)

keyboard =
  [ [ "Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P" ]
  , [ "A", "S", "D", "F", "G", "H", "J", "K", "L" ]
  , [ "Z", "X", "C", "V", "B", "N", "M" ]
  ]
