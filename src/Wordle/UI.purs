module Wordle.UI
  ( Action
  , Message
  , Query(..)
  , State
  , component
  ) where

import Prelude
import Control.Monad.State (class MonadState)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), toUpper)
import Data.String.CodeUnits (toCharArray)
import Data.String.CodeUnits as String
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Halogen (ClassName(..), modify_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Wordle.Game (validate, validateWord)
import Wordle.Grade (resultToString)
import Wordle.Keyboard as Keyboard
import Wordle.Util (take, snoc)

type State
  = { words :: Array String
    , inputs :: Array String
    , answer :: String
    , input :: String
    }

type Params
  = { words :: Array String, answer :: String }

data Query a
  = WindowKeyDown String a

data Action
  = KeyDown String

data Message
  = Toggled Boolean

component :: H.Component Query Params Message Aff
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

initialState :: Params -> State
initialState params =
  { words: params.words
  , inputs:
      []
  , input: ""
  , answer: params.answer
  }

maxTrials :: Int
maxTrials = 6

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    rows =
      map
        ( \word ->
            HH.div
              [ HP.classes
                  [ ClassName "row"
                  , ClassName if validateWord state.answer word then "" else "vibration"
                  ]
              ]
              ( map
                  ( \(Tuple c result) ->
                      HH.div
                        [ HP.classes
                            [ ClassName "cell"
                            , ClassName $ resultToString result
                            ]
                        ]
                        [ HH.text c ]
                  )
                  $ validate state.answer word
              )
        )
        state.inputs

    inputs =
      HH.div
        [ HP.classes [ ClassName "row" ] ]
        ( map (\c -> HH.div [ HP.class_ (ClassName "cell") ] [ HH.text $ String.singleton c ])
            $ toCharArray (take 5 (state.input <> "     "))
        )

    empty =
      HH.div [ HP.class_ (ClassName "row") ]
        ( map (\c -> HH.div [ HP.class_ (ClassName "cell") ] [ HH.text $ String.singleton c ])
            $ toCharArray "     "
        )

    concat = Array.slice 0 maxTrials (rows <> [ inputs ] <> Array.replicate 5 empty)
  in
    HH.div [ HP.class_ (ClassName "root") ]
      [ HH.h1 [] [ HH.text "Wordle Extreme" ]
      , HH.div
          [ HP.class_ (ClassName "inputs") ]
          concat
      , HH.div [] [ HH.text state.answer ]
      , Keyboard.render KeyDown
      ]

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  KeyDown key -> handleKey key

handleQuery :: forall m a. Query a -> H.HalogenM State Action () Message m (Maybe a)
handleQuery = case _ of
  WindowKeyDown key a -> do
    handleKey key
    pure (Just a)

handleKey :: forall m. MonadState State m => String -> m Unit
handleKey key =
  modify_
    ( \state -> case key of
        "Enter" ->
          if String.length state.input == 5 then
            state
              { inputs = state.inputs <> [ state.input ]
              , input = ""
              }
          else
            state
        "Backspace" -> state { input = snoc state.input }
        _ ->
          if String.contains (Pattern $ toUpper key) "ABCDEFGHIJKLMNOPQRSTUVWXYZ" then
            state { input = take 5 (state.input <> toUpper key) }
          else
            state
    )
