module Wordle.UI
  ( Action
  , Message
  , Query(..)
  , State
  , component
  ) where

import Prelude
import Control.Monad.Free (liftF)
import Control.Monad.State (class MonadState, lift)
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), toUpper)
import Data.String.CodeUnits (toCharArray)
import Data.String.CodeUnits as String
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..), get, liftAff, liftEffect, modify_, modify)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties (enabled)
import Halogen.HTML.Properties as HP
import Wordle.Dialog as Dialog
import Wordle.Game (validate, validateWord)
import Wordle.Grade (resultToString)
import Wordle.Keyboard as Keyboard
import Wordle.Util (take, snoc)

type State
  = { words :: Array String
    , inputs :: Array String
    , answer :: String
    , input :: String
    , vibration :: Boolean
    , dialogVisible :: Boolean
    , enable :: Boolean
    }

type Params
  = { words :: Array String, answer :: String }

data Query a
  = WindowKeyDown String a

data Action
  = KeyDown String
  | CloseDialog

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
  , vibration: false
  , dialogVisible: false
  , enable: true
  }

maxTrials :: Int
maxTrials = 6

render :: State -> H.ComponentHTML Action () Aff
render state =
  let
    rows =
      map
        ( \word ->
            HH.div
              [ HP.classes [ ClassName "row" ]
              ]
              ( map
                  ( \(Tuple c result) ->
                      HH.div
                        [ HP.classes
                            [ ClassName "cell"
                            , ClassName $ resultToString result
                            , ClassName "open"
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
        [ HP.classes
            [ ClassName "row"
            , ClassName if state.vibration then "vibration" else ""
            ]
        ]
        ( mapWithIndex
            ( \i c ->
                HH.div
                  [ HP.classes
                      [ ClassName "cell"
                      , ClassName if state.enable && String.length state.input == i then "active" else ""
                      ]
                  ]
                  [ HH.text $ String.singleton c ]
            )
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
      [ HH.h1 [] [ HH.text "WORDLE SPEEDRUN" ]
      , HH.div
          [ HP.class_ (ClassName "inputs") ]
          concat
      , HH.div [] [ HH.text state.answer ]
      , Keyboard.render KeyDown
      , Dialog.render state.dialogVisible state CloseDialog
      ]

handleAction :: Action -> H.HalogenM State Action () Message Aff Unit
handleAction = case _ of
  KeyDown key -> do
    handleKey key
  CloseDialog -> do
    modify_ \state -> state { dialogVisible = false }

handleQuery :: forall a. Query a -> H.HalogenM State Action () Message Aff (Maybe a)
handleQuery = case _ of
  WindowKeyDown key a -> do
    handleKey key
    pure (Just a)

handleKey :: forall output. String -> H.HalogenM State Action () output Aff Unit
handleKey key = case key of
  "Enter" -> do
    current <- get
    if String.length current.input == 5 then
      if Array.elem current.input current.words then do
        modify_ \state ->
          state
            { inputs = state.inputs <> [ state.input ]
            , input = ""
            , vibration = false
            , enable = false
            }
        H.liftAff $ delay (Milliseconds 1500.0)
        if validateWord current.answer current.input then do
          H.liftAff $ delay (Milliseconds 1000.0)
          modify_ \state -> state { dialogVisible = true }
        else do
          modify_ \state -> state { enable = true }
      else
        modify_ \state -> state { vibration = true }
    else
      pure unit
  "Backspace" -> modify_ \state -> state { input = snoc state.input, vibration = false }
  _ ->
    modify_ \state ->
      if String.contains (Pattern $ toUpper key) "ABCDEFGHIJKLMNOPQRSTUVWXYZ" then
        state { input = take 5 (state.input <> toUpper key), vibration = false }
      else
        state
