module Wordle.UI
  ( Action
  , Message
  , Query(..)
  , State
  , component
  ) where

import Prelude
import Data.Array (mapWithIndex, length)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), toUpper)
import Data.String.CodeUnits (toCharArray)
import Data.String.CodeUnits as String
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Milliseconds(..), delay)
import Halogen (ClassName(..), get, modify_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Wordle.Dialog.Lose as Lose
import Wordle.Dialog.Win as Win
import Wordle.Game (nextProblem, validate, validateWord)
import Wordle.Keyboard as Keyboard
import Wordle.Util (take, snoc)

type State
  = { words :: Array String
    , inputs :: Array String
    , answer :: String
    , input :: String
    , vibration :: Boolean
    , winDialogVisible :: Boolean
    , loseDialogVisible :: Boolean
    , active :: Boolean
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
  , inputs: []
  , input: ""
  , answer: params.answer
  , vibration: false
  , winDialogVisible: false
  , loseDialogVisible: false
  , active: true
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
                            , ClassName $ show result
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
                      , ClassName if state.active && String.length state.input == i then "active" else ""
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
      [ HH.h1 [] [ HH.text "WORDLE CLONE" ]
      , HH.div
          [ HP.class_ (ClassName "inputs") ]
          concat
      -- , HH.div [] [ HH.text state.answer ]
      , Keyboard.render state.answer state.inputs KeyDown
      , Win.render state.winDialogVisible state CloseDialog
      , Lose.render state.loseDialogVisible state CloseDialog
      ]

handleAction :: Action -> H.HalogenM State Action () Message Aff Unit
handleAction = case _ of
  KeyDown key -> do
    handleKey key
  CloseDialog -> do
    state <- get
    answer <- nextProblem state.words
    modify_ \s ->
      s
        { winDialogVisible = false
        , loseDialogVisible = false
        , answer = answer
        , input = ""
        , inputs = []
        , active = true
        }

handleQuery :: forall a. Query a -> H.HalogenM State Action () Message Aff (Maybe a)
handleQuery = case _ of
  WindowKeyDown key a -> do
    handleKey key
    pure (Just a)

handleKey :: forall output. String -> H.HalogenM State Action () output Aff Unit
handleKey key = do
  s <- get
  if s.active then case key of
    "Enter" -> do
      current <- get
      if String.length current.input == 5 then
        if Array.elem current.input current.words then do
          modify_ \state ->
            state
              { inputs = state.inputs <> [ state.input ]
              , input = ""
              , vibration = false
              , active = false
              }
          H.liftAff $ delay (Milliseconds 1500.0)
          s <- get
          if validateWord current.answer current.input then do
            H.liftAff $ delay (Milliseconds 1000.0)
            modify_ \state -> state { winDialogVisible = true }
          else if length s.inputs == 6 then do
            modify_ \state -> state { loseDialogVisible = true }
          else do
            modify_ \state -> state { active = true }
        else do
          modify_ \state -> state { vibration = false }
          H.liftAff $ delay (Milliseconds 1.0)
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
  else
    pure unit
