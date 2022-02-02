module Main where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, toUpper)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Window (toEventTarget)
import Web.UIEvent.KeyboardEvent (fromEvent, key)
import Wordle.Game (nextProblem)
import Wordle.UI as Wordle
import Wordle.Fetch (fetch)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    result <- fetch "words5.txt"
    case result of
      Left err -> do
        log $ "fetch failed: " <> err
      Right text -> do
        let
          words = map toUpper $ split (Pattern "\n") text
        answer <- nextProblem words
        io <- runUI Wordle.component { words, answer } body
        liftEffect do
          win <- window
          listener <-
            eventListener
              ( \e -> case fromEvent e of
                  Nothing -> pure unit
                  Just keyboardEvent ->
                    launchAff_ do
                      let
                        k = key keyboardEvent
                      _ <- io.query $ H.mkTell $ Wordle.WindowKeyDown k
                      log k
                      pure unit
              )
          addEventListener (EventType "keydown") listener false (toEventTarget win)
