module Main
  ( main
  ) where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Array as Array
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String (Pattern(..), split, toUpper)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (randomInt)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Window (toEventTarget)
import Web.UIEvent.KeyboardEvent (fromEvent, key)
import Wordle.UI as Wordle

main :: Effect Unit
main =
  HA.runHalogenAff do
    result <- AX.request (AX.defaultRequest { url = "/words5.txt", method = Left GET, responseFormat = ResponseFormat.string })
    words <- case result of
      Left err -> do
        log $ "GET /api response failed to decode: " <> AX.printError err
        pure []
      Right response -> do
        pure $ map toUpper $ split (Pattern "\n") response.body
    body <- HA.awaitBody
    answerIndex <- liftEffect $ randomInt 0 (Array.length words)
    let
      answer = Maybe.fromMaybe "xxxxx" $ Array.index words answerIndex
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
