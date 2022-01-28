module Main
  ( main
  ) where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Aff (launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Example.Basic.Button as Button
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Window (toEventTarget)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, fromEvent, key)

main :: Effect Unit
main =
  HA.runHalogenAff do
    result <- AX.request (AX.defaultRequest { url = "/words5.txt", method = Left GET, responseFormat = ResponseFormat.string })
    words <- case result of
      Left err -> do
        log $ "GET /api response failed to decode: " <> AX.printError err
        pure []
      Right response -> do
        pure $ split (Pattern "\n") response.body
    body <- HA.awaitBody
    io <- runUI Button.component words body
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
                  _ <- io.query $ H.mkTell $ Button.WindowKeyDown k
                  log k
                  pure unit
          )
      addEventListener (EventType "keydown") listener false (toEventTarget win)

-- type KeyboardEvent
--   = { key :: String }
