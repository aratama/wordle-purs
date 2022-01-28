module Main
  ( main
  ) where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class.Console (log)
import Example.Basic.Button as Button
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

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
    runUI Button.component words body
