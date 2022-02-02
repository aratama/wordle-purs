module Wordle.Fetch (fetch, FetchError) where

import Web.Fetch.Response (Response)
import Prelude
import Control.Promise as Promise
import Data.Either (Either(..))
import Effect.Class (liftEffect)
import Unsafe.Coerce (unsafeCoerce)
import Web.Fetch as Fetch
import Web.Fetch.Request as FR
import Web.Promise as WebPromise
import Effect.Aff (Aff)
import Web.Fetch.Response as Res

promiseToPromise :: forall a. WebPromise.Promise a -> Promise.Promise a
promiseToPromise = unsafeCoerce

type FetchError
  = String

fetch :: String -> Aff (Either FetchError String)
fetch url = do
  req <- liftEffect $ FR.new url FR.defaultOptions
  promise :: WebPromise.Promise Response <- liftEffect $ Fetch.fetch req
  result :: Response <- Promise.toAff $ (promiseToPromise promise)
  if Res.ok result then do
    promiseString <- liftEffect (Res.text result)
    Right <$> Promise.toAff (promiseToPromise promiseString)
  else
    pure $ Left $ Res.statusText result
