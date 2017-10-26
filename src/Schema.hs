{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Schema where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LE
import GraphQL (Response, interpretQuery)
import GraphQL.API ((:>), Argument, Field, Object)
import GraphQL.Resolver (Handler)
import QueryExchange

type Hello = Object "Hello" '[] '[ Argument "who" Text :> Field "greeting" Text]

hello :: Handler IO Hello
hello = pure greeting
  where
    greeting :: Text -> IO Text
    greeting who = pure ("Hello " <> who <> "!")

helloQuery :: QueryRequest -> IO Response
helloQuery (QueryRequest q name v) =
  let query = LT.toStrict q
      variables = fromMaybe Map.empty v
  in interpretQuery @Hello hello query name variables

helloJson :: QueryRequest -> IO LT.Text
helloJson = fmap asJson . helloQuery
  where
    asJson = LE.decodeUtf8 . encode
