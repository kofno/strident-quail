{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad ((>=>))
import Data.Aeson (eitherDecode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (Maybe, fromMaybe)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as LE
import Network.HTTP.Media (mapAcceptMedia)
import Network.HTTP.Types.Status (status415)
import Network.Wai.Handler.Warp (Port)
import QueryExchange (QueryRequest(..))
import Schema (helloJson)
import Web.Scotty
       (ActionM, ScottyM, body, file, get, header, html, liftAndCatchIO,
        param, post, raise, rescue, scotty, setHeader, status, text)

renderGraphiQL :: ActionM ()
renderGraphiQL = setHeader "Content-Type" "text/html" >> file "graphiql.html"

renderResult :: QueryRequest -> ActionM ()
renderResult query = do
  result <- liftAndCatchIO $ helloJson query
  text result

graphqlEndpoint :: QueryRequest -> ActionM ()
graphqlEndpoint query = header "Accept" >>= toResponse
  where
    toStrictUtf8 :: L.Text -> B.ByteString
    toStrictUtf8 = B.concat . BL.toChunks . LE.encodeUtf8
    notSupported :: ActionM ()
    notSupported = status status415
    acceptMedia :: B.ByteString -> Maybe (ActionM ())
    acceptMedia =
      mapAcceptMedia
        [ ("text/html", renderGraphiQL)
        , ("application/json", renderResult query)
        ]
    toAction :: Maybe L.Text -> Maybe (ActionM ())
    toAction = fmap toStrictUtf8 >=> acceptMedia
    toResponse :: Maybe L.Text -> ActionM ()
    toResponse = fromMaybe notSupported . toAction

routes :: ScottyM ()
routes = do
  get "/graphql" $ do
    payload <- param "query" `rescue` const (pure "")
    let query = QueryRequest payload Nothing Nothing
    graphqlEndpoint query
  post "/graphql" $ do
    s <- LE.decodeUtf8 <$> body
    liftAndCatchIO $ print s
    q <- eitherDecode <$> body
    either (raise . L.pack) graphqlEndpoint q

port :: Port
port = 4000

main :: IO ()
main = scotty port routes
