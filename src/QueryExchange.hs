{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module QueryExchange where

import Data.Aeson (FromJSON, (.:), (.:?), parseJSON, withObject)
import Data.Aeson.Types
       (Parser, Value(..), explicitParseFieldMaybe)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text.Lazy (Text)
import GHC.Generics
import GraphQL (VariableValues)
import GraphQL.Internal.Syntax.AST (Name, NameError(..), makeName)

data QueryRequest = QueryRequest
  { query :: Text
  , name :: Maybe Name
  , variables :: Maybe VariableValues
  } deriving (Show, Generic)

instance FromJSON QueryRequest where
  parseJSON =
    withObject "QueryRequest" $ \v -> do
      query <- v .: "query"
      name <- explicitParseFieldMaybe parseName v "operationName"
      variables <- explicitParseFieldMaybe parseVariables v "variables"
      return QueryRequest {..}

parseName :: Value -> Parser Name
parseName (String n) =
  let toErrStr (NameError t) = T.unpack t
      handleFail = fail . toErrStr
  in either handleFail return $ makeName n
parseName _ = fail "could not parse. GraphQL names must be Strings."

parseVariables :: Value -> Parser VariableValues
parseVariables _ = fail "TODO: Fix variable parsing to work on stuff"
