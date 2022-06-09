-- | Anything with a compact @{ id, name }@ representation
module Asana.Api.Named
  ( Named(..)
  ) where

import Asana.Api.Prelude

import Asana.Api.Gid (Gid)
import Data.Aeson (FromJSON, genericParseJSON, parseJSON)
import Data.Aeson.Casing (aesonPrefix, snakeCase)

data Named = Named
  { nGid :: Gid
  , nName :: Text
  }
  deriving stock (Eq, Generic, Show)

instance FromJSON Named where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
