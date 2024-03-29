module Asana.Api.Tag
  ( Tag (..),
  )
where

import Asana.Api.Gid
import Asana.Api.Prelude
import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)

data Tag = Tag
  { tGid :: Gid,
    tName :: Text
  }
  deriving stock (Eq, Generic, Show)

instance FromJSON Tag where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
