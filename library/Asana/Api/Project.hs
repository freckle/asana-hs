module Asana.Api.Project
  ( Project (..),
  )
where

import Asana.Api.Gid (Gid)
import Asana.Api.Prelude
import Data.Aeson (FromJSON, genericParseJSON, parseJSON)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Time (UTCTime)

data Project = Project
  { pGid :: Gid,
    pName :: Text,
    pCreatedAt :: UTCTime
  }
  deriving stock (Generic, Show)

instance FromJSON Project where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
