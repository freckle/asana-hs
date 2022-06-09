module Asana.Api.CustomField
  ( CustomField(..)
  , CustomFields(..)
  , customEnumId
  , EnumOption(..)
  , putCustomField
  , putCustomFields
  ) where

import Asana.Api.Prelude

import Asana.Api.Gid (Gid, gidToText)
import Asana.Api.Request
import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.List (find)
import Data.Scientific (Scientific)
import Data.String (fromString)
import qualified Data.Text as T

data CustomField
  = CustomNumber Gid Text (Maybe Scientific)
  | CustomEnum Gid Text [EnumOption] (Maybe Text)
  | CustomText Gid Text (Maybe Text)
  | Other -- ^ Unexpected types dumped here
  deriving stock (Eq, Generic, Show)

newtype CustomFields = CustomFields { getCustomFields :: [CustomField] }
  deriving stock (Show, Eq)
  deriving newtype (FromJSON)

instance ToJSON CustomFields where
  toJSON (CustomFields fields) = object $ concatMap toPair fields
   where
    toPair = \case
      CustomNumber gid _ n -> [gidToKey gid .= n]
      e@(CustomEnum gid _ _ _) -> [gidToKey gid .= customEnumId e]
      _ -> []

    -- fromString will give us Text for aeson-1.x and Key for aeson-2.x
    gidToKey = fromString . T.unpack . gidToText

data EnumOption = EnumOption
  { eoGid :: Gid
  , eoName :: Text
  }
  deriving stock (Eq, Generic, Show)

instance FromJSON EnumOption where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- | Return a @'CustomField'@s value's Enum id, if possible
--
-- - Must be a @'CustomEnum'@
-- - Must have a value
-- - Must have an option with the same name as that value
--
customEnumId :: CustomField -> Maybe Gid
customEnumId (CustomEnum _ _ opts mValue) = do
  value <- mValue
  option <- find ((== value) . eoName) opts
  pure $ eoGid option
customEnumId _ = Nothing

instance FromJSON CustomField where
  parseJSON = withObject "CustomField" $ \o -> do
    oType <- o .: "type"

    case (oType :: Text) of
      "text" -> CustomText <$> o .: "gid" <*> o .: "name" <*> o .: "text_value"
      "number" ->
        CustomNumber <$> o .: "gid" <*> o .: "name" <*> o .: "number_value"
      "enum" -> do
        value <- o .: "enum_value"
        CustomEnum
          <$> (o .: "gid")
          <*> (o .: "name")
          <*> (o .: "enum_options")
          <*> case value of
                Object vo -> vo .:? "name"
                _ -> pure Nothing
      _ -> pure Other

putCustomField
  :: (MonadUnliftIO m, MonadLogger m, MonadReader env m, HasAsanaAccessKey env)
  => Gid
  -> CustomField
  -> m ()
putCustomField taskId = putCustomFields taskId . CustomFields . pure

putCustomFields
  :: (MonadUnliftIO m, MonadLogger m, MonadReader env m, HasAsanaAccessKey env)
  => Gid
  -> CustomFields
  -> m ()
putCustomFields taskId fields =
  void $ put ("/tasks/" <> T.unpack (gidToText taskId)) $ ApiData
    (object ["custom_fields" .= fields])
