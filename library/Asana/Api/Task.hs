module Asana.Api.Task
  ( Task(..)
  , Membership(..)
  , TaskStatusFilter(..)
  , ResourceSubtype(..)
  , PostTask(..)
  , getTask
  , getProjectTasks
  , getProjectTasksCompletedSince
  , postTask
  , putCompleted
  , taskUrl
  , extractNumberField
  , extractEnumField
  ) where

import Asana.Api.Prelude

import Asana.Api.CustomField
import Asana.Api.Gid
import Asana.Api.Named
import Asana.Api.Request
import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.HashMap.Strict (HashMap)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.ISO8601 (formatISO8601)

data Membership = Membership
  { mProject :: Named
  , mSection :: Maybe Named
  }
  deriving stock (Eq, Generic, Show)

instance FromJSON Membership where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data ResourceSubtype = DefaultTask | Milestone | Section
  deriving stock (Eq, Generic, Show)

instance FromJSON ResourceSubtype where
  parseJSON =
    genericParseJSON $ defaultOptions { constructorTagModifier = snakeCase }

data Task = Task
  { tAssignee :: Maybe Named
  , tName :: Text
  , tCompleted :: Bool
  , tCompletedAt :: Maybe UTCTime
  , tCreatedAt :: UTCTime
  , tCustomFields :: CustomFields
  , tMemberships :: [Membership]
  , tGid :: Gid
  , tResourceSubtype :: ResourceSubtype
  , tNotes :: Text
  , tProjects :: [AsanaReference]
  }
  deriving stock (Eq, Generic, Show)

instance FromJSON Task where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- | Return all details for a task by id
getTask
  :: (MonadUnliftIO m, MonadLogger m, MonadReader env m, HasAsanaAccessKey env)
  => Gid
  -> m Task
getTask taskId = getSingle $ "/tasks/" <> T.unpack (gidToText taskId)

data PostTask = PostTask
  { ptProjects :: [Gid]
  , ptCustomFields :: HashMap Gid Text
  , ptName :: Text
  , ptNotes :: Text
  , ptParent :: Maybe Gid
  }
  deriving stock Generic

instance FromJSON PostTask where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON PostTask where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

-- | Create a new 'Task'
postTask
  :: (MonadUnliftIO m, MonadLogger m, MonadReader env m, HasAsanaAccessKey env)
  => PostTask
  -> m (Result Task)
postTask body = fmap adData . fromJSON <$> post "/tasks" (ApiData body)

-- | Return compact task details for a project
--
-- Iterating ourselves and returning @['Task']@ is a better interface but
-- precludes us logging things each time we request an element. So we return
-- @'Named'@ for now and let the caller use @'getTask'@ themselves.
--
getProjectTasks
  :: (MonadUnliftIO m, MonadLogger m, MonadReader env m, HasAsanaAccessKey env)
  => Gid
  -> TaskStatusFilter
  -> m [Named]
getProjectTasks projectId taskStatusFilter = do
  now <- liftIO getCurrentTime
  getAllParams
    (T.unpack $ "/projects/" <> gidToText projectId <> "/tasks")
    (completedSince now)

 where
  completedSince now = case taskStatusFilter of
    AllTasks -> []
    IncompletedTasks -> [("completed_since", formatISO8601 now)]

data TaskStatusFilter = IncompletedTasks | AllTasks

getProjectTasksCompletedSince
  :: (MonadUnliftIO m, MonadLogger m, MonadReader env m, HasAsanaAccessKey env)
  => Gid
  -> UTCTime
  -> m [Named]
getProjectTasksCompletedSince projectId since = getAllParams
  (T.unpack $ "/projects/" <> gidToText projectId <> "/tasks")
  [("completed_since", formatISO8601 since)]

putCompleted
  :: (MonadUnliftIO m, MonadLogger m, MonadReader env m, HasAsanaAccessKey env)
  => Gid
  -> Bool
  -> m ()
putCompleted taskId completed =
  void $ put ("/tasks/" <> T.unpack (gidToText taskId)) $ ApiData
    (object ["completed" .= completed])

taskUrl :: Task -> Text
taskUrl Task {..} = "https://app.asana.com/0/0/" <> gidToText tGid <> "/f"

extractNumberField :: Text -> Task -> Maybe CustomField
extractNumberField fieldName Task {..} =
  listToMaybe $ flip mapMaybe (getCustomFields tCustomFields) $ \case
    customField@(CustomNumber _ t _) -> customField <$ guard (t == fieldName)
    _ -> Nothing

extractEnumField :: Text -> Task -> Maybe CustomField
extractEnumField fieldName Task {..} =
  listToMaybe $ flip mapMaybe (getCustomFields tCustomFields) $ \case
    customField@(CustomEnum _ t _ _) ->
      if t == fieldName then Just customField else Nothing
    _ -> Nothing
