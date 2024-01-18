module Asana.Api.Task.Search
  ( SearchWorkspace (..),
    TaskTypeFilter (..),
    searchWorkspace,
  )
where

import Asana.Api.Gid
import Asana.Api.Named
import Asana.Api.Prelude
import Asana.Api.Request
import Asana.Api.Task
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (intercalate)
import qualified Data.Text as T

data TaskTypeFilter = TasksOnly | SubtasksOnly | AllTaskTypes

data SearchWorkspace = SearchWorkspace
  { swWorkspaceId :: Gid,
    swProjectIds :: [Gid],
    swTaskStatusFilter :: TaskStatusFilter,
    swCustomFields :: HashMap Gid Text,
    swTaskTypeFilter :: TaskTypeFilter
  }

-- | Search for tasks within a workspace
searchWorkspace ::
  (MonadUnliftIO m, MonadLogger m, MonadReader env m, HasAsanaAccessKey env) =>
  SearchWorkspace ->
  m [Named]
searchWorkspace SearchWorkspace {..} =
  getAllParams
    (T.unpack $ "/workspaces/" <> gidToText swWorkspaceId <> "/tasks/search")
    $ ( "projects.all",
        intercalate "," $ map (T.unpack . gidToText) swProjectIds
      )
      : customFieldParams
        <> completed
        <> isSubtask
  where
    customFieldParams =
      map
        ( \(a, b) ->
            ("custom_fields." <> T.unpack (gidToText a) <> ".value", T.unpack b)
        )
        $ HashMap.toList swCustomFields

    completed = case swTaskStatusFilter of
      AllTasks -> []
      IncompletedTasks -> [("completed", "false")]

    isSubtask = case swTaskTypeFilter of
      AllTaskTypes -> []
      TasksOnly -> [("is_subtask", "false")]
      SubtasksOnly -> [("is_subtask", "true")]
