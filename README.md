# Asana

Haskell client for the Asana API.

## API Key

1. Visit *Settings > Apps > Developer apps*, create a Personal Access Token

## Simple Usage

For example, to make a quick script to list all incomplete tasks in a project:

```hs
import Asana.Api.Request
import Asana.Api.Task
import Data.Text (pack)
import System.Environment (getEnv)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (runReaderT)

main :: IO ()
main = do
  key <- AsanaAccessKey . pack <$> getEnv "ASANA_ACCESS_KEY"

  runStdoutLoggingT $ flip runReaderT key $ do
    let projectId = "..."
    tasks <- getProjectTasks projectId IncompletedTasks
    print tasks
```

## Advanced Usage

This library implements the `Has`-class pattern for use in a `ReaderT`-style
application.

```hs
data App = App
  { -- ...
  , appAsanaAccessKey :: ApiKey
  }

instance HasAsanaAccessKey App where
  asanaAccessKeyL = lens appAsanaAccessKey $ \x y -> x { appAsanaAccessKey = y }

loadApp :: IO App
loadApp = undefined

main :: IO ()
main = do
  app <- loadApp
  runStdoutLoggingT $ runReaderT run app

run :: (MonadLogger m, MonadReader env m, HasAsanaAccessKey env) => m ()
run = undefined
```

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
