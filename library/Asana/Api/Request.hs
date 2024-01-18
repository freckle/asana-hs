module Asana.Api.Request
  ( AsanaAccessKey (..),
    HasAsanaAccessKey (..),
    Single (..),
    Page (..),
    NextPage (..),
    ApiData (..),
    getAll,
    getAllParams,
    getSingle,
    put,
    post,
    maxRequests,
  )
where

import           Asana.Api.Prelude
import           Data.Aeson
import           Data.Aeson.Casing        (aesonPrefix, snakeCase)
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T
import           Network.HTTP.Simple      (JSONException (JSONConversionException, JSONParseException),
                                           Request, Response, addRequestHeader,
                                           getResponseBody, getResponseHeader,
                                           getResponseStatusCode, httpJSON,
                                           parseRequest_, setRequestBodyJSON,
                                           setRequestMethod)
import           UnliftIO.Concurrent      (threadDelay)

newtype AsanaAccessKey = AsanaAccessKey
  { unAsanaAccessKey :: Text
  }

class HasAsanaAccessKey env where
  asanaAccessKeyL :: Lens' env AsanaAccessKey

instance HasAsanaAccessKey AsanaAccessKey where
  asanaAccessKeyL = id

maxRequests :: Int
maxRequests = 50

-- | Type for a single-resource response, containing @{ data: { ... } }@
newtype Single a = Single
  { sData :: a
  }
  deriving newtype (Eq, Show)
  deriving stock (Generic)

instance (FromJSON a) => FromJSON (Single a) where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- | Type for a list-resource response, containing @{ data: [{ ... }] }@
data Page a = Page
  { pData     :: [a],
    pNextPage :: Maybe NextPage
  }
  deriving stock (Eq, Generic, Show)

instance (FromJSON a) => FromJSON (Page a) where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- | The @next_page@ element of a paginated response
data NextPage = NextPage
  { npOffset :: Text,
    npPath   :: Text,
    npUri    :: Text
  }
  deriving stock (Eq, Generic, Show)

instance FromJSON NextPage where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

-- | Generic type for un/wrapping an item as @{ data: <item> }@
newtype ApiData a = ApiData
  { adData :: a
  }
  deriving newtype (Show, Eq)
  deriving stock (Generic)

instance (FromJSON a) => FromJSON (ApiData a) where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance (ToJSON a) => ToJSON (ApiData a) where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

-- | Naively GET all pages of a paginated resource
getAll ::
  ( MonadUnliftIO m,
    MonadLogger m,
    MonadReader env m,
    HasAsanaAccessKey env,
    FromJSON a
  ) =>
  String ->
  m [a]
getAll path = getAllParams path []

getAllParams ::
  ( MonadUnliftIO m,
    MonadLogger m,
    MonadReader env m,
    HasAsanaAccessKey env,
    FromJSON a
  ) =>
  String ->
  [(String, String)] ->
  m [a]
getAllParams path params = go Nothing
  where
    go mOffset = do
      Page d mNextPage <- get path params 50 mOffset

      maybe (pure d) (fmap (d ++) . go . Just . T.unpack . npOffset) mNextPage

-- | Get a single resource
getSingle ::
  ( MonadUnliftIO m,
    MonadLogger m,
    MonadReader env m,
    HasAsanaAccessKey env,
    FromJSON a
  ) =>
  String ->
  m a
getSingle path = sData <$> get path [] 1 Nothing

get ::
  ( MonadUnliftIO m,
    MonadLogger m,
    MonadReader env m,
    HasAsanaAccessKey env,
    FromJSON a
  ) =>
  String ->
  [(String, String)] ->
  Int ->
  Maybe String ->
  m a
get path params limit mOffset = do
  AsanaAccessKey key <- view asanaAccessKeyL
  let request =
        parseRequest_ $
          "https://app.asana.com/api/1.0"
            <> path
            <> "?limit="
            <> show limit -- Ignored on not paging responses
            <> maybe "" ("&offset=" <>) mOffset
            <> concatMap (\(k, v) -> "&" <> k <> "=" <> v) params
  response <- retry 50 $ httpJSON (addAuthorization key request)
  when (300 <= getResponseStatusCode response) $
    logWarnNS "Asana" $
      "GET failed, status: "
        <> pack (show $ getResponseStatusCode response)
  pure $ getResponseBody response

put ::
  ( MonadUnliftIO m,
    MonadLogger m,
    MonadReader env m,
    HasAsanaAccessKey env,
    ToJSON a
  ) =>
  String ->
  a ->
  m Value
put = httpAction "PUT"

post ::
  ( MonadUnliftIO m,
    MonadLogger m,
    MonadReader env m,
    HasAsanaAccessKey env,
    ToJSON a
  ) =>
  String ->
  a ->
  m Value
post = httpAction "POST"

httpAction ::
  ( MonadUnliftIO m,
    MonadLogger m,
    MonadReader env m,
    HasAsanaAccessKey env,
    ToJSON a
  ) =>
  ByteString ->
  String ->
  a ->
  m Value
httpAction verb path payload = do
  AsanaAccessKey key <- view asanaAccessKeyL
  let request = parseRequest_ $ "https://app.asana.com/api/1.0" <> path

  response <-
    retry 10 $
      httpJSON
        ( setRequestMethod verb . setRequestBodyJSON payload $
            addAuthorization
              key
              request
        )
  when (300 <= getResponseStatusCode response) $
    logWarnNS "Asana" $
      mconcat
        [ "Request failed",
          "\n  method: " <> T.decodeUtf8 verb,
          "\n  status: " <> pack (show $ getResponseStatusCode response),
          "\n  body  : "
            <> T.decodeUtf8
              (BSL.toStrict $ encode $ toJSON $ getResponseBody @Value response)
        ]

  pure $ getResponseBody response

addAuthorization :: Text -> Request -> Request
addAuthorization key =
  addRequestHeader "Authorization" $ "Bearer " <> T.encodeUtf8 key

retry ::
  forall a m.
  (MonadUnliftIO m, MonadLogger m) =>
  Int ->
  m (Response a) ->
  m (Response a)
retry attempt go
  | attempt <= 0 = go
  | otherwise = handler =<< go `catch` handleParseError
  where
    handleParseError :: JSONException -> m (Response a)
    handleParseError e = case e of
      JSONParseException _ rsp _      -> orThrow e rsp
      JSONConversionException _ rsp _ -> orThrow e rsp

    orThrow :: (Exception e) => e -> Response b -> m (Response a)
    orThrow e response
      | getResponseStatusCode response == 429 = do
          let seconds = getResponseDelay response
          logWarnNS "Asana" $ "Retrying after " <> pack (show seconds) <> " seconds"
          threadDelay $ seconds * 1000000
          retry (pred attempt) go
      | otherwise = liftIO $ throwIO e

    handler :: Response a -> m (Response a)
    handler response
      | getResponseStatusCode response == 429 = do
          let seconds = getResponseDelay response
          logWarnNS "Asana" $ "Retrying after " <> pack (show seconds) <> " seconds"
          threadDelay $ seconds * 100000
          retry (pred attempt) go
      | otherwise = pure response

getResponseDelay :: Response a -> Int
getResponseDelay =
  fromMaybe 0
    . readMaybe
    . T.unpack
    . T.decodeUtf8With T.lenientDecode
    . mconcat
    . getResponseHeader "Retry-After"
