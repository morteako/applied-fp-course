{-# LANGUAGE OverloadedStrings #-}

module Level06.Core (
  runApplication,
  prepareAppReqs,
  app,
) where

import qualified Control.Exception as Ex
import Control.Monad.IO.Class (liftIO)

import Network.Wai (
  Application,
  Request,
  Response,
  pathInfo,
  requestMethod,
  responseLBS,
  strictRequestBody,
 )
import Network.Wai.Handler.Warp (run)

import Network.HTTP.Types (
  Status,
  hContentType,
  status200,
  status400,
  status404,
  status500,
 )

import qualified Data.ByteString.Lazy.Char8 as LBS

import Data.Either (
  Either (Left, Right),
  either,
 )

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy.Encoding (encodeUtf8)

import Common.SQLite.Error (SQLiteResponse)

import qualified Data.Aeson as Aeson
import Data.Aeson (encode)
import qualified Data.Aeson.Encoding as Aeson
import Data.Aeson.Types (ToJSON)

import           Level06.AppM                       (App, AppM (..), liftEither, runApp)
import qualified Level06.Conf                       as Conf
import qualified Level06.DB                         as DB
import           Level06.Types                      (Conf, ConfigError(..),
                                                     ContentType (..),
                                                     Error (..),
                                                     RqType (AddRq, ListRq, ViewRq),
                                                     mkCommentText, mkTopic,
                                                     renderContentType, dbPath, getDBFilePath, confPortToWai)
import Data.Bifunctor (first)
import System.IO (hPrint, stderr)
import System.Exit (exitWith, ExitCode (..))
import Control.Monad ((<=<))


-- Our start-up is becoming more complicated and could fail in new and
-- interesting ways. But we also want to be able to capture these errors in a
-- single type so that we can deal with the entire start-up process as a whole.
data StartUpError
  = DBInitErr SQLiteResponse
  | ConfErr ConfigError
  deriving (Show)

runApplication :: IO ()
runApplication = do
  -- Load our configuration
  cfgE <- runAppM prepareAppReqs
  -- Loading the configuration can fail, so we have to take that into account now.
  case cfgE of
    Left (DBInitErr sqlErr) -> do
      -- We can't run our app at all! Display the message and exit the application.
      hPrint stderr sqlErr
      exitWith $ ExitFailure 1
    Left (ConfErr cfgErr) -> do
      hPrint stderr $ getCfgError cfgErr
      exitWith $ ExitFailure 1

    Right (cfg, db) ->
      -- We have a valid config! We can now complete the various pieces needed to run our
      -- application. This function 'finally' will execute the first 'IO a', and then, even in the
      -- case of that value throwing an exception, execute the second 'IO b'. We do this to ensure
      -- that our DB connection will always be closed when the application finishes, or crashes.
      Ex.finally (run (confPortToWai cfg) $ app cfg db) (DB.closeDB db)

getCfgError :: ConfigError -> String
getCfgError err = case err of
  BadConfFile str -> "Bad config file: " <> str
  MissingPortConf -> "Missing port configuration"
  MissingDbFileConf -> "Missing dbFile configuration"
-- | We need to complete the following steps to prepare our app requirements:
--
-- 1) Load the configuration.
-- 2) Attempt to initialise the database.
-- 3) Combine the results into a tuple
--
-- The file path for our application config is: "files/appconfig.json"
--
-- The config loading process is starting to become unweildly. We will re-use
-- our generalised AppM to also remove the problem of handling errors on start
-- up!
--
prepareAppReqs :: AppM StartUpError (Conf, DB.FirstAppDB)
prepareAppReqs = do
  conf <- first ConfErr $ Conf.parseOptions "files/appconfig.json"
  db <- first DBInitErr $ liftEither <=< liftIO $ DB.initDB $ getDBFilePath . dbPath $ conf
  pure (conf, db)
  

-- | Some helper functions to make our lives a little more DRY.
mkResponse ::
  Status ->
  ContentType ->
  LBS.ByteString ->
  Response
mkResponse sts ct =
  responseLBS sts [(hContentType, renderContentType ct)]

resp200 ::
  ContentType ->
  LBS.ByteString ->
  Response
resp200 =
  mkResponse status200

resp404 ::
  ContentType ->
  LBS.ByteString ->
  Response
resp404 =
  mkResponse status404

resp400 ::
  ContentType ->
  LBS.ByteString ->
  Response
resp400 =
  mkResponse status400

-- Some new helpers for different statuses and content types
resp500 ::
  ContentType ->
  LBS.ByteString ->
  Response
resp500 =
  mkResponse status500

resp200Json ::
  ToJSON a =>
  a ->
  Response
resp200Json =
  mkResponse status200 JSON . encode

-- | Now that we have our configuration, pass it where it needs to go.
app
  :: Conf
  -> DB.FirstAppDB
  -> Application
app _cfg db rq cb =
  runApp (handleRequest db =<< mkRequest rq) >>= cb . handleRespErr
  where
    handleRespErr :: Either Error Response -> Response
    handleRespErr = either mkErrorResponse id

handleRequest ::
  DB.FirstAppDB ->
  RqType ->
  App Response
handleRequest db rqType = case rqType of
  AddRq topic comment -> resp200 PlainText "Success" <$ DB.addCommentToTopic db topic comment
  ViewRq topic -> resp200Json <$> DB.getComments db topic
  ListRq -> resp200Json <$> DB.getTopics db

mkRequest ::
  Request ->
  App RqType
mkRequest rq =
  liftEither =<< case (pathInfo rq, requestMethod rq) of
    -- Commenting on a given topic
    ([t, "add"], "POST") -> liftIO $ mkAddRequest t <$> strictRequestBody rq
    -- View the comments on a given topic
    ([t, "view"], "GET") -> pure (mkViewRequest t)
    -- List the current topics
    (["list"], "GET") -> pure mkListRequest
    -- Finally we don't care about any other requests so build an Error response
    _ -> pure (Left UnknownRoute)

mkAddRequest ::
  Text ->
  LBS.ByteString ->
  Either Error RqType
mkAddRequest ti c =
  AddRq
    <$> mkTopic ti
    <*> (mkCommentText . decodeUtf8 . LBS.toStrict) c

mkViewRequest ::
  Text ->
  Either Error RqType
mkViewRequest =
  fmap ViewRq . mkTopic

mkListRequest ::
  Either Error RqType
mkListRequest =
  Right ListRq

mkErrorResponse ::
  Error ->
  Response
mkErrorResponse UnknownRoute =
  resp404 PlainText "Unknown Route"
mkErrorResponse EmptyCommentText =
  resp400 PlainText "Empty Comment"
mkErrorResponse EmptyTopic =
  resp400 PlainText "Empty Topic"
mkErrorResponse (SqlError _) =
  resp500 PlainText "Database error"
