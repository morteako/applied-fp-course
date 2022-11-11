{-# LANGUAGE OverloadedStrings #-}

module Level05.Core (
  runApp,
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

import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy.Encoding (encodeUtf8)

import Common.SQLite.Error (SQLiteResponse)

import qualified Data.Aeson as Aeson

import Data.Aeson (encode)
import qualified Data.Aeson.Encoding as Aeson
import Data.Aeson.Types (ToJSON)
import Data.Bifunctor ( Bifunctor(first) )
import Level05.Conf (Conf (Conf, dbFilePath), firstAppConfig)
import Level05.DB ( initDB )
import qualified Level05.DB as DB
import Level05.Types (
  ContentType (JSON, PlainText),
  Error (..),
  RqType (AddRq, ListRq, ViewRq),
  mkCommentText,
  mkTopic,
  renderContentType,
 )

import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (stderr, hPrint)
import Level05.AppM (AppM, liftEither, runAppM)
import Data.Function ((&))

-- Our start-up is becoming more complicated and could fail in new and
-- interesting ways. But we also want to be able to capture these errors in a
-- single type so that we can deal with the entire start-up process as a whole.
data StartUpError
  = DBInitErr SQLiteResponse
  deriving (Show)

runApp :: IO ()
runApp = do
  -- Load our configuration
  cfgE <- prepareAppReqs firstAppConfig
  -- Loading the configuration can fail, so we have to take that into account now.
  case cfgE of
    Left (DBInitErr sqlErr) -> do
      -- We can't run our app at all! Display the message and exit the application.
      hPrint stderr sqlErr
      exitWith $ ExitFailure 1
    Right cfg ->
      -- We have a valid config! We can now complete the various pieces needed to run our
      -- application. This function 'finally' will execute the first 'IO a', and then, even in the
      -- case of that value throwing an exception, execute the second 'IO b'. We do this to ensure
      -- that our DB connection will always be closed when the application finishes, or crashes.
      Ex.finally (run 3000 $ app cfg) (DB.closeDB cfg)

-- We need to complete the following steps to prepare our app requirements:
--
-- 1) Load the configuration.
-- 2) Attempt to initialise the database.
--
-- Our application configuration is defined in Conf.hs
--
prepareAppReqs ::
  Conf -> IO (Either StartUpError DB.FirstAppDB)
prepareAppReqs conf =
  first DBInitErr <$> initDB conf.dbFilePath


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

-- |
-- How has this implementation changed, now that we have an AppM to handle the
-- errors for our application? Could it be simplified? Can it be changed at all?
app
  :: DB.FirstAppDB
  -> Application
app db rq cb = mkRequest rq >>= handleRequest db & runAppM >>= cb . either mkErrorResponse id

handleRequest ::
  DB.FirstAppDB ->
  RqType ->
  AppM Response
handleRequest db rqType = case rqType of
  AddRq topic comment -> resp200 PlainText "Success" <$ DB.addCommentToTopic db topic comment
  ViewRq topic -> resp200Json <$> DB.getComments db topic
  ListRq -> resp200Json <$> DB.getTopics db

mkRequest ::
  Request ->
  AppM RqType
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
mkErrorResponse (SqlError err) =
  resp500 PlainText $ LBS.pack $ "SQL Error: " ++ show err
