{-# LANGUAGE OverloadedStrings #-}
module Level07.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks)

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection)
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level07.AppM                      (App, Env (envDB) )
import           Level07.Types                     (Comment, CommentText,
                                                     DBFilePath (getDBFilePath),
                                                     Error (DBError),
                                                     FirstAppDB (FirstAppDB, dbConn),
                                                     Topic, fromDBComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)
import Level07.DB.Types (DBComment)
import Control.Monad.Error.Class (liftEither)

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: DBFilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open ( getDBFilePath fp )
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

getDBConn
  :: App Connection
getDBConn = asks $ dbConn . envDB

runDB
  :: (a -> Either Error b)
  -> (Connection -> IO a)
  -> App b
runDB f query = do
  conn <- getDBConn
  res <- liftIO $ Sql.runDBAction $ query conn
  liftEither $ either (Left . DBError) f res

getComments
  :: Topic
  -> App [Comment]
getComments topic =
  let
    sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
    query conn = Sql.query conn sql (Sql.Only topic)
  in
    runDB (traverse fromDBComment) query

addCommentToTopic
  :: Topic
  -> CommentText
  -> App ()
addCommentToTopic topic comment =
  let
    sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
    query conn = getCurrentTime >>= \time -> Sql.execute conn sql (topic, comment, time)
  in
    runDB Right query

getTopics
  :: App [Topic]
getTopics =
  let
    sql = "SELECT DISTINCT topic FROM comments"
    query conn = Sql.query_ conn sql :: IO [Topic]
  in
    runDB Right query

deleteTopic
  :: Topic
  -> App ()
deleteTopic topic =
  let
    sql = "DELETE FROM comments WHERE topic = ?"
    query conn = Sql.execute conn sql (Sql.Only topic)
  in do
    runDB Right query

-- Go on to 'src/Level07/Core.hs' next.