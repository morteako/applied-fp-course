{-# LANGUAGE OverloadedStrings          #-}

module Level05.Types
  ( Error (..)
  , RqType (..)
  , ContentType (..)
  , Topic
  , CommentText
  , CommentId(..)
  , Comment (..)
  , mkTopic
  , getTopic
  , mkCommentText
  , getCommentText
  , renderContentType
  , fromDBComment
  ) where

import           GHC.Generics               (Generic)

import           Data.ByteString            (ByteString)
import           Data.Text                  (Text, pack)

import           Data.List                  (stripPrefix)
import           Data.Maybe                 (fromMaybe)

import           Data.Functor.Contravariant ((>$<))

import           Data.Time                  (UTCTime)
import qualified Data.Time.Format           as TF



import           Level05.DB.Types

-- Notice how we've moved these types into their own modules. It's cheap and
-- easy to add modules to carve out components in a Haskell application. So
-- whenever you think that a module is too big, covers more than one piece of
-- distinct functionality, or you want to carve out a particular piece of code,
-- just spin up another module.
import           Level05.Types.CommentText  (CommentText, getCommentText,
                                             mkCommentText)
import           Level05.Types.Topic        (Topic, getTopic, mkTopic)

import           Level05.Types.Error        (Error (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import Data.Aeson (ToJSON)

newtype CommentId = CommentId Int
  deriving (Generic, Eq, Show)

instance ToJSON CommentId

-- | This is the `Comment` record that we will be sending to users, it's a
-- straightforward record type, containing an `Int`, `Topic`, `CommentText`, and
-- `UTCTime`.
data Comment = Comment
  { id    :: CommentId
  , topic :: Topic
  , body  :: CommentText
  , time  :: UTCTime
  }
  deriving (Generic, Show)

instance ToJSON Comment

-- Implement Aeson.ToJSON for Comment

-- | For safety we take our stored `DBComment` and try to construct a `Comment`
-- that we would be okay with showing someone. However unlikely it may be, this
-- is a nice method for separating out the back and front end of a web app and
-- providing greater guarantees about data cleanliness.
fromDBComment
  :: DBComment
  -> Either Error Comment
fromDBComment dbc =
  let
    commentId = CommentId dbc.id
    topic = mkTopic dbc.topic
    body = mkCommentText dbc.body
    time = dbc.time
  in
    Comment commentId <$> topic <*> body <*> pure time

data RqType
  = AddRq Topic CommentText
  | ViewRq Topic
  | ListRq

data ContentType
  = PlainText
  | JSON

renderContentType
  :: ContentType
  -> ByteString
renderContentType PlainText = "text/plain"
renderContentType JSON      = "application/json"

-- | Move on to ``src/Level05/DB.hs`` next.
