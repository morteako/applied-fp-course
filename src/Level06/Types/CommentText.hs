module Level06.Types.CommentText (
  CommentText,
  mkCommentText,
  getCommentText,
) where

import Level06.Types.Error (
  Error (EmptyCommentText),
  nonEmptyText,
 )

import Data.Functor.Contravariant (contramap)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Database.SQLite.Simple.ToField (ToField)

newtype CommentText = CommentText Text
  deriving (Generic, Show, ToField)

mkCommentText ::
  Text ->
  Either Error CommentText
mkCommentText =
  nonEmptyText CommentText EmptyCommentText

getCommentText ::
  CommentText ->
  Text
getCommentText (CommentText t) =
  t

instance ToJSON CommentText