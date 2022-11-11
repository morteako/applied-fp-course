module Level04.Types.Topic (
  Topic,
  mkTopic,
  getTopic,
) where

import Data.Aeson (ToJSON)
import Data.Functor.Contravariant (contramap)
import Data.Text (Text)

import Level04.Types.Error (Error (EmptyTopic), nonEmptyText)
import GHC.Generics (Generic)
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromRow

newtype Topic = Topic Text
  deriving (Generic, Show, ToField)

instance FromRow Topic where
  fromRow = Topic <$> field
  

mkTopic ::
  Text ->
  Either Error Topic
mkTopic =
  nonEmptyText Topic EmptyTopic

getTopic ::
  Topic ->
  Text
getTopic (Topic t) =
  t

--implement ToJSON Topic
-- Se : https://hackage.haskell.org/package/aeson-2.1.1.0/docs/Data-Aeson.html

instance ToJSON Topic

-- Maybe ToRow or/and ToField?

