module Level06.Conf.File where

import           Data.ByteString.Lazy            (ByteString)
import qualified Data.ByteString.Lazy as B

import           Data.Text                  (Text, pack)

import           Data.Bifunctor             (first)
import           Data.Monoid                (Last (Last))

import           Control.Exception          (try)

import qualified Data.Attoparsec.ByteString as AB

import           Level06.AppM               (AppM, runAppM, liftEither)
import           Level06.Types              (ConfigError (BadConfFile),
                                             PartialConf (PartialConf))
import Control.Exception.Base (IOException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
-- $setup
-- >>> :set -XOverloadedStrings

-- | The configuration file is in the JSON format, so we need to use Aeson
-- to go from JSON to our 'PartialConf'.
--
-- Update these tests when you've completed this function.
--
-- >>> runAppM $ readConfFile "badFileName.no"
-- WAS Left (<YourErrorConstructorHere> "badFileName.no: openBinaryFile: does not exist (No such file or directory)")
-- NOW Left (BadConfFile "badFileName.no: openBinaryFile: does not exist (No such file or directory)")
-- >>> runAppM $ readConfFile "files/test.json"
-- WAS Right "{\n  \"foo\": 33\n}\n"
-- NOW Right "{\"foo\":33}\n"
--
readConfFile
  :: FilePath
  -> AppM ConfigError ByteString
readConfFile path = do
  -- Reading a file may throw an exception for any number of
  -- reasons. Use the 'try' function from 'Control.Exception' to catch
  -- the exception and turn it into an error value that is thrown as
  -- part of our 'AppM' transformer.
  --
  -- No exceptions from reading the file should escape this function.
  --
  fileContent <- liftIO $ first (BadConfFile . show) <$> (try @IOException $ B.readFile path)
  liftEither fileContent

-- | Construct the function that will take a ``FilePath``, read it in, decode it,
-- and construct our ``PartialConf``.
parseJSONConfigFile
  :: FilePath
  -> AppM ConfigError PartialConf
parseJSONConfigFile path = do
  content <- readConfFile path
  liftEither $ first BadConfFile $ eitherDecode content

-- Go to 'src/Level06/Conf.hs' next.
