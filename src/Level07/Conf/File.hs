{-# LANGUAGE OverloadedStrings #-}
module Level07.Conf.File where

import           Data.ByteString.Lazy            (ByteString)
import qualified Data.ByteString.Lazy as B

import           Data.Bifunctor             (first)

import           Control.Exception          (try)

import           Level07.Types              (ConfigError (..),
                                             PartialConf (..))
import Data.Aeson (eitherDecode)
-- $setup
-- >>> :set -XOverloadedStrings

-- | The configuration file is in the JSON format, so we need to use Aeson
-- to go from JSON to our 'PartialConf'.
--
-- Update these tests when you've completed this function.
--
-- >>> readConfFile "badFileName.no"
-- Left (ConfigFileReadError badFileName.no: openBinaryFile: does not exist (No such file or directory))
-- >>> readConfFile "files/test.json"
-- Right "{\"foo\":33}\n"
--
readConfFile
  :: FilePath
  -> IO (Either ConfigError ByteString)
readConfFile path =
  -- Reading a file may throw an exception for any number of
  -- reasons. Use the 'try' function from 'Control.Exception' to catch
  -- the exception and turn it into an error value that is thrown as
  -- part of our 'AppM' transformer.
  --
  -- No exceptions from reading the file should escape this function.
  --
  first ConfigFileReadError <$> try (B.readFile path)

-- | Construct the function that will take a ``FilePath``, read it in, decode it,
-- and construct our ``PartialConf``.
parseJSONConfigFile
  :: FilePath
  -> IO (Either ConfigError PartialConf)
parseJSONConfigFile path = do
  content <- readConfFile path
  pure $ first BadConfFile . eitherDecode =<< content
