{-# Language RecordWildCards #-}
{-# Language ApplicativeDo #-}

module Backend.Utils
  ( BackendException(..)
  , loadConfig
  , Config(..)
  , module Network.HTTP.Types.Status
  ) where

import LCOLONQ.Prelude

import Network.HTTP.Types.Status

import qualified Toml

newtype BackendException = BackendException Text
  deriving (Show, Eq, Ord)
instance Exception BackendException

data Config = Config
  { port :: Int
  , assetPath :: FilePath
  } deriving (Show, Eq, Ord)

configCodec :: Toml.TomlCodec Config
configCodec = do
  port <- Toml.int "port" Toml..= (\a -> a.port)
  assetPath <- Toml.string "asset_path" Toml..= (\a -> a.assetPath)
  pure $ Config{..}

loadConfig :: FilePath -> IO Config
loadConfig path = Toml.decodeFileEither configCodec path >>= \case
  Left err -> throwM . BackendException $ tshow err
  Right config -> pure config
