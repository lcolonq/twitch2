module Main where

import LCOLONQ.Prelude

import Options.Applicative

import Backend
import Backend.Utils

newtype Opts = Opts
  { config :: FilePath
  }

parseOpts :: Parser Opts
parseOpts = Opts
  <$> strOption (long "config" <> metavar "PATH" <> help "Path to config file" <> showDefault <> value "backend.toml")

main :: IO ()
main = do
  opts <- execParser $ info (parseOpts <**> helper)
    ( fullDesc
    <> header "backend - web app backend server"
    )
  cfg <- loadConfig opts.config
  server cfg
