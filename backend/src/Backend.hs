module Backend where

import LCOLONQ.Prelude

import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Static as Wai.Static
import qualified Network.Wai.Handler.Warp as Warp

import qualified Web.Scotty as S

import qualified Data.Text.Lazy as Text.Lazy

import Backend.Utils

server :: Config -> IO ()
server cfg = do
  log $ "Server running on port " <> tshow cfg.port
  Warp.run cfg.port =<< app cfg

app :: Config -> IO Wai.Application
app cfg = S.scottyApp do
  S.middleware . Wai.Static.staticPolicy $ Wai.Static.addBase cfg.assetPath
  S.get "/echo/:name" do
    name <- S.pathParam "name"
    log name
    S.text $ Text.Lazy.fromStrict name
