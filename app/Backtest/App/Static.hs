module Backtest.App.Static where

import Backtest.Prelude hiding (Text)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Web.Scotty.Trans (ActionT, ScottyT, setHeader, raw, get, text, file)
-- import Web.UI as UI
-- import Web.Example as UI
import Tailwind hiding (static)
import qualified Data.Text.Lazy as Lazy
import qualified Juniper.JS as Juniper




files :: MonadIO m => ScottyT Lazy.Text m ()
files = do
  -- get "/ui.css" $ do
  --   setHeader "Content-Type" "text/css"
  --   text $ cs (UI.stylesheet (range :: [AppColor]) (range :: [Space]))
  -- get "/modern-normalize.css" $ static ".css" normalize

  get "/app.js"  $ static ".js" scripts
  get "/app.css" $ dynamic ".css" "static/output.css"

  -- get "/icons.svg" $ static ".svg" icons
  -- get "/images.css" $ do
  --   css_ <- generateImagesCss
  --   setHeader "Content-Type" "text/css"
  --   text css_


  where
    -- normalize = $(embedFile "static/modern-normalize.css")

    css = $(embedFile "static/output.css")
    -- icons = $(embedFile "static/icons.svg")

    scripts = Juniper.scripts <> "\n" <> bundle
    bundle = $(embedFile "static/bundle.js")




dynamic :: MonadIO m => Extension -> FilePath -> ActionT Lazy.Text m ()
dynamic ext fp = do
  setHeader "Content-Type" $ contentType ext
  file fp

static :: MonadIO m => Extension -> ByteString -> ActionT Lazy.Text m ()
static ext cnt = do
  setHeader "Content-Type" $ contentType ext
  raw (cs cnt)


type Extension = String
contentType :: Extension -> Lazy.Text
contentType ".png" = "image/png"
contentType ".js" = "text/javascript"
contentType ".css" = "text/css"
contentType ".html" = "text/html"
contentType ".svg" = "image/svg+xml"
contentType _ = "text/plain"
