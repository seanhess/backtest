{-# LANGUAGE DeriveAnyClass #-}
module Backtest.App where

import Backtest.Prelude
import Backtest.History (loadReturns, toHistories)
import Web.Scotty as Scotty hiding (text, html)
import qualified Web.Scotty as Scotty
import Network.Wai.Middleware.Static (staticWithOptions, defaultOptions)
import Network.Wai (Application)
import Lucid (renderBS, renderText, Html)
import Lucid.Html5
import qualified Backtest.App.Results as Results
import qualified Backtest.App.Static as Static
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import Backtest.LayoutTest (layoutTest)

import Juniper
import Juniper.Web (document)
-- import Tailwind.Color
-- import Tailwind

-- import Web.UI.Generate (stylesheet, range)
-- import Web.UI (AppColor)
-- import Web.UI.Attributes (Space)

-- writeStylesheet :: IO ()
-- writeStylesheet = do
--   Text.writeFile "static/ui.css" $ stylesheet (range :: [AppColor]) (range :: [Space])

-- for this to work, I would need 

start :: IO ()
start = do


  -- load embedded js
  -- todos <- atomically $ newTVar [Todo "Test Item" False]
  let cfg = Render False toDocument

  scotty 3030 $ do
    -- middleware $ staticWithOptions defaultOptions

    page "/results" $ do
      handle cfg Results.page

    get "/layout" $ do
      Scotty.html $ renderText $ toDocument $ layoutTest

    -- -- if you use "lucid" it doesn't work
    -- get "/app/about" $
    --   static $ About.view

    get "/" $ do
      Scotty.html $ cs $ renderBS $ do
        h1_ "Backtest"
        li_ $ a_ [href_ "/results"] "Results"

    Static.files


-- allClasses :: Text
-- allClasses = Text.unlines $ mconcat
--   [ map bg :: 

--   ]


toDocument :: Html () -> Html ()
toDocument cnt = do
  html_ $ do
    head_ $ do
      title_ "Magic Withdrawals, as seen on TV"
      meta_ [charset_ "UTF-8"]
      meta_ [httpEquiv_ "Content-Type", content_ "text/html", charset_ "UTF-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0" ]

      -- Googole Fonts
      link_ [rel_ "preconnect", href_ "https://fonts.googleapis.com"]
      link_ [rel_ "preconnect", href_ "https://fonts.gstatic.com", crossorigin_ ""]
      -- link_ [rel_ "stylesheet", href_ "https://fonts.googleapis.com/css2?family=Archivo+Narrow:wght@400;500;600&family=Inter:wght@400;500;700&display=swap"]

      -- link_ [type_ "text/css", rel_ "stylesheet", href_ "/modern-normalize.css"]
      link_ [type_ "text/css", rel_ "stylesheet", href_ "/app.css"]
      -- link_ [type_ "text/css", rel_ "stylesheet", href_ "/finalfive.css"]


    body_ $ do
      cnt

      script_ [type_ "text/javascript", src_ "/app.js"] ("" :: Text)
      -- link_ [type_ "text/css", rel_ "stylesheet", href_ "/images.css"]

  -- link_ [type_ "text/css", rel_ "stylesheet", href_ "/example/example.css"]



