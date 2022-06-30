module Backtest.App where

import Backtest.Prelude
import Web.Scotty as Scotty
import Network.Wai.Middleware.Static (staticWithOptions, defaultOptions)
import Network.Wai (Application)
import Lucid (renderBS, Html)
import Lucid.Html5
import qualified Backtest.App.Counter as Counter

import Juniper
import Juniper.Web (document)



start :: IO ()
start = do

  -- load embedded js
  -- todos <- atomically $ newTVar [Todo "Test Item" False]
  let cfg = Render True toDocument

  scotty 3030 $ do
    -- middleware $ staticWithOptions defaultOptions

    page "/page" $ do
      handle cfg Counter.page

    -- -- if you use "lucid" it doesn't work
    -- get "/app/about" $
    --   static $ About.view

    get "/" $ do
      html $ cs $ renderBS $ do
        h1_ "Backtest"
        li_ $ a_ [href_ "/page"] "Page"


toDocument :: Html () -> Html ()
toDocument = document "Example" $ do
  style_ "CSS HERE"
  -- link_ [type_ "text/css", rel_ "stylesheet", href_ "/example/example.css"]



