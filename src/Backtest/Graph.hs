module Backtest.Graph where

-- import to get compiler to check
import qualified Backtest.Graph.Vega


import Backtest.Prelude
import Lucid (Html)
import Lucid.Svg
import Lucid.Svg.Path


testGraph :: Html ()
testGraph = do
  svg_ [version_ "1.2"] $ do
    path_ [d_ (mA 10 10 <> hA 90 <> vA 90 <> hA 10 <> z), fill_ "#FF0000"]




