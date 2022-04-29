{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use camelCase" #-}
module Invest.Lib where

import Invest.Prelude
import Invest.Types
import Data.Csv as Csv (decodeByName, Header)
import Data.ByteString.Lazy (readFile)
import Data.Vector (Vector)

test :: IO ()
test = do
    rs <- loadReturns
    mapM_ print rs

loadReturns :: IO (Vector Returns)
loadReturns = do
    putStrLn "Loading"
    f <- readFile "data/data.csv"
    Right (_, rs) <- pure $ Csv.decodeByName f
    pure rs
