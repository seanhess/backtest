{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use camelCase" #-}
module Invest.Lib where

import Invest.Prelude
import Invest.Types
import Data.Csv as Csv (decodeByName, Header)
import Data.ByteString.Lazy (readFile)
import Data.Vector as Vector (Vector, toList)

test :: IO ()
test = do
    rs <- loadReturns
    simulation million (Vector.toList rs)
    -- mapM_ print rs

loadReturns :: IO (Vector Returns)
loadReturns = do
    putStrLn "Loading"
    f <- readFile "data/data.csv"
    Right (_, rs) <- pure $ Csv.decodeByName f
    pure rs

simulation :: USD Balance -> [Returns] -> IO ()
simulation balance [] = pure ()
simulation balance (r:_) = do
    let wdr = Pct 4 :: Pct Withdrawal
    let bal' = calculateYear r wdr balance
    print bal'

    pure ()
    
  where

    calculateYear :: Returns -> Pct Withdrawal -> USD Balance -> YearResult
    calculateYear returns wdr bal = 
        let withdrawal = portion wdr bal
            growth     = portion returns.totalStock bal
            end        = bal & grow growth & withdraw withdrawal
            start      = bal
        in YearResult {..}

    portion :: Pct p -> USD a -> USD p
    portion (Pct p) (USD d) = USD $ round $ (p/100) * fromIntegral d

    grow :: USD Return -> USD Balance -> USD Balance
    grow (USD amt) (USD d) = USD (amt + d)

    withdraw :: USD Withdrawal -> USD Balance -> USD Balance
    withdraw (USD amt) (USD bal) = USD (bal - amt)


-- USD allowed operations: add, subtract
-- PCT allowed operations: add, mult * usd, grow

million :: USD Balance
million = USD 1000000


