{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use camelCase" #-}
module Invest.Lib where

import Invest.Prelude
import Invest.Types
import Data.Csv as Csv (decodeByName, Header)
import Data.List as List
import Data.ByteString.Lazy (readFile)
import Data.Vector as Vector (Vector, toList)

test :: IO ()
test = do
    rs <- Vector.toList <$> loadReturns

    let srs = map (simulation million) (samples rs)

    mapM_ (print . (\sr -> (sr.startYear, sr.endYear, sr.endBalance))) $ srs

    -- mapM_ print sr.years
    -- putStrLn ""
    -- putStrLn $ "ENDING BALANCE: " <> show sr.endingBalance
    -- mapM_ print rs

loadReturns :: IO (Vector Returns)
loadReturns = do
    putStrLn "Loading"
    f <- readFile "data/data.csv"
    Right (_, rs) <- pure $ Csv.decodeByName f
    pure rs


samples :: [Returns] -> [[Returns]]
samples rss = List.tails rss
--   & fmap (take 30)
  & filter (\rs -> length rs >= 30)

simulation :: USD Balance -> [Returns] -> SimResult
simulation balance rs =
    let wdr = Pct 4 :: Pct Withdrawal
        yr  = (head rs).year
        (bal', yrs) = List.mapAccumL (eachReturns wdr) balance rs
    in SimResult
      { startYear = (head rs).year
      , startBalance = balance
      , endYear = (last yrs).year
      , endBalance = bal'
      , years = yrs
      }
    
  where

    eachReturns :: Pct Withdrawal -> USD Balance -> Returns -> (USD Balance, YearResult)
    eachReturns wdr bal r =
        let yr = calculateYear r wdr bal
        in (yr.realEnd, yr)

    calculateYear :: Returns -> Pct Withdrawal -> USD Balance -> YearResult
    calculateYear rets wdr bal = 
        let investments = rets.totalStock

            realChange = zero
               & gain investments
               & devalue rets.inflation
               & withdraw wdr

            realEnd = bal & change realChange

        in YearResult
          { year = rets.year
          , start = bal
          , realEnd = realEnd
          , realChange = realChange
          , investments = investments
          , withdrawal = wdr
          , inflation = rets.inflation
          }


change :: Pct Change -> USD Balance -> USD Balance
change (Pct p) (USD d) = USD $ round $ (1 + p/100) * fromIntegral d

zero :: Pct Change
zero = Pct 0

toReturn :: Pct a -> Pct Return
toReturn (Pct a) = Pct a

compound :: Pct p -> Pct p -> Pct p
compound (Pct p) (Pct p2) = Pct $ 100 * (p/100 * p2/100)

gain :: Pct Return -> Pct Change -> Pct Change
gain (Pct r) (Pct c) = Pct $ c + r

loss :: Pct Return -> Pct Change -> Pct Change
loss (Pct r) (Pct c) = Pct $ c - r

withdraw :: Pct Withdrawal -> Pct Change -> Pct Change
withdraw pct = loss (toReturn pct)

devalue :: Pct Inflation -> Pct Change -> Pct Change
devalue pct = loss (toReturn pct)


million :: USD Balance
million = USD 1000000


