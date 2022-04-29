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


run :: IO ()
run = do
    rs <- loadReturns

    let hs = toHistories rs
    let ss = samples hs
    -- mapM_ print hs
    -- mapM_ print rs
    -- mapM_ print $ zip rs (drop 1 rs)

    -- mapM_ print hs
    -- let srs = map (simulation Strategy million) ss
    let res = simulation bondsFirst port6040 million (head ss)

    let wda = amount swr4 (total million)

    -- print wdr
    -- print wda
    

    -- print million
    -- print $ balance 100000
    -- mapM_ print res.years
    mapM_ print (head ss)
    -- mapM_ (print . (.stocks)) (head ss)
    mapM_ (print) res.years


    -- mapM_ (print . (\sr -> (sr.startYear, sr.endYear, sr.endBalance))) $ srs

    -- mapM_ print sr.years
    -- putStrLn ""
    -- putStrLn $ "ENDING BALANCE: " <> show sr.endingBalance
    -- mapM_ print rs

loadReturns :: IO [HistoryRow]
loadReturns = do
    putStrLn "Loading"
    f <- readFile "data/data.csv"
    case Csv.decodeByName f of
        Left e -> fail $ "Decoding CSV Failed: " <> e
        Right (_, rs) -> do
            pure $ filter firstMonth $ Vector.toList rs
    where
        firstMonth rets = rets.month == 1


toHistories :: [HistoryRow] -> [History]
toHistories hr = zipWith toHistory hr (drop 1 hr)


toHistory :: HistoryRow -> HistoryRow -> History
toHistory past now =
    History
      { year = now.year
      , stocks = gainsPercent past.stocks now.stocks
      , bonds = gainsPercent past.bonds now.bonds
      }


samples :: [History] -> [[History]]
samples hs = List.tails hs
  & fmap (take 30)
  & filter (\hs' -> length hs' >= 30)

simulation :: WithdrawalStrategy -> RebalanceStrategy -> Balances -> [History] -> SimResult
simulation withdraw rebalance bal hs =
    let wdr = Pct 0.04 :: Pct Withdrawal
        wda = amount wdr (total bal)
        yr  = (head hs).year
        (bal', yrs) = List.mapAccumL (eachReturns wda) bal hs
    in SimResult
      { startYear = (head hs).year
      , startBalance = bal
      , endYear = (last yrs).history.year
      , endBalance = bal'
      , years = yrs
      }
    
  where

    eachReturns :: USD Amount -> Balances -> History -> (Balances, YearResult)
    eachReturns wda b r =
        let yr = yearResult wda r b
        in (yr.end, yr)


    -- returnFromRealTotal

    apply :: Changes -> Balances -> Balances
    apply ch bals =
        Portfolio (addAmount ch.stocks bals.stocks) (addAmount ch.bonds bals.bonds)

    yearResult :: USD Amount -> History -> Balances -> YearResult
    yearResult wdr h start = 

        -- This is silly :) 
        -- well I want to call a series of functions and have them return their calculations
        -- I could require the types to match
        let rt = calcReturns h start
            rtb = start & apply rt

            wd = withdraw wdr rtb
            wdb = rtb & apply wd

            rb = rebalance wdb
            
            -- rb = rebalance 
            end = wdb & apply rb

        in YearResult
          { history = h
          , start = start
          , returns = rt
          , end = end
          , withdrawals = wd
        --   , rebalance = rb
          }


-- ah, we need the previous one also!
calcReturns :: History -> Balances -> Changes
calcReturns h b =
    let ds = amount h.stocks b.stocks
        db = amount h.bonds b.bonds
    in Portfolio ds db

-- wait, no, this is based on the original withdrawal amount
type WithdrawalStrategy = USD Amount -> Balances -> Changes

bondsFirst :: USD Amount -> Balances -> Changes
bondsFirst wda b =
    -- what if we always withdraw from bonds? and rebalancing fixes it?
    if totalCents b.bonds >= totalCents wda
        then Portfolio (USD 0) (loss wda)
        else Portfolio (loss wda) (USD 0)

type RebalanceStrategy = Balances -> Changes

fixedPortfolio :: Pct Stocks -> Pct Bonds -> Balances -> Changes
fixedPortfolio ps pb bal =
    let tot = total bal
        ts = fromUSD $ amount ps tot :: USD Stocks
        tb = fromUSD $ amount pb tot :: USD Bonds
    in Portfolio (gains bal.stocks ts) (gains bal.bonds tb)



-- applyChange :: Pct Change -> USD Balance -> USD Balance
-- applyChange (Pct p) (USD d) = USD $ round $ (1 + p/100) * fromIntegral d

-- zero :: Pct a
-- zero = Pct 0

-- toReturn :: Pct a -> Pct Return
-- toReturn (Pct a) = Pct a

-- compound :: Pct p -> Pct p -> Pct p
-- compound (Pct p) (Pct p2) = Pct $ 100 * (p/100 * p2/100)

-- withdraw :: Pct Withdrawal -> USD Balance -> Pct Change
-- withdraw pct = loss (toReturn pct)

-- devalue :: Pct Inflation -> Pct Return -> Pct Return
-- devalue pct = loss (toReturn pct)

million :: Balances
million = Portfolio
  { stocks = balance $ 600*1000*100
  , bonds = balance $ 400*1000*100
  }

swr4 :: Pct Amount
swr4 = Pct 0.04


port6040 :: RebalanceStrategy
port6040 = fixedPortfolio (Pct 0.6) (Pct 0.4)