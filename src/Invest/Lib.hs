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
import Debug.Trace (trace)


run :: IO ()
run = do
    rs <- loadReturns

    let hs = toHistories rs
    let ss = samples hs

    let sim = simulation (staticPercent swr4 million) bondsFirst port6040 million
    let srs = map sim ss :: [SimResult]
    mapM_ (print) $ (head srs).years
    -- mapM_ (putStrLn . showSimResult) srs
    -- mapM_ print rs
  where
      showSimResult sr =
          show (sr.startYear, sr.endYear, sr.endBalance)

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
  & fmap (take 40)
  & filter (\hs' -> length hs' >= 40)

simulation :: WithdrawalAmount -> WithdrawalStrategy -> RebalanceStrategy -> Balances -> [History] -> SimResult
simulation withdrawAmount withdraw rebalance initial hs =
    let yr  = (head hs).year
        (bal', yrs) = List.mapAccumL eachReturns initial hs
    in SimResult
      { startYear = (head hs).year
      , startBalance = initial
      , endYear = (last yrs).history.year
      , endBalance = bal'
      , years = yrs
      }
    
  where

    eachReturns :: Balances -> History -> (Balances, YearResult)
    eachReturns b r =
        let yr = yearResult r b
        in (yr.end, yr)


    -- returnFromRealTotal


    yearResult :: History -> Balances -> YearResult
    yearResult h start = 


        -- TODO I don't like how these flow into each other
        let rt = calcReturns h start
            rtb = start & apply rt

            wda = withdrawAmount rtb
            wd = withdraw wda rtb
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


apply :: Changes -> Balances -> Balances
apply ch bals =
    Portfolio (addAmount ch.stocks bals.stocks) (addAmount ch.bonds bals.bonds)


-- ah, we need the previous one also!
calcReturns :: History -> Balances -> Changes
calcReturns h b =
    let ds = amount h.stocks b.stocks
        db = amount h.bonds b.bonds
    in Portfolio ds db

-- wait, no, this is based on the original withdrawal amount


-- ignore the balances and just return the amount
type WithdrawalAmount = Balances -> USD Amount
staticPercent :: Pct Amount -> Balances -> Balances -> USD Amount 
staticPercent p bal1 _ =
    amount p (total bal1)

 

type WithdrawalStrategy = USD Amount -> Balances -> Changes

bondsFirst :: USD Amount -> Balances -> Changes
bondsFirst wda b =
    -- what if we always withdraw from bonds? and rebalancing fixes it?
    if totalCents b.bonds >= totalCents wda
        then Portfolio (USD 0) (loss wda)
        else Portfolio (loss wda) (USD 0)

noWithdraw :: USD Amount -> Balances -> Changes
noWithdraw _ _ = Portfolio zero zero


type RebalanceStrategy = Balances -> Changes

fixedPortfolio :: Pct Stocks -> Pct Bonds -> Balances -> Changes
fixedPortfolio ps pb bal =
    let tot = total bal
        ts = fromUSD $ amount ps tot :: USD Stocks
        tb = fromUSD $ amount pb tot :: USD Bonds
    in Portfolio (gains bal.stocks ts) (gains bal.bonds tb)

noRebalance :: Balances -> Changes
noRebalance _ = Portfolio zero zero



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
swr4 = Pct 4 0


port6040 :: RebalanceStrategy
port6040 = fixedPortfolio (Pct 60 0) (Pct 40 0)
