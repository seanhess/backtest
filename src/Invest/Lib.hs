{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Invest.Lib where

import Invest.Prelude
import Invest.Types
import Data.Csv as Csv (decodeByName, Header)
import Data.List as List
import Data.ByteString.Lazy (readFile)
import Data.Vector as Vector (Vector, toList)
import Debug.Trace (trace)
import Control.Monad.State (State, MonadState, get, put, gets, execState)


run :: IO ()
run = do
    rs <- loadReturns

    let hs = toHistories rs
    let ss = samples hs

    let sim = simulation (standard6040Withdraw4 million) million
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

simulation :: Actions () -> Balances -> [History] -> SimResult
simulation actions initial hs =
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



    yearResult :: History -> Balances -> YearResult
    yearResult h bal = 

        let ret = calcReturns h bal
            bal' = bal & apply ret

            act = runActions actions bal'
            end = bal' & apply act

            wd = gains (total bal) (total end)

        in YearResult
          { history = h
          , start = bal
          , returns = ret
          , actions = act
          , end = end
          , withdrawals = wd
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






newtype Actions a = Actions { fromActions :: State (Balances -> Changes) a }
  deriving (Monad, Applicative, Functor, MonadState (Balances -> Changes))



change :: (Balances -> Changes) -> Actions ()
change f = do
    cur <- get :: Actions (Balances -> Changes)
    put $ combine cur f


combine :: (Balances -> Changes) -> (Balances -> Changes) -> Balances -> Changes
combine f1 f2 bal =
    let chg1 = f1 bal
        bal1 = apply chg1 bal
        chg2 = f2 bal1
    in chg2

runActions :: Actions () -> Balances -> Changes
runActions (Actions st) = 
    execState st noChanges


-- ignore the balances and just return the amount
-- staticPercent :: Pct Amount -> Balances -> Balances -> USD Amount 
-- staticPercent p bal1 _ =
--     amount p (total bal1)

withdrawBondsFirst :: USD Amount -> Balances -> Changes
withdrawBondsFirst wda b =
    -- what if we always withdraw from bonds? and rebalancing fixes it?
    if totalCents b.bonds >= totalCents wda
        then Portfolio (USD 0) (loss wda)
        else Portfolio (loss wda) (USD 0)

rebalanceFixed :: Pct Stocks -> Pct Bonds -> Balances -> Changes
rebalanceFixed ps pb bal =
    let tot = total bal
        ts = fromUSD $ amount ps tot :: USD Stocks
        tb = fromUSD $ amount pb tot :: USD Bonds
    in Portfolio (gains bal.stocks ts) (gains bal.bonds tb)

noChanges :: Balances -> Changes
noChanges _ = Portfolio mempty mempty

noActions :: Actions ()
noActions = pure ()


million :: Balances
million = Portfolio
  { stocks = balance $ 600*1000*100
  , bonds = balance $ 400*1000*100
  }

swr4 :: Pct Amount
swr4 = Pct 4 0


rebalance6040 :: Balances -> Changes
rebalance6040 = rebalanceFixed (Pct 60 0) (Pct 40 0)


standardWithdraw4 :: Balances -> Actions ()
standardWithdraw4 start = do
    let const4Percent = loss $ amount swr4 (total start) :: USD Amount
    change $ withdrawBondsFirst const4Percent

standard6040Withdraw4 :: Balances -> Actions ()
standard6040Withdraw4 start = do
    let const4Percent = loss $ amount swr4 (total start) :: USD Amount
    change $ withdrawBondsFirst const4Percent
    change $ rebalanceFixed (Pct 60 0) (Pct 40 0)
    


