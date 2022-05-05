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

    let h1 = head hs

    -- mapM_ print hs
    let ys = (head srs).years
    let y1 = head ys
    print y1
    -- print $ bp.toFloat
    -- print $ take 2 rs
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

            end = runActions bal' actions
            act = changes bal' end

            wd = total act

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






newtype Actions a = Actions { fromActions :: State Balances a }
  deriving (Monad, Applicative, Functor, MonadState Balances)




changes :: Balances -> Balances -> Changes
changes start end =
    Portfolio (gains start.stocks end.stocks) (gains start.bonds end.bonds)

-- well, wait, when we runActions/

-- Run a function that produces changes
change :: (Balances -> Changes) -> Actions ()
change f = do
    cur <- get :: Actions Balances
    put $ apply (f cur) cur

runActions :: Balances -> Actions () -> Balances
runActions bal (Actions st) = 
    execState st bal




staticWithdrawalAmount :: Pct Amount -> Balances -> USD Amount 
staticWithdrawalAmount p bal1 =
    amount p (total bal1)

withdrawBondsFirst :: USD Amount -> Balances -> Changes
withdrawBondsFirst wda b =
    -- what if we always withdraw from bonds? and rebalancing fixes it?
    if totalCents b.bonds >= totalCents wda
        then Portfolio mempty (loss wda)
        else Portfolio (loss wda) mempty

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
swr4 = pct 4


rebalance6040 :: Balances -> Changes
rebalance6040 = rebalanceFixed (pct 60.0) (pct 40.0)


standardWithdraw4 :: Balances -> Actions ()
standardWithdraw4 start = do
    let const4Percent = loss $ staticWithdrawalAmount swr4 start :: USD Amount
    change $ withdrawBondsFirst const4Percent

standard6040Withdraw4 :: Balances -> Actions ()
standard6040Withdraw4 start = do
    standardWithdraw4 start
    standardRebalance6040
    
standardRebalance6040 :: Actions ()
standardRebalance6040 = do
    change $ rebalanceFixed (pct 60.0) (pct 40.0)


-- test1 :: Balances -> Actions [Changes]
-- test1 start = do
--     f <- get
--     standardWithdraw4 start
--     f2 <- get
--     standardRebalance6040
--     f3 <- get
--     pure [f start, f2 start, f3 start]


