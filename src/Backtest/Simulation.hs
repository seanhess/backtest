{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Backtest.Simulation
  ( simulation
  , calcReturns
  , averageEndPortfolio, medianEndPortfolio
  , rebalance
  , withdraw
  , bondsFirst
  , Actions
  , noActions
  , noChanges
  , runActions
  , runActionState
  , history
  , balances
  , yearsLeft
  ) where

import Backtest.Prelude
import Backtest.Types hiding (history)
import Backtest.Types.Pct as Pct
import qualified Backtest.Types.Sim as Sim

import Control.Monad.State (State, MonadState, modify, execState, put, get, gets)
import Data.List as List
import Debug.Trace


-- Year 0 = 1900 = START at the beginning of 1900
-- Year 1 = 1901
-- Year 2 = 1902

-- It's the beginning of year 1900
--  a. withdraw (balance start)
--  b. rebalance

-- It's the beginning of 1901
--  a. apply returns (1900)
--  b. withdraw (cape 1900)
--  c. rebalance




-- TODO START withdraw, rebalance
-- TODO NEXT apply returns, withdraw, rebalance

simulation :: Balances -> Actions () -> [History] -> SimResult
simulation _ _ [] = error "Simulation: [History] is empty"
simulation initial actions hs =
    let ys  = (head hs).year

        -- the last year the simulation is run
        yl  = (last hs).year

        -- the year the money should run out
        ye  = nextYear yl

        (h:hs') = hs

        firstYear = firstYearResult ye h initial

        (yr, yrs) = List.mapAccumL (eachReturns ye) (Left firstYear.end) hs'

        bal' = case yr of
                Left b -> b
                Right y -> y.end

        wds = map (.withdrawal) yrs

    in SimResult
      { startYear = ys
      , startBalance = initial
      , endYear = ye
      , endBalance = bal'
      , years = firstYear:yrs
      , wdAmts = withdrawalResults wds
      , wdSpread = withdrawalSpread (total initial) wds
      }
    
  where

    eachReturns :: Year -> Either Balances YearStart -> History -> (Either Balances YearStart, YearStart)
    eachReturns ye (Left b) h =
        let yr = nextYearResult ye h Nothing b
        in (Right yr, yr)
    eachReturns ye (Right lastYear) h =
        let yr = nextYearResult ye h (Just lastYear) lastYear.end
        in (Right yr, yr)


    firstYearResult :: Year -> History -> Balances -> YearStart
    firstYearResult ye h start = 

        -- run actions, but do not apply returns
        -- simply withdraw, etc
        let st = runActionState ye h (Left start) actions
            end = st._balances
            act = changes start end
            ret = Portfolio mempty mempty

        in YearStart
          { history = Just h
          , year = h.year
          , start = start
          , returns = ret
          , actions = act
          , end = end
          , withdrawal = st._withdrawal
          }


    -- Produces:
    -- YearResult 1900 (0 ret) Withdrawal Rebalance
    -- YearResult 1901 (10% ret) (Withdrawal) Rebalance

    -- ye = the simulation ends in which year?
    nextYearResult :: Year -> History -> Maybe YearStart -> Balances -> YearStart
    nextYearResult ye h lastYear balOld = 

        -- Its the beginning of simulation
        --   a. withdraw
        --   b. rebalance

        -- It's the END of year 1900
        --  a. apply returns (1900)
        --  a. withdraw (cape 1900)
        --  b. rebalance

        -- It's the end of 1901
        --  a. apply returns (1901)
        --  b. withdraw (cape 1901)
        --  c. rebalance

        let balRet = calcReturns h balOld
            ret = changes balOld balRet
            
            st = runActionState ye h (Left balRet) actions
            end = st._balances
            act = changes balRet end

        in YearStart
          { history = Just h
          , year = h.year
          , start = balOld
          , returns = ret
          , actions = act
          , end = end
          , withdrawal = st._withdrawal
          }

withdrawalResults :: [USD Amt Withdrawal] -> WithdrawalResults
withdrawalResults wds =
    WithdrawalResults
        { low = minimum wds
        , med = median wds
        , init = head wds
        , p10 = percentile 0.10 wds
        , p25 = percentile 0.25 wds
        , p75 = percentile 0.75 wds
        , p90 = percentile 0.90 wds
        }
    where


withdrawalSpread :: USD Bal Total -> [USD Amt Withdrawal] -> WithdrawalSpread Int
withdrawalSpread start wds =
    WithdrawalSpread
        { wlow = lowWithdrawals start (pct 0.0) (pct 2.0) wds
        , w2_0 = lowWithdrawals start (pct 2.0) (pct 2.5) wds
        , w2_5 = lowWithdrawals start (pct 2.5) (pct 3.0) wds
        , w3_0 = lowWithdrawals start (pct 3.0) (pct 3.5) wds
        , w3_5 = lowWithdrawals start (pct 3.5) (pct 4.0) wds
        , w4_0 = lowWithdrawals start (pct 4.0) (pct 4.5) wds
        , w4_5 = lowWithdrawals start (pct 4.5) (pct 5.0) wds
        , whigh = lowWithdrawals start (pct 5.0) (pct 100) wds
        }

lowWithdrawals :: USD Bal Total -> Pct Withdrawal -> Pct Withdrawal -> [USD Amt Withdrawal] -> Int
lowWithdrawals start low high wds =
    let l = amount low start
        h = amount high start
    in length $ filter (\w -> l <= w && w < h) wds


-- drawdowns :: USD Amt Withdrawal -> [YearResult] -> [[YearResult]]
-- drawdowns med ys =
--     filter (not . null) $ map (takeWhile (isDrawdown med)) (tails ys)

-- -- longest drawdown
-- drawdownLength' :: [[YearResult]] -> Int
-- drawdownLength' dds =
--     maximum $ map length dds

-- -- deepest drawdown
-- drawdownPeak' :: [[YearResult]] -> USD Amt Withdrawal
-- drawdownPeak' dds =
--     minimum $ map peak dds
--     where
--         peak yrs = minimum $ map (.withdrawal) yrs

-- isDrawdown :: USD Amt Withdrawal -> YearResult -> Bool
-- isDrawdown med yr = yr.withdrawal < med


-- drawdown :: Pct Withdrawal -> USD Amt Withdrawal -> [USD Amt Withdrawal] -> Int
-- drawdown p int wds =
--     let f = Pct.toFloat p
--         t = fromCents $ round $ (1 - f) * fromIntegral (totalCents int)
--     in length (filter (<=t) wds)


calcReturns :: History -> Balances -> Balances
calcReturns h b =
    let ds = amount h.stocks b.stocks
        db = amount h.bonds b.bonds
    in Portfolio (addToBalance ds b.stocks) (addToBalance db b.bonds)






averageEndPortfolio :: [SimResult] -> USD Bal Total
averageEndPortfolio srs =
    let tots = map (total . (.endBalance)) srs
        avg = fromCents $ round $ (fromIntegral $ sum $ map (totalCents) tots :: Float) / (fromIntegral $ length tots :: Float) :: USD Bal Total
    in avg

-- Median END Balance, not portfolio
medianEndPortfolio :: [SimResult] -> USD Bal Total
medianEndPortfolio srs =
    let tots = map (total . (.endBalance)) srs
    in fromCents $ median $ map totalCents tots





-----------------------------------
-- * Actions
-----------------------------------



newtype Actions a = Actions { fromActions :: State ActionState a }
  deriving (Monad, Applicative, Functor, MonadState ActionState)

data ActionState = ActionState
  { _balances :: Balances
  , _withdrawal :: USD Amt Withdrawal
  , _history :: History
  , _lastYear :: Maybe YearStart

  -- the year you are out of money and take no actions
  -- 1900 - 1950
  -- 1900 = start
  -- 1950 = end
  -- 1949 = last year you take action
  , _end :: Year
  }

runActions :: Year -> History -> Either Balances YearStart -> Actions () -> Balances
runActions y h bal act = 
    let as = runActionState y h bal act
    in as._balances

runActionState :: Year -> History -> Either Balances YearStart -> Actions () -> ActionState
runActionState ye h eby (Actions st) = 
    let bal = either identity (.end) eby :: Balances
        ly = either (const Nothing) Just eby :: Maybe YearStart
        as = ActionState bal (usd 0) h ly ye :: ActionState
    in execState st as





withdraw :: USD Amt Withdrawal -> Actions ()
withdraw wda = do
    modify $ \st -> st
      { _withdrawal = wda
      , _balances = bondsFirst wda st._balances
      }

bondsFirst :: USD Amt Withdrawal -> Balances -> Balances
bondsFirst wd b =
    let wdb = toBonds wd :: USD Amt Bonds
        wds = toStocks $ minZero $ gains (toAmount b.bonds) (toBonds wd) :: USD Amt Stocks
    in  Portfolio (addToBalance (loss wds) b.stocks) (addToBalance (loss wdb) b.bonds)

rebalance :: (Balances -> Balances) -> Actions ()
rebalance f = do
    modify $ \st -> st
      { _balances = f st._balances
      }

balances :: Actions Balances
balances = do
    gets _balances

history :: Actions History
history = do
    gets _history


yearsLeft :: Actions Int
yearsLeft = do
    Year ye <- gets _end
    Year yc <- (.year) <$> gets _history
    pure $ ye - yc

noActions :: Actions ()
noActions = pure ()

noChanges :: Balances -> Changes
noChanges _ = Portfolio mempty mempty











median :: (Ord a) => [a] -> a   
median [] = error "Median of empty list"
median xs = sort xs !! mid   
 where mid = (length xs) `div` 2 

percentile :: (Ord a) => Float -> [a] -> a   
percentile _ [] = error "Percentile of empty list"
percentile p xs = sort xs !! n
 where n = round $ (fromIntegral $ length xs) * p


