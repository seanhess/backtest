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


simulation :: Balances -> Actions () -> [History] -> SimResult
simulation _ _ [] = error "Simulation: [History] is empty"
simulation initial actions hs =
    let ys  = (head hs).year
        ye  = (last hs).year
        (bal', yrs) = List.mapAccumL (eachReturns ye) initial hs
    in SimResult
      { startYear = ys
      , startBalance = initial
      , endYear = (last yrs).history.year
      , endBalance = bal'
      , years = yrs
      , withdrawals = withdrawalResults yrs
      }
    
  where

    eachReturns :: Year -> Balances -> History -> (Balances, YearResult)
    eachReturns ye b h =
        let yr = yearResult ye h b
        in (yr.end, yr)



    yearResult :: Year -> History -> Balances -> YearResult
    yearResult  ye h bal = 

        let bal' = calcReturns h bal
            ret = changes bal bal'

            st = runActionState ye h bal' actions
            end = st._balances
            act = changes bal' end

        in YearResult
          { history = h
          , start = bal
          , returns = ret
          , actions = act
          , end = end
          , withdrawal = st._withdrawal
          }

withdrawalResults :: [YearResult] -> WithdrawalResults
withdrawalResults yrs =
    let wds = map (.withdrawal) yrs
        int = (head yrs).withdrawal
    in WithdrawalResults
        { low = minimum wds
        , med = median wds
        , init = int
        , p10 = percentile 0.10 wds
        , p25 = percentile 0.25 wds
        , p75 = percentile 0.75 wds
        , p90 = percentile 0.90 wds
        , drawdown50 = drawdown (pct 50) int wds
        , drawdown40 = drawdown (pct 40) int wds
        , drawdown30 = drawdown (pct 30) int wds
        , drawdown20 = drawdown (pct 20) int wds
        , drawdown10 = drawdown (pct 10) int wds
        }
    where
        drawdown :: Pct Withdrawal -> USD Amt Withdrawal -> [USD Amt Withdrawal] -> Int
        drawdown p int wds =
            let f = Pct.toFloat p
                t = fromCents $ round $ (1 - f) * fromIntegral (totalCents int)
            in length $ filter (<=t) wds


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
  , _end :: Year
  }

runActions :: Year -> History -> Balances -> Actions () -> Balances
runActions y h bal act = 
    let as = runActionState y h bal act
    in as._balances

runActionState :: Year -> History -> Balances -> Actions () -> ActionState
runActionState ye h bal (Actions st) = 
    let as = ActionState bal (usd 0) h ye :: ActionState
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
    pure $ (ye - yc) + 1

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


