{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Backtest.Simulation
  ( simulation
  , calcReturns
  , averagePortfolio, medianPortfolio
  , rebalance
  , withdraw
  , bondsFirst
  , Actions
  , noActions
  , noChanges
  , runActions
  ) where

import Backtest.Prelude
import Backtest.Types

import Control.Monad.State (State, MonadState, modify, execState, put, get)
import Data.List as List


simulation :: Balances -> Actions () -> [History] -> SimResult
simulation initial actions hs =
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

        let bal' = calcReturns h bal
            ret = changes bal bal'

            st = runActionState bal' actions
            end = st.balances
            act = changes bal' end

        in YearResult
          { history = h
          , start = bal
          , returns = ret
          , actions = act
          , end = end
          , withdrawals = st.withdrawal
          }


calcReturns :: History -> Balances -> Balances
calcReturns h b =
    let ds = amount h.stocks b.stocks
        db = amount h.bonds b.bonds
    in Portfolio (addToBalance ds b.stocks) (addToBalance db b.bonds)






averagePortfolio :: [SimResult] -> USD Bal Total
averagePortfolio srs =
    let tots = map (total . (.endBalance)) srs
        avg = fromCents $ round $ (fromIntegral $ sum $ map (totalCents) tots :: Float) / (fromIntegral $ length tots :: Float) :: USD Bal Total
    in avg

medianPortfolio :: [SimResult] -> USD Bal Total
medianPortfolio srs =
    let tots = map (total . (.endBalance)) srs
    in fromCents $ median $ map totalCents tots





-----------------------------------
-- * Actions
-----------------------------------

newtype Actions a = Actions { fromActions :: State ActionState a }
  deriving (Monad, Applicative, Functor, MonadState ActionState)

data ActionState = ActionState
  { balances :: Balances
  , withdrawal :: USD Amt Withdrawal
  }

runActions :: Balances -> Actions () -> Balances
runActions bal act = 
    let as = runActionState bal act
    in as.balances

runActionState :: Balances -> Actions () -> ActionState
runActionState bal (Actions st) = 
    let as = ActionState bal (usd 0) :: ActionState
    in execState st as






withdraw :: USD Amt Withdrawal -> Actions ()
withdraw wda = do
    modify $ \st -> st
      { withdrawal = wda
      , balances = bondsFirst wda st.balances
      }

bondsFirst :: USD Amt Withdrawal -> Balances -> Balances
bondsFirst wd b =
    let wdb = toBonds wd :: USD Amt Bonds
        wds = toStocks $ minZero $ gains (toAmount b.bonds) (toBonds wd) :: USD Amt Stocks
    in  Portfolio (addToBalance (loss wds) b.stocks) (addToBalance (loss wdb) b.bonds)

rebalance :: (Balances -> Balances) -> Actions ()
rebalance f = do
    modify $ \st -> st
      { balances = f st.balances
      }

noActions :: Actions ()
noActions = pure ()

noChanges :: Balances -> Changes
noChanges _ = Portfolio mempty mempty










median :: (Ord a, Num a) => [a] -> a   
median [] = 0  
median xs = sort xs !! mid   
 where mid = (length xs) `div` 2 


