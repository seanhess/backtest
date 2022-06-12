{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Backtest.Simulation
  ( simulation, simulation'
  , calcReturns
  , rebalance
  , withdraw
  , income
  , expense
  , bondsFirst
  , Actions
  , noActions
  , noChanges
  , runActions
  , runActionState
  , runSimActions
  , SimContext(..)
  , now
  , history
  , balances
  , yearsLeft
  , yearsElapsed
  , lastWithdrawal
  , onYears
  ) where

import Backtest.Prelude
import Backtest.Types hiding (history, startYear)
import Backtest.Types.Pct as Pct
import Backtest.Aggregate (withdrawalResults, withdrawalSpread)
import qualified Backtest.Types.Sim as Sim

import Control.Monad.State (State, StateT, MonadState, modify, execStateT, put, get, gets)
import Control.Monad.Reader (ReaderT, Reader, MonadReader, runReaderT, runReader, asks)
import Control.Monad (when)
import qualified Data.List as List
import Debug.Trace

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty, head, fromList)


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




simulation' :: Balances -> (History -> Actions ()) -> NonEmpty History -> SimResult
simulation' start getActions hs =
    let acts = getActions (head hs)
    in simulation start acts hs

simulation :: Balances -> Actions () -> NonEmpty History -> SimResult
simulation initial actions hs =
    let ys  = (head hs).year

        -- the last year the simulation is run
        yl  = (last hs).year

        -- the year the money should run out
        ye  = nextYear yl

        (h :| hs') = hs

        context = SimContext hs ye

        firstYear = flip runReader context $ firstYearResult h initial

        -- a function of 
        ((yr, _), yrs) = List.mapAccumL (\a h' -> runReader (eachReturns ys a h') context) (firstYear, [firstYear]) hs'

        years = NE.fromList $ firstYear : yrs

        bal' = yr.end

        wds = fmap (.withdrawal) years

    in SimResult
        { startYear = ys
        , startBalance = initial
        , endYear = ye
        , endBalance = bal'
        , years = years
        , wdAmts = withdrawalResults wds
        , wdSpread = withdrawalSpread (total initial) wds
        }
    
  where

    eachReturns :: Year -> (YearStart, [YearStart]) -> History -> Reader SimContext ((YearStart, [YearStart]), YearStart)
    eachReturns ys (lastYear', pastYears) h = do
        yr <- nextYearResult ys h pastYears lastYear'.end
        pure ((yr, yr:pastYears), yr)


    firstYearResult :: History -> Balances -> Reader SimContext YearStart
    firstYearResult h start = do

        st <- runActionState h start [] actions
        let end = st._balances
            act = changes start end
            ret = Portfolio mempty mempty

        -- in trace (show ("firstYearResult", h.year)) $ YearStart
        pure $ YearStart
          { history = Just h
          , year = h.year
          , yearIndex = 0
          , start = start
          , returns = ret
          , actions = act
          , end = end
          , withdrawal = st._withdrawal
          , netIncome = st._income
          , netExpenses = st._expenses
          }


    -- Produces:
    -- YearResult 1900 (0 ret) Withdrawal Rebalance
    -- YearResult 1901 (10% ret) (Withdrawal) Rebalance

    -- ye = the simulation ends in which year?
    nextYearResult :: Year -> History -> [YearStart] -> Balances -> Reader SimContext YearStart
    nextYearResult ys h pastYears balOld = do

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
            
        st <- runActionState h balRet pastYears actions
        let end = st._balances
            act = changes balRet end

        pure YearStart
          { history = Just h
          , year = h.year
          , yearIndex = fromYear $ h.year - ys
          , start = balRet
          , returns = ret
          , actions = act
          , end = end
          , withdrawal = st._withdrawal
          , netIncome = st._income
          , netExpenses = st._expenses
          }



-- drawdowns :: USD (Amt Withdrawal) -> [YearResult] -> [[YearResult]]
-- drawdowns med ys =
--     filter (not . null) $ map (takeWhile (isDrawdown med)) (tails ys)

-- -- longest drawdown
-- drawdownLength' :: [[YearResult]] -> Int
-- drawdownLength' dds =
--     maximum $ map length dds

-- -- deepest drawdown
-- drawdownPeak' :: [[YearResult]] -> USD (Amt Withdrawal)
-- drawdownPeak' dds =
--     minimum $ map peak dds
--     where
--         peak yrs = minimum $ map (.withdrawal) yrs

-- isDrawdown :: USD (Amt Withdrawal) -> YearResult -> Bool
-- isDrawdown med yr = yr.withdrawal < med


-- drawdown :: Pct Withdrawal -> USD (Amt Withdrawal) -> [USD (Amt Withdrawal)] -> Int
-- drawdown p int wds =
--     let f = Pct.toFloat p
--         t = fromCents $ round $ (1 - f) * fromIntegral (totalCents int)
--     in length (filter (<=t) wds)


calcReturns :: History -> Balances -> Balances
calcReturns h b =
    let ds = amount h.returns.stocks b.stocks
        db = amount h.returns.bonds b.bonds
    in Portfolio (addToBalance ds b.stocks) (addToBalance db b.bonds)









-----------------------------------
-- * Actions
-----------------------------------


-- actions can only do the little actions
newtype Actions a = Actions { fromActions :: StateT ActionState (Reader SimContext) a }
  deriving (Monad, Applicative, Functor, MonadState ActionState, MonadReader SimContext)


-- could it be different from the history itself?
data SimContext = SimContext
  { _history :: NonEmpty History
  , _end :: Year
  }

data ActionState = ActionState
  { _balances :: Balances
  , _withdrawal :: USD (Amt Withdrawal)
  , _income :: USD (Amt Income)
  , _expenses :: USD (Amt Expense)
  , _now :: History
  , _pastYears :: [YearStart]
--   , _history :: NonEmpty History

  -- the year you are out of money and take no actions
  -- 1900 - 1950
  -- 1900 = start
  -- 1950 = end
  -- 1949 = last year you take action
--   , _end :: Year
  }

runSim :: Reader SimContext a -> SimContext -> a
runSim r c = runReader r c

runSimActions :: SimContext -> History -> Balances -> [YearStart] -> Actions () -> Balances
runSimActions c h bal ys acts = runReader (runActions h bal ys acts) c

runActions :: History -> Balances -> [YearStart] -> Actions () -> Reader SimContext Balances
runActions h bal ys act = do
    as <- runActionState h bal ys act
    pure as._balances

runActionState :: History -> Balances -> [YearStart] -> Actions () -> Reader SimContext ActionState
runActionState h bal ys (Actions st) = 
    let as = ActionState bal (usd 0) (usd 0) (usd 0) h ys :: ActionState
    -- in trace (show ("runActionState", h.year)) $ execState st as
    in execStateT st as


withdraw :: USD (Amt Withdrawal) -> Actions ()
withdraw wda = do
    st <- get
    let bal = bondsFirst wda st._balances
    -- actual withdrawal is handled by withdrawal method
    let wa' = total $ changes st._balances bal
    put st
      { _withdrawal = gain $ fromUSD $ wa'
      , _balances = bal
      }

income :: USD (Amt Income) -> Actions ()
income inc = do
    modify $ \st -> st
      { _balances = addIncome inc st._balances
      , _income = st._income + inc
      }

expense :: USD (Amt Expense) -> Actions ()
expense ex = do
    modify $ \st -> st
      { _balances = addExpense ex st._balances
      , _expenses = st._expenses + ex 
      }

addIncome :: USD (Amt Income) -> Balances -> Balances
addIncome inc b =
    Portfolio b.stocks (addToBalance (gain inc) b.bonds)

-- shoot this doesn't work if stocks are 100%
addExpense :: USD (Amt Expense) -> Balances -> Balances
addExpense ex b = bondsFirst (toWithdrawal ex) b



bondsFirst :: USD (Amt Withdrawal) -> Balances -> Balances
bondsFirst wd b =
    let wdb = toBonds wd :: USD (Amt Bonds)
        wds = toStocks $ minZero $ gains (toAmount b.bonds) (toBonds wd) :: USD (Amt Stocks)
    in  Portfolio (addToBalance (loss wds) b.stocks) (addToBalance (loss wdb) b.bonds)

rebalance :: (Balances -> Balances) -> Actions ()
rebalance f = do
    modify $ \st -> st
      { _balances = f st._balances
      }

balances :: Actions Balances
balances = do
    gets _balances

now :: Actions History
now = do
    gets _now

history :: Actions (NonEmpty History)
history = do
    asks _history


onYears :: [Int] -> Actions () -> Actions ()
onYears yrs action = do
    ye <- yearsElapsed
    when (ye `elem` yrs) action

startYear :: Actions Year
startYear = do
    hs <- asks _history
    pure $ (.year) $ head hs

yearsElapsed :: Actions Int
yearsElapsed = do
    ys <- startYear
    yc <- (.year) <$> now
    pure $ fromYear yc - fromYear ys

yearsLeft :: Actions Int
yearsLeft = do
    Year ye <- asks _end
    Year yc <- (.year) <$> now
    pure $ ye - yc

lastYear :: Actions (Maybe YearStart)
lastYear = do
    headMay <$> gets _pastYears

lastWithdrawal :: Actions (Maybe (USD (Amt Withdrawal)))
lastWithdrawal = do
    fmap (.withdrawal) <$> lastYear


noActions :: Actions ()
noActions = pure ()

noChanges :: Balances -> Changes
noChanges _ = Portfolio mempty mempty











