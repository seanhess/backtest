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
  , runSimActions
  , runSim
  , runYear
  , SimContext(..)
  , now
  , history
  , balances
  , yearsLeft
  , yearsElapsed
  , lastWithdrawal
  , onYears
  , actionContext
  , actionState
  , yearBalances
  , withdrawals
  , npvExpenses
  , npvExpense
  ) where

import Backtest.Prelude
import Backtest.Types hiding (history, startYear)
import Backtest.Types.Pct as Pct
import qualified Backtest.Types.Sim as Sim

import Control.Monad.State (State, StateT, MonadState, modify, execStateT, put, get, gets, runStateT, evalState, evalStateT)
import Control.Monad.Reader (ReaderT, Reader, MonadReader, runReaderT, runReader, asks, ask)
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
simulation start actions hs =
    let yl  = (last hs).year -- the last year the simulation is run

        -- the year the money should run out
        ye  = nextYear yl

        context = SimContext hs ye start

    in runSim context $ do
          years <- runYears actions hs

          end <- gets (._balances) 

          pure $ SimResult
            { startYear = (head hs).year
            , startBalance = start
            , endYear = ye
            , endBalance = end
            , years = years
            -- , wdAmts = withdrawalResults wds
            -- , wdSpread = withdrawalSpread (total start) wds
            }
    

-----------------------------------
-- * Actions
-----------------------------------

-- 1. Accumulate successive states or values
-- 2. State history available to actions
-- 3. Modify state

newtype Sim a = Sim { fromSim :: StateT SimState (Reader SimContext) a}
  deriving (Monad, Applicative, Functor, MonadState SimState, MonadReader SimContext)

data SimState = SimState
  { _years :: [YearStart]

  -- the year over year balance
  , _balances :: Balances
  -- don't have a yearstart here. They aren't editing it. They just make it
  }

data SimContext = SimContext
  { _history :: NonEmpty History
  , _end :: Year
  , _start :: Balances
  }


-- actions can only do the little actions
newtype Actions a = Actions { fromActions :: StateT ActionState (Reader ActionContext) a }
  deriving (Monad, Applicative, Functor, MonadState ActionState, MonadReader ActionContext)


-- stuff they can't change
data ActionContext = ActionContext
  { _pastYears :: [YearStart]

  , _returns :: Changes

  -- Balances AFTER returns
  , _balances :: Balances

  , _now :: History
  , _history :: NonEmpty History
  , _end :: Year
  }

-- they can't edit balances directly, just add information 
data ActionState = ActionState
  { _returns    :: Changes -- this is derived from history, no? No, not always. Some years you don't do any returns

  , _withdrawal :: USD (Amt Withdrawal)
  , _incomes    :: [USD (Amt Income)]
  , _expenses   :: [USD (Amt Expense)]

  , _rebalance :: (Balances -> Balances)
  }


runSim :: SimContext -> Sim a -> a
runSim ctx simActions =
    let st = SimState [] ctx._start
    in runReader (evalStateT (fromSim simActions) st) ctx

runYears :: Actions () -> NonEmpty History -> Sim (NonEmpty YearStart)
runYears acts hs = do
    let (h :| hs') = hs

    year0 <- runYear0 h
    years <- mapM runYearN hs'

    pure $ (year0 :| years)

  where
      -- no returns in year0
      runYear0 :: History -> Sim YearStart
      runYear0 h = runYear h mempty acts 

      runYearN :: History -> Sim YearStart
      runYearN h = do
          bal <- gets (._balances)
          runYear h (calcReturns h bal) acts 


-- this is where the magic happens
-- the changes require
runYear :: History -> Changes -> Actions () -> Sim YearStart
runYear h rets actions = do

    year <- runSimActions h rets $ do
                bal <- balances

                -- apply their actions
                actions            

                -- calculate their end balance
                end <- yearBalances

                -- calculate their net actions
                let act = changes bal end

                yearStart end act

    saveYear year
    saveBalances year.end

    pure $ year


saveYear :: YearStart -> Sim ()
saveYear ys = modify addYear
  where
    addYear st = st
      { _years = ys : st._years
      }
    


saveBalances :: Balances -> Sim ()
saveBalances bal = modify $ setBalances bal

setBalances :: Balances -> SimState -> SimState
setBalances bal st = st { _balances = bal }





-- TODO calc returns
runActions :: ActionContext -> ActionState -> Actions a -> a
runActions ctx st acts = do
    runReader (evalStateT (fromActions acts) st) ctx

runSimActions :: History -> Changes -> Actions a -> Sim a
runSimActions h rets actions = do
    ctx <- actionContext h rets
    pure $ runActions ctx (actionState rets) actions


netActions :: Actions Changes
netActions = do
    bal <- balances
    end <- yearBalances
    pure $ changes bal end

yearBalances :: Actions Balances
yearBalances = do
    bal <- balances
    st <- get
    pure $ bal
      & addIncomes st._incomes
      & addExpenses st._expenses
      & addWithdrawal st._withdrawal
      & st._rebalance


-- doesn't calculate anything, just summarizes it
yearStart :: Balances -> Changes -> Actions YearStart
yearStart endBal as = do
    ctx <- ask
    st  <- get
    let h = ctx._now :: History
    let h0 = head (ctx._history)
    pure YearStart
        { history = h
        , year = h.year
        , yearIndex = fromYear $ h.year - h0.year
        , start = ctx._balances
        , returns = st._returns
        , actions = as
        , end = endBal
        , withdrawal = st._withdrawal
        , netIncome = sum $ st._incomes
        , netExpenses = sum $ st._expenses
        }


-- | Creates a context with the returns applied, ready to go
actionContext :: History -> Changes -> Sim ActionContext
actionContext h rets = do
    sim <- get
    ctx <- ask
    -- let rets = calcReturns h sim.balances
    pure $ ActionContext
      { _pastYears = sim._years
      , _returns = rets
      , _balances = addChanges rets sim._balances

      , _now = h
      , _end = ctx._end
      , _history = ctx._history
      }

actionState :: Changes -> ActionState
actionState rets = ActionState rets 0 [] [] identity

withdraw :: USD (Amt Withdrawal) -> Actions ()
withdraw wda = do

    -- TODO: income should be applied to balance before calling anything that might calculate withdrawal
    -- it needs to be like rebalancing I think

    bal <- balances
    let wa' = min (gain wda) (toWithdrawal $ toAmount $ total bal)

    -- only one
    modify $ \st -> st { _withdrawal = wa' }


income :: USD (Amt Income) -> Actions ()
income inc = do
    modify $ \st -> st
      { _incomes = gain inc : st._incomes }

expense :: USD (Amt Expense) -> Actions ()
expense ex = do
    modify $ \st -> st
      { _expenses = gain ex : st._expenses }

addIncome :: USD (Amt Income) -> Balances -> Balances
addIncome inc b =
    Portfolio b.stocks (addToBalance (gain inc) b.bonds)

-- shoot this doesn't work if stocks are 100%
addExpense :: USD (Amt Expense) -> Balances -> Balances
addExpense ex b = bondsFirst (toWithdrawal ex) b

addIncomes :: [USD (Amt Income)] -> Balances -> Balances
addIncomes is b = foldr addIncome b is

addExpenses :: [USD (Amt Expense)] -> Balances -> Balances
addExpenses is b = foldr addExpense b is

addWithdrawal :: USD (Amt Withdrawal) -> Balances -> Balances
addWithdrawal = bondsFirst

-- | Withdraws from bonds, but limits it to zero if the withdrawal is higher
bondsFirst :: USD (Amt Withdrawal) -> Balances -> Balances
bondsFirst wd b =
    let wdb = toBonds wd :: USD (Amt Bonds)
        wds = toStocks $ minZero $ gains (toAmount b.bonds) (toBonds wd) :: USD (Amt Stocks)
    in  Portfolio (addToBalance (loss wds) b.stocks) (addToBalance (loss wdb) b.bonds)



-- rebalancing!
-- you can only call this once
-- how can I force myself to only call it once?
rebalance :: (Balances -> Balances) -> Actions ()
rebalance rb = do
    modify $ \st -> st
      { _rebalance = rb
      }

balances :: Actions Balances
balances = do
    asks (._balances)

now :: Actions History
now = do
    asks (._now)

history :: Actions (NonEmpty History)
history = do
    asks (._history)


onYears :: [Int] -> Actions () -> Actions ()
onYears yrs action = do
    NumYears ye <- yearsElapsed
    when (ye `elem` yrs) action

startYear :: Actions Year
startYear = do
    hs <- history
    pure $ (.year) $ head hs

yearsElapsed :: Actions NumYears
yearsElapsed = do
    ys <- startYear
    yc <- (.year) <$> now
    pure $ numYears $ fromYear yc - fromYear ys

yearsLeft :: Actions NumYears
yearsLeft = do
    Year ye <- asks (._end)
    Year yc <- (.year) <$> now
    pure $ NumYears $ ye - yc

lastYear :: Actions (Maybe YearStart)
lastYear = do
    headMay <$> asks (._pastYears)

lastWithdrawal :: Actions (Maybe (USD (Amt Withdrawal)))
lastWithdrawal = do
    fmap (.withdrawal) <$> lastYear


noActions :: Actions ()
noActions = pure ()

noChanges :: Balances -> Changes
noChanges _ = Portfolio mempty mempty

















calcReturns :: History -> Balances -> Changes
calcReturns h b =
    let ds = fromUSD $ amount h.returns.stocks b.stocks :: USD (Amt Stocks)
        db = fromUSD $ amount h.returns.bonds b.bonds :: USD (Amt Bonds)
    in Portfolio ds db

withdrawals :: SimResult -> Sorted (USD (Amt Withdrawal))
withdrawals sr = sorted $ fmap (.withdrawal) sr.years


-- num years is the current year?
npvExpenses :: NumYears -> [Transaction Expense] -> USD (Amt Expense)
npvExpenses cy exs = sum $ map (npvExpense cy) exs

-- years last
-- expYear :: NumYears -> Transaction Expense -> USD (Amt Expense)
-- can we turn it into a list?
npvExpense :: NumYears -> Transaction Expense -> USD (Amt Expense)
npvExpense cy ex =
  let past = max 0 (fromNumYears cy - fromNumYears ex.start) :: Int
  in sum $ drop past $ replicate ex.duration ex.amt



