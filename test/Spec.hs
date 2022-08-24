{-# LANGUAGE OverloadedLists #-}


import Backtest.Prelude
import Backtest.Types
import Backtest.Lib
import Backtest.History
import Backtest.MSWR
import Backtest.Simulation
import Backtest.Strategy
import Backtest.Strategy.ABW
import Backtest.Strategy.Peak
import Backtest.Debug
import Backtest.Optimize
import qualified Backtest.Types.Pct as Pct

import Control.Monad.Catch (try, throwM)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, runReader, ask)
import Control.Monad.State (runState)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE


data Failure
  = Failure Expectation Info
  deriving (Show)
instance Exception Failure

data Expectation
  = NotEqual
  | Equal
  deriving (Show)

data Info
  = Val String
  | Vals String String
  deriving (Show)

type Test = ReaderT String IO
type Assertion = IO

tests :: IO ()
tests = main

-- TODO withdrawal isn't zeroing out in my example
main :: IO ()
main = do
  putStrLn "Running Tests"
  test "testPercent" testPercent
  test "testAmount" testAmount
  test "testReturns" testReturns
  test "testHistory" testHistory
  test "testActions" testActions
  test "testWithdrawal" testWithdrawal
  test "testRebalance" testRebalance
  test "testInflation" testInflation
  test "testSimEndBalance" testSimEndBalance
  test "testSimulation" testSimulation
  test "testStandard" testStandard
  test "testPrimaeHarvesting" testPrimeHarvesting
  test "testPrimeNew" testPrimeNew
  test "testBands" testBands
  test "testPMT" testPMT
  test "testABW" testABW
  test "testPmtFluctate" testFluctate
  test "testPeak" testPeak
  test "testExpenses" testExpenses
  -- test "testOptimize" testOptimize



-- the only assertions should be IN the assertions
expect :: String -> Assertion () -> Test ()
expect expectation asrt = do

  testName <- ask

  res <- try $ liftIO asrt
  case res of
    Left (Failure r vals) -> do
      putStrLn ""
      putStrLn $ "[x] " <> testName <> " - " <> expectation
      putStrLn $  "   * Expected " <> show r <> ": \n" <> showInfo vals

    Right _ -> do
      pure ()
      -- putStrLn $ "[√] " <> expectation

  where
    showOne a =
      "   | " <> a

    showInfo :: Info -> String
    showInfo (Vals a b) = 
        intercalate "\n" $ map showOne [a, b]

    showInfo (Val a) = 
        showOne a



test :: String -> Test () -> IO ()
test nm t = do
  runReaderT t nm



(===) :: (Show a, Eq a) => a -> a -> Assertion () 
a === b = do
  if a == b
    then pure ()
    else throwM $ Failure Equal $ Vals (show a) (show b)

(/==) :: (Show a, Eq a) => a -> a -> Assertion () 
a /== b = do
  if a /= b
    then pure ()
    else throwM $ Failure NotEqual $ Vals (show a) (show b)


testAmount :: Test ()
testAmount = do
  expect "usd to be equal" $ do
    usd 100 === usd 100

  expect "amount to be equal to percent" $ do
    amount (pct 10.0) (usd 100) === usd 10
    amount (pct 1.0) (usd 100) === usd 1

  expect "hundredths to count" $ do
    amount (pct 0.01) (usd 600000) === usd 60

  expect "close dollars to round" $ do
    dollars (usd 15999.98) === 16000


testReturns :: Test ()
testReturns = do
  let h = History (Year 1900) (Portfolio (pct 10.0) (pct 1.0)) mempty (CAPE 10)
  let b = Portfolio (usd 100) (usd 100)

  expect "returns to match history" $ do
    calcReturns h b === Portfolio (usd 10) (usd 1)

  expect "apply returns should add up" $ do
    let ret = calcReturns h thousand60
    dollars ret.stocks === 60
    dollars ret.bonds === 4


    


testWithdrawal :: Test ()
testWithdrawal = do
  let b = Portfolio (usd 600) (usd 400)
  let wa = staticWithdrawal swr4 b
  expect "withdrawal amount to be 4%" $ do
    wa      === usd (40)

  expect "loss to invert the amount" $ do
    loss wa === usd (-40)
  
  expect "loss to always be negative, even when starting negative" $ do
    loss (usd (-20)) === usd (-20)

  expect "gain to always be positive" $ do
    gain (usd (-20)) === usd 20
    gain (usd 30) === usd 30

  expect "withdraw 0 from stocks and 40 from bonds" $ do
    let ch = changes b (bondsFirst wa b)
    ch.stocks === usd 0
    ch.bonds === usd (-40)

  expect "withdraw from stocks if bonds are zero" $ do
    let bal = Portfolio (usd 1000) (usd 0)
    let wa' = staticWithdrawal swr4 bal
    let ch = changes bal (bondsFirst wa' bal)
    ch.stocks === usd (-40)
    ch.bonds === usd 0

  expect "withdraw from bonds until drained, rest from stocks" $ do
    let bal = Portfolio (usd 990) (usd 10)
    let wa' = amount swr4 (total bal)
    let bal' = bondsFirst wa bal :: Balances
    let chg = changes bal bal' :: Changes
    chg.stocks === usd (-30)
    chg.bonds === usd (-10)

  expect "withdraw everything if balance is insufficient" $ do
    let bal = Portfolio (usd 20) (usd 10)
    let wa' = amount swr4 (total bal)
    let bal' = bondsFirst wa bal :: Balances
    let chg = changes bal bal' :: Changes
    chg.stocks === usd (-20)
    chg.bonds === usd (-10)

  expect "withdraw everything with negative withdrawal amounts" $ do
    let bal = Portfolio (usd 7.80) (usd 4.58)
    let wda = loss $ usd 50000
    let bal' = bondsFirst wda bal
    bal'.stocks === mempty
    bal'.bonds === mempty

  expect "withdrawal is zero if nothing is remaining" $ do
    let bal = Portfolio (usd 0) (usd 0)
    let wa' = staticWithdrawal swr4 bal
    let bal' = bondsFirst wa bal
    let chg = changes bal bal'
    chg.stocks === usd 0
    chg.bonds === usd 0












testPercent :: Test ()
testPercent = do

  expect "to use 3 decimal places" $ do
    Pct.toFloat (pct 25.0140) === 0.25014

  expect "to calculate gains" $ do
    gainsPercent (usd 100) (usd 110) === pct 10.0





testHistory :: Test ()
testHistory = do
  -- HistoryRow is a snapshot of the market at the START of the year
  -- TODO should I be using month 12? Are they end of month values or beginning?
  let hr1 = HistoryRow (Year 1871) 1 (usd 1.00) (usd 1.00) (Just $ CAPE 10)
  let hr2 = HistoryRow (Year 1872) 1 (usd 1.10) (usd 1.10) (Just $ CAPE 20)
  let hs = toHistories [hr1, hr2]

  expect "to combine into one history entry" $ do
    length hs === 1

  expect "history entry is for second year" $ do
    fmap (.year) hs === [Year 1872]

  expect "history gains are diff between two years" $ do
    fmap ((.stocks) . (.returns)) hs === [pct 10.0]
    fmap ((.bonds) . (.returns)) hs === [pct 10.0]

  expect "CAPE ratio is for the start of the current year (from second row)" $ do
    fmap (.cape) hs === [CAPE 20]


testInflation :: Test ()
testInflation = do
  let hr1 = HistoryRow (Year 1900) 1 (usd 100) (usd 100) (Just $ CAPE 10)
  let hr2 = HistoryRow (Year 1901) 1 (usd 110) (usd 101) (Just $ CAPE 10)
  let hr3 = HistoryRow (Year 1902) 1 (usd 95)  (usd 101) (Just $ CAPE 10)
  let hs = toHistories [hr1, hr2, hr3]

  -- we want to only withdraw
  let sim = simulation thousand50 (withdraw4 thousand50) hs
  
  expect "3 histroy rows to 2 histories" $ do
    length sim.years === 2

  expect "withdraw from bonds first" $ do
    fmap (\r -> r.actions.stocks) sim.years === [usd 0, usd 0]

  expect "keep withdrawals constant from year to year in real dollars" $ do
    fmap (\r -> r.actions.bonds)  sim.years === [usd (-40), usd (-40)]


testSimEndBalance :: Test ()
testSimEndBalance = do

  -- This results in only 1872, with returns over 1871 and the current cape raito (20)
  let hr1 = HistoryRow (Year 1900) 1 (usd 1.00) (usd 1.00) (Just $ CAPE 10)
  let hr2 = HistoryRow (Year 1901) 1 (usd 1.10) (usd 1.01) (Just $ CAPE 20)
  let hs = toHistories [hr1, hr2]

  -- there are no withdrawals or rebalancing, so it is only returns
  let sim = simulation thousand60 noActions hs
  
  [y] <- pure sim.years

  expect "first year is 1901" $ do
    y.year === Year 1901
    y.history.cape === CAPE 20

  expect "no first year returns" $ do
    y.returns === Portfolio mempty mempty

  let hr3 = HistoryRow (Year 1902) 1 (usd 2.20) (usd 2.02) (Just $ CAPE 30)
  let hs' = toHistories [hr1, hr2, hr3]
  let sim' = simulation thousand60 noActions hs'

  expect "two years of history" $ do
    fmap (.year) hs' === [Year 1901, Year 1902]

  [_, h1903] <- pure hs'

  expect "1903 stock returns are 100%" $ do
    h1903.returns.stocks === pct 100

  expect "1903 bond returns are 100%" $ do
    h1903.returns.bonds === pct 100

  expect "two years of simulation. One at the beginning of 1901, and one at the beginning of 1903" $ do
    fmap (.year) sim'.years === [Year 1901, Year 1902]

  expect "stock end balance to be only 1903 returns" $ do
    dollars (sim'.endBalance.stocks) === 1200



testSimulation :: Test ()
testSimulation = do

  -- These are PREVIOUS YEARS gains
  -- CURRENT cape ratio 
  -- State at the beginning of the year
  --   past returns
  --   current cape ratio
  let h0 = History (Year 1900) (Portfolio (pct 10.0) (pct 1.0)) mempty (CAPE 10)
  let h1 = History (Year 1901) (Portfolio (pct 20.0) (pct 2.0)) mempty (CAPE 20)
  let h2 = History (Year 1902) (Portfolio (pct 30.0) (pct 2.0)) mempty (CAPE 30)

  let bal = Portfolio (usd 1000) (usd 0)

  -- withdraw, but don't rebalance
  let sim = simulation bal (withdraw4 bal) [h0, h1, h2]
  let y = head sim.years

  expect "should give results for all years of history" $ do
    fmap (.year) sim.years === [Year 1900, Year 1901, Year 1902]

  expect "start year should not have returns" $ do
    y.year === Year 1900
    y.returns === Portfolio mempty mempty

  expect "start year should use first CAPE ratio" $ do
    y.history.cape === CAPE 10

  let s1900 = 1000-40
  expect "start year should withdraw immediately with no returns" $ do
    y.returns === Portfolio mempty mempty
    y.end.stocks === usd s1900

  let r1901 = s1900 * 1.2
  let s1901 = r1901 - 40
  expect "second year should include returns from 1901 history entry, and withdrawal" $ do
    [_, y2, _] <- pure sim.years
    y2.returns === Portfolio (usd (r1901 - s1900)) mempty
    y2.end.stocks === usd s1901

  let r1902 = s1901 * 1.3
  let s1902 = r1902 - 40

  expect "third year should apply its returns and withdraw also" $ do
    [_, _, y3] <- pure sim.years
    y3.returns    === Portfolio (usd (r1902 - s1901)) mempty
    y3.end.stocks === usd s1902

  -- expect "three past years" $ do



testRebalance :: Test ()
testRebalance = do
  let bs = Portfolio (usd 100) (usd 0)
  let bb = Portfolio (usd 0) (usd 100)
  let be = Portfolio (usd 60) (usd 40)

  let rb = rebalanceFixed S60 bs

  expect "rebalance calculates targets" $ do
    rb === be

  expect "rebalance 100/0 by 40 and 40" $ do
    changes bs rb === Portfolio (usd (-40)) (usd (40))

  expect "rebalance 0/100 by 60 and 60" $ do
    changes bb rb === Portfolio (usd (60)) (usd (-60))

  expect "rebalance correct by 0" $ do
    changes be rb === Portfolio mempty mempty


testStandard :: Test ()
testStandard = do
  let start = Portfolio (usd 600) (usd 400)
  let bal = Portfolio   (usd 800) (usd 400)
  let wda = staticWithdrawal swr4 start
  let h = History (Year 1872) (Portfolio (pct 10.0) (pct 1.0)) mempty (CAPE 10)
  let y = h.year
  let ctx = SimContext [] y


  -- ok, so what should happen?


  expect "withdrawal amount should be 4%" $ do
    wda === (usd 40)

  expect "bondsFirst 4%" $ do
    let const4Percent = loss $ staticWithdrawal swr4 start :: USD (Amt Withdrawal)
    let bal' = bondsFirst const4Percent bal
    let ch = changes bal bal'
    ch.bonds === usd (-40)

  expect "rebalance" $ do
    let bal' = rebalanceFixed S60 bal
    let ch = changes bal bal'
    ch.bonds === usd (80)
    ch.stocks === usd (-80)
  
  expect "withdrawal " $ do

    -- just check running actions, no sim!
    let bal' = runTestActionsBal h bal (withdraw4 start)
    bal'.stocks === usd 800
    bal'.bonds === usd 360

  expect "rebalance" $ do
    let bal' = runTestActionsBal h bal $ rebalancePct S60
    bal'.stocks === usd 720
    bal'.bonds === usd 480


  -- run full standard
  let bal' = runTestActionsBal h bal (withdraw4 start >> rebalancePct S60)
  let chs  = changes bal bal'

  expect "withdrawal should result in net -40" $ do
    total chs === (toTotal $ loss wda)

  expect "stocks should be rebalanced off of new total" $ do
    chs.stocks === (loss $ usd 104)

  expect "bonds should be the same value, but with the withdrawal" $ do
    chs.bonds === usd (104 - 40)


  -- expect "stocks should be rebalanced off of new amount" $ do
  --   -- let (cgs, f) = runState (fromActions (test1 start)) noChanges

  --   expect "should "
  --     changes.stocks === usd (-104)
  --   changes.bonds === usd (64)

    
testActions :: Test ()
testActions = do
  let bal = Portfolio (usd 100) (usd 200)
  let h = History (Year 1900) (Portfolio (pct 10.0) (pct 1.0)) mempty (CAPE 10)

  let ch = \b -> Portfolio (addToBalance (usd 20) b.stocks) (addToBalance (usd (-20)) b.bonds)
  expect "stocks should be the sum of both changes" $ do
    let fin = runTestActionsBal h bal $ do
                rebalance ch
                withdraw $ usd 30

    fin.stocks === usd 120
    fin.bonds === usd 150

  expect "withdraw everything if balance is insufficient" $ do
    let bal' = Portfolio (usd 20) (usd 10)
    let wa = usd 40
    let ye = runTestActions h bal' (withdraw wa)
    ye.withdrawal === usd 30
    ye.end.stocks === usd 0
    ye.end.bonds === usd 0


testPrimeHarvesting :: Test ()
testPrimeHarvesting = do
  let start = Portfolio (usd 500) (usd 500)
  let bal = \n -> Portfolio (usd n) (usd 500)

  expect "do nothing if within 120% of stocks" $ do
    rebalancePrime start.stocks (bal 450) === bal 450
    rebalancePrime start.stocks (bal 500) === bal 500
    rebalancePrime start.stocks (bal 400) === bal 400
    rebalancePrime start.stocks (bal 300) === bal 300

  expect "do nothing if exactly 120% of stocks " $ do
    rebalancePrime start.stocks (bal 600) === bal 600

  expect "transfer excess to bonds" $ do
    rebalancePrime start.stocks (bal 700) === Portfolio (usd 600) (usd 600)

  expect "total is equal" $ do
    let b = bal 700
    total (rebalancePrime start.stocks b) === total b


testPrimeNew :: Test ()
testPrimeNew = do
  let start = Portfolio (usd 500) (usd 500)
  let bal = \n -> Portfolio (usd n) (usd 500)

  expect "do nothing if within 120% of stocks" $ do
    rebalancePrimeNew start.stocks (bal 450) === bal 450
    rebalancePrimeNew start.stocks (bal 500) === bal 500
    rebalancePrimeNew start.stocks (bal 400) === bal 400

  expect "do nothing if exactly 120% of stocks " $ do
    rebalancePrimeNew start.stocks (bal 600) === bal 600

  expect "transfer excess to bonds" $ do
    -- 700 500 should be 600 600. ds = (-100)
    rebalancePrimeNew start.stocks (bal 700) === Portfolio (usd 600) (usd 600)

  expect "transfer excess to stocks" $ do
    rebalancePrimeNew start.stocks (bal 300) === Portfolio (usd 400) (usd 400)

  expect "total is equal with excess stocks" $ do
    let b = bal 700
    total (rebalancePrime start.stocks b) === total b

  expect "total is equal with too few stocks" $ do
    let b = bal 300
    total (rebalancePrime start.stocks b) === total b

  expect "with no bonds, still do nothing" $ do
    let b = Portfolio (usd 500) (usd 0)
    rebalancePrimeNew start.stocks b === b

  expect "with no bonds but above, still transfer" $ do
    let b = Portfolio (usd 700) (usd 0)
    rebalancePrimeNew start.stocks b === Portfolio (usd 600) (usd 100)

  expect "with no bonds but below, do nothing" $ do
    let b = Portfolio (usd 300) (usd 0)
    rebalancePrimeNew start.stocks b === Portfolio (usd 300) (usd 0)


testBands :: Test ()
testBands = do
  expect "diffAbsPercent normal" $ do
    diffAbsPercent S60 (Portfolio (usd 62) (usd 38)) === pct 2

  expect "diffAbsPercent high" $ do
    diffAbsPercent S60 (Portfolio (usd 65) (usd 35)) === pct 5

  expect "diffAbsPercent low" $ do
    diffAbsPercent S50 (Portfolio (usd 65) (usd 35)) === pct 15

  expect "diffRelPercent normal" $ do
    diffRelPercent S50 (Portfolio (usd 55) (usd 45))  === pct 10

  expect "diffRelPercent high" $ do
    diffRelPercent S50 (Portfolio (usd 65) (usd 35))  === pct 30

  expect "diffRelPercent low" $ do
    diffRelPercent S50 (Portfolio (usd 30) (usd 70))  === pct 40


testPMT :: Test ()
testPMT = do
  expect "same results as retirement spreadsheet" $ do
    (round $ pmt' 0.0237 61 (-1) * 10000) === (-304)

  expect "same results as spreadsheet function" $ do
    (round $ pmt 0.01 360 100000 * 100) === 102861

  expect "using calcWithdrawal" $ do
    calcWithdrawal (numYears 60) (pct 2.37) === pct 3.067


testABW :: Test ()
testABW = do
  expect "total return to match calculation" $ do
    let ps = pct 50
    let rs = pct 10
    let rb = pct 2
    (totalReturn [weightedReturn ps rs, weightedReturn ps rb]) === pct 6

  expect "estimated return stocks to match CAPE" $ do
    estimatedReturnStocks (CAPE 40) === pct 2.5

  expect "estimated return total to match calc" $ do
    (estimatedReturnTotal thousand50 estimatedReturnBonds (estimatedReturnStocks $ CAPE 40)) === pct 2.25



  let p = thousand S100
  let years = NumYears 50
  let cape = CAPE 40
  let ret = estimatedReturnTotal p estimatedReturnBonds (estimatedReturnStocks cape)
  let wdp = calcWithdrawal years ret
  let wda = amount wdp p.stocks


  expect "return equals 1/cape" $ do
    ret === pct 2.5

  expect "withdrawal = pmt" $ do
    wdp === pct 3.44

  expect "withdrawal amount matches" $ do
    amortizedWithdrawal years cape p === wda


  -- check actual withdrawal
  let y = Year 1900
  let ye = Year 1950 -- the year we are out of money and take no actions
  let h = History y (Portfolio (pct 0.0) (pct 0.0)) mempty (CAPE 40)
  let ctx = SimContext [h] ye p
  let st = runSim ctx $ runYear h mempty $ do
              withdrawABW
              
  expect "withdrawal amount to be the same" $ do
    st.withdrawal === wda

  expect "changes to equal withdrawal amount" $ do
    st.end.bonds === usd 0
    st.end.stocks === addToBalance (loss wda) p.stocks


  let h1 = History (Year 1900) (Portfolio (pct 0.0) (pct 10.0)) mempty (CAPE 10)
  let h2 = History (Year 1901) (Portfolio (pct 0.0) (pct 20.0)) mempty (CAPE 20)
  let h3 = History (Year 1902) (Portfolio (pct 0.0) (pct 30.0)) mempty (CAPE 30)
  let hs = [h1, h2, h3]
  let bal = Portfolio (usd 1000) (usd 0)

  -- withdraw, but don't rebalance
  let sim = simulation bal withdrawABW [h1, h2, h3]

  expect "years to cover 1900-1902" $ do
    fmap (.year) sim.years === fmap (.year) hs

  expect "first year withdrawal to be based on CAPE 10" $ do
    let y1 = head sim.years
    y1.year === Year 1900
    let wdp1 = calcWithdrawal (numYears 3) (pctFromFloat (1/10))
    y1.withdrawal === amount wdp1 (total bal)

  expect "second year withdrawal to be based on CAPE 20" $ do
    (y1 : y2 :_) <- pure $ NE.toList sim.years
    y2.year === Year 1901
    let wpd2 = calcWithdrawal (numYears 2) (pctFromFloat (1/20))
    y2.withdrawal === amount wpd2 (total y1.end)

  expect "third year withdrawal to be based on CAPE 30" $ do
    [_,y2,y3] <- pure sim.years
    y3.year === Year 1902
    let wpd3 = calcWithdrawal (numYears 1) (pctFromFloat (1/30))
    y3.withdrawal === amount wpd3 (total y2.end)
    dollars (y3.withdrawal) === dollars (total y2.end)


testFluctate :: Test ()
testFluctate = do
  expect "to run known example" $ do
    runReturns [pct 0, pct 3, pct 3, pct 3] (usd 20) 10000 === 374

  expect "to run negative at the end" $ do
    runReturns [pct 0, pct 10, pct 10, pct 10] (usd 25) 10000 === (-1620)

  expect "to run without returns when negative" $ do
    runReturns [pct 0, pct 0, pct 10, pct 10] (usd 50) 10000 === (-15000)

  expect "to converge on example" $ do
    pmtFluctuate [pct 10, pct 10, pct 10, pct 10] (usd 100) === usd (23.98)

  expect "to converge when initial withdrawal is too high" $ do
    pmtFluctuate' [pct 0, pct 0, pct 0, pct 0] (usd 100) (usd 71.56) === usd 20

  expect "to converge with high end percent" $ do
    pmtFluctuate' [pct 0, pct 0, pct 0, pct 1000] (usd 100) (usd 71.56) === usd (24.44)

  expect "to withdraw everything with no returns" $ do
    pmtFluctuate [] (usd 100) === usd (100)


testPeak :: Test ()
testPeak = do

  let hr0 = History (Year 1900) mempty (Portfolio (usd 100) (usd 111)) (CAPE 20)
  let hr1 = History (Year 1901) mempty (Portfolio (usd 110) (usd 100)) (CAPE 20)
  let hr2 = History (Year 1902) mempty (Portfolio (usd 95)  (usd 101)) (CAPE 20)
  let hr3 = History (Year 1903) mempty (Portfolio (usd 100) (usd 100)) (CAPE 20)
  let hr4 = History (Year 1904) mempty (Portfolio (usd 120) (usd 100)) (CAPE 20)

  let rows = [hr0, hr1, hr2, hr3, hr4] :: NonEmpty History

  expect "to find the peak" $ do
    peakBalance (reverseTimeline hr3.year rows) thousand60 === Portfolio (usd 660) (usd 400)

  expect "to use current as peak" $ do
    peakBalance (reverseTimeline hr4.year rows) thousand50 === Portfolio (usd 500) (usd 500)

  expect "to find peak via bonds" $ do
    peakBalance (reverseTimeline hr3.year rows) thousand50 === Portfolio (usd 500) (usd 555)

testExpenses :: Test ()
testExpenses = do
  let cs1 = Transaction "Child Support" (NumYears 0) 3 (usd 9.6)
      cs2 = Transaction "Child Support" (NumYears 3) 2 (usd 6.4)
      cs3 = Transaction "Child Support" (NumYears 5) 3 (usd 3.4)
      kid = Transaction "Extra Kids"    (NumYears 0) 8 (usd 13.2)
  
  expect "to be sum of all if early" $ do
    npvExpense (NumYears 0) cs2 === (usd 6.4*2)

  expect "to be all if start" $ do
    npvExpense (NumYears 3) cs2 === (usd 6.4*2)

  expect "to be drop off" $ do
    npvExpense (NumYears 4) cs2 === (usd 6.4)

  expect "to be zero late" $ do
    npvExpense (NumYears 5) cs2 === (usd 0)

  expect "sum of all of them" $ do
    npvExpenses (NumYears 0) [cs1, cs2, cs3, kid] === usd 157.4

  expect "only later ones" $ do
    npvExpenses (NumYears 7) [cs1, cs2, cs3, kid] === usd (13.2+3.4)

  pure ()


runTestActions :: History -> Balances -> Actions () -> YearStart
runTestActions h bal acts =
  runSim (SimContext [h] (nextYear h.year) bal) $ do
    runYear h mempty acts

runTestActionsBal :: History -> Balances -> Actions () -> Balances
runTestActionsBal h bal acts = (.end) <$>
  runSim (SimContext [h] (nextYear h.year) bal) $ do
    runYear h mempty acts


-- testOptimize :: Test ()
-- testOptimize = do

--   hs <- liftIO loadHistories

--   forM_ (maximizeRate isSimValid (runSim' hs S100)) $ \(wr, res) -> do
--     print wr
--   -- expect "Nope" $ do

--   where
--     runSim' :: NonEmpty History -> Allocation -> Pct Withdrawal -> NonEmpty SimResult
--     runSim' hs al wr = fmap (simulation start (actions al wr)) (samples years hs)
--     start = thousand50
--     years = NumYears 50
--     actions al wr = do
--       withdraw $ staticWithdrawal wr start
--       rebalance $ rebalanceFixed al




