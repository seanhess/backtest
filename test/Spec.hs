

import Backtest.Prelude
import Backtest.Types
import Backtest.Lib
import Backtest.History
import Backtest.MSWR
import Backtest.Simulation
import Backtest.Strategy
import Backtest.Strategy.ABW
import qualified Backtest.Types.Pct as Pct

import Control.Monad.Catch (try, throwM)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, ask)
import Control.Monad.State (runState)
import Data.List (intercalate)


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

main :: IO ()
main = do
  putStrLn "Running Tests"
  test "Percent" assertPercent
  test "Amount" assertAmount
  test "Returns" assertReturns
  test "History" assertHistory
  test "Actions" assertActions
  test "withdrawal" assertWithdrawal
  test "rebalance" assertRebalance
  test "inflation" assertInflation
  test "simWithdrawEnd" assertSimWithdrawEnd
  test "simEndBalance" assertSimEndBalance
  test "standard" assertStandard
  test "primeHarvesting" assertPrimeHarvesting
  test "primeNew" assertPrimeNew
  test "testBands" assertBands
  test "testPMT" assertPMT
  test "testABW" assertABW



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


assertAmount :: Test ()
assertAmount = do
  expect "usd to be equal" $ do
    usd 100 === usd 100

  expect "amount to be equal to percent" $ do
    amount (pct 10.0) (usd 100) === usd 10
    amount (pct 1.0) (usd 100) === usd 1

  expect "hundredths to count" $ do
    amount (pct 0.01) (usd 600000) === usd 60

  expect "close dollars to round" $ do
    dollars (usd 15999.98) === 16000


assertReturns :: Test ()
assertReturns = do
  let h = History (Year 1872) (pct 10.0) (pct 1.0) (CAPE 10)
  let b = Portfolio (usd 100) (usd 100)

  expect "returns to match history" $ do
    calcReturns h b === Portfolio (usd 110) (usd 101)

  expect "apply returns should add up" $ do
    let ret = calcReturns h million60
    dollars ret.stocks === 660000
    dollars ret.bonds === 404000


    


assertWithdrawal :: Test ()
assertWithdrawal = do
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












assertPercent :: Test ()
assertPercent = do

  expect "to use 3 decimal places" $ do
    Pct.toFloat (pct 25.0140) === 0.25014

  expect "to calculate gains" $ do
    gainsPercent (usd 100) (usd 110) === pct 10.0





assertHistory :: Test ()
assertHistory = do
  let hr1 = HistoryRow (Year 1871) 1 (usd 1.00) (usd 1.00) (pct 10.0) (Just $ CAPE 10)
  let hr2 = HistoryRow (Year 1872) 1 (usd 1.10) (usd 1.10) (pct 20.0) (Just $ CAPE 10)
  let hs = toHistories [hr1, hr2]

  expect "to combine into one history entry" $ do
    length hs === 1

  expect "history entry to match second year gains" $ do
    map (.year) hs === [Year 1872]
    map (.stocks) hs === [pct 10.0]
    map (.bonds) hs === [pct 10.0]


assertInflation :: Test ()
assertInflation = do
  let hr1 = HistoryRow (Year 1871) 1 (usd 100) (usd 100) (pct 10) (Just $ CAPE 10)
  let hr2 = HistoryRow (Year 1872) 1 (usd 110) (usd 101) (pct 20) (Just $ CAPE 10)
  let hr3 = HistoryRow (Year 1873) 1 (usd 95)  (usd 101) (pct 20) (Just $ CAPE 10)
  let hs = toHistories [hr1, hr2, hr3]

  -- we want to only withdraw
  let sim = simulation million50 (withdraw4 million50) hs
  
  expect "3 histroy rows to 2 histories" $ do
    length sim.years === 2

  expect "withdraw from bonds first" $
    map (\r -> r.actions.stocks) sim.years === [usd 0, usd 0]

  expect "keep withdrawals constant from year to year in real dollars" $
    map (\r -> r.actions.bonds)  sim.years === [usd (-40000), usd (-40000)]


assertSimEndBalance :: Test ()
assertSimEndBalance = do
  let hr1 = HistoryRow (Year 1871) 1 (usd 1.00) (usd 1.00) (pct 10.0) (Just $ CAPE 10)
  let hr2 = HistoryRow (Year 1872) 1 (usd 1.10) (usd 1.01) (pct 20.0) (Just $ CAPE 10)
  let hs = toHistories [hr1, hr2]
  let sim = simulation million60 noActions hs
  
  [y] <- pure sim.years

  expect "stock return to be exactly 10%" $ do
    [h] <- pure hs
    h.stocks === pct 10.0

  expect "stocks to end 10% higher than start" $ do
    dollars (y.end.stocks) === 660000

  expect "bonds to end 1% higher than start" $ do
    dollars (y.end.bonds) === 404000


assertSimWithdrawEnd :: Test ()
assertSimWithdrawEnd = do
  -- 10% gain in both
  let hr1 = HistoryRow (Year 1871) 1 (usd 1.00) (usd 1.00) (pct 10.0) (Just $ CAPE 10)
  let hr2 = HistoryRow (Year 1872) 1 (usd 1.10) (usd 2.00) (pct 10.0) (Just $ CAPE 10)
  let hs = toHistories [hr1, hr2]

  -- just below the threshhold to 
  let bal = Portfolio (usd 1000) (usd 30)
  let sim = simulation bal (withdraw4 bal) hs
  let wda = amount swr4 (total bal)
  
  [y] <- pure sim.years

  expect "returns should put us over the threshold for withdrawal amount" $ do
    y.returns === Portfolio (usd 100) (usd 30)

  expect "withdraw from bonds first because they grow" $ do
    y.actions === Portfolio (usd 0) (loss $ toBonds wda)

  expect "withdrawals should be equal to wda" $ do
    y.withdrawals === loss wda

  -- History {year = 1872, stocks = 13.907, bonds = 3.1000}
  -- Portfolio {stocks = $600000.00, bonds = $400000.00}

  -- expect "historical returns (was causing error)" $ do
  --   let h' = History (Year 1872) (Pct 0 0) (Pct 3 100)
  --   calcReturns h' million === Portfolio (usd 0) (usd 12400)


assertRebalance :: Test ()
assertRebalance = do
  let bs = Portfolio (usd 100) (usd 0)
  let bb = Portfolio (usd 0) (usd 100)
  let be = Portfolio (usd 60) (usd 40)

  let rb = rebalanceFixed (pct 60) bs

  expect "rebalance calculates targets" $ do
    rb === be

  expect "rebalance 100/0 by 40 and 40" $ do
    changes bs rb === Portfolio (usd (-40)) (usd (40))

  expect "rebalance 0/100 by 60 and 60" $ do
    changes bb rb === Portfolio (usd (60)) (usd (-60))

  expect "rebalance correct by 0" $ do
    changes be rb === Portfolio mempty mempty


assertStandard :: Test ()
assertStandard = do
  let start = Portfolio (usd 600) (usd 400)
  let bal = Portfolio   (usd 800) (usd 400)
  let wda = staticWithdrawal swr4 start
  let h = History (Year 1872) (pct 10.0) (pct 1.0) (CAPE 10)
  let y = h.year


  -- ok, so what should happen?


  expect "withdrawal amount should be 4%" $ do
    wda === (usd 40)

  expect "bondsFirst 4%" $ do
    let const4Percent = loss $ staticWithdrawal swr4 start :: USD Amt Withdrawal
    let bal' = bondsFirst const4Percent bal
    let ch = changes bal bal'
    ch.bonds === usd (-40)

  expect "rebalance" $ do
    let bal' = rebalanceFixed (pct 60) bal
    let ch = changes bal bal'
    ch.bonds === usd (80)
    ch.stocks === usd (-80)
  
  expect "withdrawal " $ do
    let bal' = runActions y h bal (withdraw4 start)
    bal'.stocks === usd 800
    bal'.bonds === usd 360

  expect "rebalance" $ do
    let bal' = runActions y h bal $ rebalancePct (pct 60)
    bal'.stocks === usd 720
    bal'.bonds === usd 480


  -- run full standard
  let bal' = runActions y h bal (withdraw4 start >> rebalancePct (pct 60))
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

    
assertActions :: Test ()
assertActions = do
  let bal = Portfolio (usd 100) (usd 200)
  let h = History (Year 1872) (pct 10.0) (pct 1.0) (CAPE 10)
  let y = Year 1872

  let ch = \b -> Portfolio (addToBalance (usd 20) b.stocks) (addToBalance (usd 30) b.bonds)

  expect "stocks should be the sum of both changes" $ do
    let fin = runActions y h bal $ do
                rebalance ch
                rebalance ch

    fin.stocks === usd 140


assertPrimeHarvesting :: Test ()
assertPrimeHarvesting = do
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


assertPrimeNew :: Test ()
assertPrimeNew = do
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


assertBands :: Test ()
assertBands = do
  expect "diffAbsPercent normal" $ do
    diffAbsPercent (pct 60) (Portfolio (usd 62) (usd 38)) === pct 2

  expect "diffAbsPercent high" $ do
    diffAbsPercent (pct 60) (Portfolio (usd 65) (usd 35)) === pct 5

  expect "diffAbsPercent low" $ do
    diffAbsPercent (pct 40) (Portfolio (usd 65) (usd 35)) === pct 25

  expect "diffRelPercent normal" $ do
    diffRelPercent (pct 50) (Portfolio (usd 55) (usd 45))  === pct 10

  expect "diffRelPercent high" $ do
    diffRelPercent (pct 50) (Portfolio (usd 65) (usd 35))  === pct 30

  expect "diffRelPercent low" $ do
    diffRelPercent (pct 50) (Portfolio (usd 30) (usd 70))  === pct 40


assertPMT :: Test ()
assertPMT = do
  expect "same results as retirement spreadsheet" $ do
    (round $ pmt' 0.0237 61 (-1) * 10000) === (-304)

  expect "same results as spreadsheet function" $ do
    (round $ pmt 0.01 360 100000 * 100) === 102861

  expect "using calcWithdrawal" $ do
    calcWithdrawal 61 (pct 2.37) === pct 3.045


assertABW :: Test ()
assertABW = do
  expect "total return to match calculation" $ do
    let ps = pct 50
    let rs = pct 10
    let rb = pct 2
    (totalReturn [weightedReturn ps rs, weightedReturn ps rb]) === pct 6

  expect "estimated return stocks to match CAPE" $ do
    estimatedReturnStocks (CAPE 40) === pct 2.5

  expect "estimated return total to match calc" $ do
    (estimatedReturnTotal million50 (CAPE 40)) === pct 2.25



  let p = million (pct 100)
  let years = 50
  let cape = CAPE 40
  let ret = estimatedReturnTotal p cape
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
  let ye = Year 1950
  let h = History y (pct 0.0) (pct 0.0) (CAPE 40)
  let st = runActionState ye h p withdrawABW

  expect "changes to equal withdrawal amount" $ do
    st._balances.bonds === usd 0
    st._balances.stocks === addToBalance (loss wda) p.stocks

  expect "withdrawal amount to be the same" $ do
    st._withdrawal === wda
