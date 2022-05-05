

import Invest.Prelude
import Invest.Types
import Invest.Lib
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
  test "withdrawalBonds" assertWithdrawalBonds
  test "withdrawalStocks" assertWithdrawalStocks
  test "inflation" assertInflation
  test "simWithdrawlEnd" assertSimWithdrawEnd
  test "simEndBalance" assertSimEndBalance
  test "rebalance" assertRebalance
  test "standard" assertStandard



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
  let h = History (Year 1872) (pct 10.0) (pct 1.0)
  let b = Portfolio (usd 100) (usd 100)

  expect "returns to match history" $ do
    calcReturns h b === Portfolio (usd 10) (usd 1)

  expect "returns to work on million portfolio" $ do
    calcReturns h million === Portfolio (usd 60000) (usd 4000)

  expect "apply returns should add up" $ do
    let ret = calcReturns h million
    apply ret million === Portfolio (usd 660000) (usd 404000)

    


assertWithdrawalStocks :: Test ()
assertWithdrawalStocks = do
  let b = Portfolio (usd 600) (usd 400)
  let wa = amount swr4 (total b)
  expect "withdrawal amount to be 4%" $ do
    wa      === usd (40)

  expect "loss to invert the amount" $ do
    loss wa === usd (-40)
  
  expect "loss to always be negative, even when starting negative" $ do
    loss (usd (-20)) === usd (-20)

  expect "gain to always be positive" $ do
    gain (usd (-20)) === usd 20
    gain (usd 30) === usd 30

  expect "should withdraw 0 from stocks and 40 from bonds" $ do
    (withdrawBondsFirst wa b).stocks === usd 0
    (withdrawBondsFirst wa b).bonds === usd (-40)

assertWithdrawalBonds :: Test ()
assertWithdrawalBonds = do
  let b = Portfolio (usd 990) (usd 10)
  let wa = amount swr4 (total b)

  expect "to withdraw from stocks when bonds are insufficient" $ do
    wa      === usd (40)
    (withdrawBondsFirst wa b).stocks === usd (-40)
    (withdrawBondsFirst wa b).bonds === usd 0




assertPercent :: Test ()
assertPercent = do

  expect "to use 3 decimal places" $ do
    toFloat (pct 25.0140) === 0.25014

  expect "to calculate gains" $ do
    gainsPercent (usd 100) (usd 110) === pct 10.0





assertHistory :: Test ()
assertHistory = do
  let hr1 = HistoryRow (Year 1871) 1 (usd 1.00) (usd 1.00) (pct 10.0)
  let hr2 = HistoryRow (Year 1872) 1 (usd 1.10) (usd 1.10) (pct 20.0)
  let hs = toHistories [hr1, hr2]

  expect "to combine into one history entry" $ do
    length hs === 1

  expect "history entry to match second year gains" $ do
    map (.year) hs === [Year 1872]
    map (.stocks) hs === [pct 10.0]
    map (.bonds) hs === [pct 10.0]


assertInflation :: Test ()
assertInflation = do
  let hr1 = HistoryRow (Year 1871) 1 (usd 100) (usd 100) (pct 10)
  let hr2 = HistoryRow (Year 1872) 1 (usd 110) (usd 101) (pct 20)
  let hr3 = HistoryRow (Year 1873) 1 (usd 95)  (usd 101) (pct 20)
  let hs = toHistories [hr1, hr2, hr3]

  -- we want to only withdraw
  let sim = simulation (standardWithdraw4 million) million hs
  
  expect "3 histroy rows to 2 histories" $ do
    length sim.years === 2

  expect "withdraw from bonds first" $
    map (\r -> r.actions.stocks) sim.years === [usd 0, usd 0]

  expect "keep withdrawals constant from year to year in real dollars" $
    map (\r -> r.actions.bonds)  sim.years === [usd (-40000), usd (-40000)]


assertSimEndBalance :: Test ()
assertSimEndBalance = do
  let hr1 = HistoryRow (Year 1871) 1 (usd 1.00) (usd 1.00) (pct 10.0)
  let hr2 = HistoryRow (Year 1872) 1 (usd 1.10) (usd 1.01) (pct 20.0)
  let hs = toHistories [hr1, hr2]
  let sim = simulation noActions million hs
  
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
  let hr1 = HistoryRow (Year 1871) 1 (usd 1.00) (usd 1.00) (pct 10.0)
  let hr2 = HistoryRow (Year 1872) 1 (usd 1.10) (usd 2.00) (pct 10.0)
  let hs = toHistories [hr1, hr2]

  -- just below the threshhold to 
  let bal = Portfolio (usd 1000) (usd 30)
  let sim = simulation (standardWithdraw4 bal) bal hs
  let wda = amount swr4 (total bal)
  
  [y] <- pure sim.years

  expect "returns should put us over the threshold for withdrawal amount" $ do
    y.returns === Portfolio (usd 100) (usd 30)

  expect "withdraw from bonds first because they grow" $ do
    y.actions === Portfolio (usd 0) (loss wda)

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

  expect "rebalance 100/0 by 40 and 40" $ do
    rebalanceFixed (pct 60) (pct 40) bs === Portfolio (usd (-40)) (usd (40))

  expect "rebalance 0/100 by 60 and 60" $ do
    rebalanceFixed (pct 60) (pct 40) bb === Portfolio (usd (60)) (usd (-60))

  expect "rebalance correct by 0" $ do
    rebalanceFixed (pct 60) (pct 40) be === Portfolio mempty mempty


assertStandard :: Test ()
assertStandard = do
  let start = Portfolio (usd 600) (usd 400)
  let bal = Portfolio (usd 800)    (usd 400)
  let wda = staticWithdrawalAmount swr4 start

  -- ok, so what should happen?


  expect "withdrawal amount should be 4%" $ do
    wda === (usd 40)

  expect "withdrawBondsFirst 4%" $ do
    let const4Percent = loss $ staticWithdrawalAmount swr4 start :: USD Amount
    let ch = withdrawBondsFirst const4Percent bal
    ch.bonds === usd (-40)

  expect "rebalance" $ do
    let ch = rebalanceFixed (pct 60) (pct 40) bal
    ch.bonds === usd (80)
    ch.stocks === usd (-80)
  
  expect "withdrawal " $ do
    -- let changes = runActions (standard6040Withdraw4 start) bal
    let bal' = runActions bal (standardWithdraw4 start)
    bal'.stocks === usd 800
    bal'.bonds === usd 360

  expect "rebalance" $ do
    let bal' = runActions bal standardRebalance6040
    bal'.stocks === usd 720
    bal'.bonds === usd 480


  -- run full standard
  let bal' = runActions bal (standard6040Withdraw4 start)
  let chs  = changes bal bal'

  expect "withdrawal should result in net -40" $ do
    total chs === loss wda

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

  let ch = Portfolio (usd 20) (usd 30)

  expect "stocks should be the sum of both changes" $ do
    let fin = runActions bal $ do
                change $ const ch
                change $ const ch

    fin.stocks === usd 140






