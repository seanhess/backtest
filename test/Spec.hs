

import Invest.Prelude
import Invest.Types
import Invest.Lib
import Control.Monad.Catch (try, throwM)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, ask)
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
  test "Percent" assertPercent
  test "Amount" assertAmount
  test "Returns" assertReturns
  test "History" assertHistory
  test "withdrawalBonds" assertWithdrawalBonds
  test "withdrawalStocks" assertWithdrawalStocks
  test "inflation" assertInflation
  test "simWithdrawlEnd" assertSimWithdrawEnd
  test "simEndBalance" assertSimEndBalance
  test "rebalance" assertRebalance




-- are they ... computations?
-- they are state?
-- they set the state, then exit out, no?

-- the only assertions should be IN the assertions
expect :: String -> Assertion () -> Test ()
expect expectation asrt = do

  testName <- ask

  res <- try $ liftIO asrt
  case res of
    Left (Failure r vals) -> do
      putStrLn $ "[x] " <> expectation
      putStrLn $  "     * Expected " <> show r <> ": \n" <> showInfo vals
      putStrLn ""

    Right _ -> do
      pure ()
      -- putStrLn $ "[√] " <> expectation

  where
    showOne a =
      "     | " <> a

    showInfo :: Info -> String
    showInfo (Vals a b) = 
        intercalate "\n" $ map showOne [a, b]

    showInfo (Val a) = 
        showOne a



test :: String -> Test () -> IO ()
test nm t = do
  putStrLn ""
  putStrLn nm
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
    amount (Pct 10 0) (usd 100) === usd 10
    amount (Pct 1 0) (usd 100) === usd 1

  expect "hundredths to count" $ do
    amount (Pct 0 01) (usd 600000) === usd 6


assertReturns :: Test ()
assertReturns = do
  let h = History (Year 1872) (Pct 10 0) (Pct 1 0)
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
  
  -- expect "loss to always be negative, even when starting negative" $ do
  --   loss (usd (-20)) === usd (-20)

  expect "should withdraw 0 from stocks and 40 from bonds" $ do
    (bondsFirst wa b).stocks === usd 0
    (bondsFirst wa b).bonds === usd (-40)

assertWithdrawalBonds :: Test ()
assertWithdrawalBonds = do
  let b = Portfolio (usd 990) (usd 10)
  let wa = amount swr4 (total b)

  expect "to withdraw from stocks when bonds are insufficient" $ do
    wa      === usd (40)
    (bondsFirst wa b).stocks === usd (-40)
    (bondsFirst wa b).bonds === usd 0




assertPercent :: Test ()
assertPercent = do
  expect "to be equal" $ do
    pctFromFloat 0.2 === pctFromFloat 0.2

  expect "to calculate from float" $ do
    pctFromFloat 0.28690 === Pct 28 690

  expect "go back to float" $ do
    pctToFloat (Pct 28 690) === 0.28690

  expect "to use 3 decimal places" $ do
    pctFromFloat 0.25014 === Pct 25 014

  expect "to calculate gains" $ do
    gainsPercent (USD 100) (USD 110) === Pct 10 0



assertHistory :: Test ()
assertHistory = do
  let hr1 = HistoryRow (Year 1871) 1 (USD 100) (USD 100) (Pct 10 0)
  let hr2 = HistoryRow (Year 1872) 1 (USD 110) (USD 110) (Pct 20 0)
  let hs = toHistories [hr1, hr2]

  expect "to combine into one history entry" $ do
    length hs === 1

  expect "history entry to match second year gains" $ do
    map (.year) hs === [Year 1872]
    map (.stocks) hs === [Pct 10 0]
    map (.bonds) hs === [Pct 10 0]


assertInflation :: Test ()
assertInflation = do
  let hr1 = HistoryRow (Year 1871) 1 (USD 100) (USD 100) (Pct 10 0)
  let hr2 = HistoryRow (Year 1872) 1 (USD 110) (USD 101) (Pct 20 0)
  let hr3 = HistoryRow (Year 1873) 1 (USD 95)  (USD 101) (Pct 20 0)
  let hs = toHistories [hr1, hr2, hr3]
  let sim = simulation (staticPercent swr4 million) bondsFirst port6040 million hs
  
  expect "3 histroy rows to 2 histories" $ do
    length sim.years === 2

  -- withdrawals are made from bonds first
  expect "withdraw from bonds first" $
    map (\r -> r.withdrawals.stocks) sim.years === [usd 0, usd 0]

  -- withdrawals remain constant from year to year in real dollars
  expect "keep withdrawals constant from year to year in real dollars" $
    map (\r -> r.withdrawals.bonds)  sim.years === [usd (-40000), usd (-40000)]


assertSimEndBalance :: Test ()
assertSimEndBalance = do
  let hr1 = HistoryRow (Year 1871) 1 (USD 100) (USD 100) (Pct 10 0)
  let hr2 = HistoryRow (Year 1872) 1 (USD 110) (USD 101) (Pct 20 0)
  let hs = toHistories [hr1, hr2]
  let sim = simulation (const (usd 0)) noWithdraw noRebalance million hs
  
  [y] <- pure sim.years

  expect "stock return to be exactly 10%" $ do
    [h] <- pure hs
    h.stocks === Pct 10 0

  expect "stocks to end 10% higher than start" $ do
    y.end.stocks === usd 660000

  expect "bonds to end 1% higher than start" $ do
    y.end.bonds === usd 404000


assertSimWithdrawEnd :: Test ()
assertSimWithdrawEnd = do
  -- 10% gain in both
  let hr1 = HistoryRow (Year 1871) 1 (USD 100) (USD 100) (Pct 10 0)
  let hr2 = HistoryRow (Year 1872) 1 (USD 110) (USD 200) (Pct 10 0)
  let hs = toHistories [hr1, hr2]

  -- just below the threshhold to 
  let bal = Portfolio (usd 1000) (usd 30)
  let sim = simulation (staticPercent swr4 bal) (bondsFirst) noRebalance bal hs
  let wda = amount swr4 (total bal)
  
  [y] <- pure sim.years

  expect "returns should put us over the threshold for withdrawal amount" $ do
    y.returns === Portfolio (usd 100) (usd 30)

  expect "withdraw from bonds first because they grow" $ do
    y.withdrawals === Portfolio (usd 0) (loss wda)


assertRebalance :: Test ()
assertRebalance = do
  let bs = Portfolio (usd 100) (usd 0)
  let bb = Portfolio (usd 0) (usd 100)
  let be = Portfolio (usd 60) (usd 40)

  expect "rebalance 100/0 by 40 and 40" $ do
    fixedPortfolio (Pct 60 0) (Pct 40 0) bs === Portfolio (usd (-40)) (usd (40))

  expect "rebalance 0/100 by 60 and 60" $ do
    fixedPortfolio (Pct 60 0) (Pct 40 0) bb === Portfolio (usd (60)) (usd (-60))

  expect "rebalance correct by 0" $ do
    fixedPortfolio (Pct 60 0) (Pct 40 0) be === Portfolio zero zero

usd :: Float -> USD a
usd f = fromFloat f