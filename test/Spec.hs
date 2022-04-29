

import Invest.Prelude
import Invest.Types
import Invest.Lib
import Control.Monad.Catch (try, throwM)
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

main :: IO ()
main = do
  test "Percent" assertPercent
  test "Amount" assertAmount
  test "Returns" assertReturns
  test "History" assertHistory
  test "Withdrawal Bonds" assertWithdrawalBonds
  test "Withdrawal Stocks" assertWithdrawalStocks
  test "Inflation" assertInflation
  test "Sim Withdraw End" assertSimWithdrawEnd
  test "Rebalance" assertRebalance



test :: (MonadIO m, MonadCatch m) => String -> m () -> m ()
test nm asrt = do
  res <- try asrt
  case res of
    Left (Failure r vals) -> do
      putStrLn ""
      putStrLn $ "[x] " <> nm
      putStrLn $ "---------------------"
      putStrLn $  "* Expected " <> show r <> ": \n" <> showInfo vals
      putStrLn ""

    Right _ -> do
      putStrLn $ "[√] " <> nm

  where
    showOne a =
      "| " <> a

    showInfo :: Info -> String
    showInfo (Vals a b) = 
        intercalate "\n" $ map showOne [a, b]

    showInfo (Val a) = 
        showOne a



(===) :: (MonadThrow m, MonadIO m, Show a, Eq a) => a -> a -> m () 
a === b = do
  if a == b
    then pure ()
    else throwM $ Failure Equal $ Vals (show a) (show b)

(/==) :: (MonadThrow m, MonadIO m, Show a, Eq a) => a -> a -> m () 
a /== b = do
  if a /= b
    then pure ()
    else throwM $ Failure NotEqual $ Vals (show a) (show b)


assertAmount :: IO ()
assertAmount = do
  usd 100 === usd 100
  amount (Pct 0.10) (usd 100) === usd 10
  amount (Pct 0.01) (usd 100) === usd 1


assertReturns :: IO ()
assertReturns = do
  let h = History (Year 1900) (Pct 0.10) (Pct 0.01)
  let b = Portfolio (usd 100) (usd 100)
  calcReturns h b === Portfolio (usd 10) (usd 1)


assertWithdrawalStocks :: IO ()
assertWithdrawalStocks = do
  let b = Portfolio (usd 600) (usd 400)
  let wa = amount swr4 (total b)
  wa      === usd (40)
  loss wa === usd (-40)
  (bondsFirst wa b).stocks === usd 0
  (bondsFirst wa b).bonds === usd (-40)

assertWithdrawalBonds :: IO ()
assertWithdrawalBonds = do
  let b = Portfolio (usd 990) (usd 10)
  let wa = amount swr4 (total b)
  wa      === usd (40)
  (bondsFirst wa b).stocks === usd (-40)
  (bondsFirst wa b).bonds === usd 0




assertPercent :: IO ()
assertPercent = do
  Pct 0.2 === Pct 0.2
  Pct 0.10 /== Pct 0.00
  gainsPercent (USD 100) (USD 110) === Pct 0.1


assertHistory :: IO ()
assertHistory = do
  let hr1 = HistoryRow (Year 1871) 1 (USD 100) (USD 100) (Pct 0.1)
  let hr2 = HistoryRow (Year 1872) 1 (USD 110) (USD 110) (Pct 0.2)
  let hs = toHistories [hr1, hr2]

  length hs === 1

  map (.year) hs === [Year 1872]
  map (.stocks) hs === [Pct 0.10]
  map (.bonds) hs === [Pct 0.10]

assertInflation :: IO ()
assertInflation = do
  let hr1 = HistoryRow (Year 1871) 1 (USD 100) (USD 100) (Pct 0.1)
  let hr2 = HistoryRow (Year 1872) 1 (USD 110) (USD 101) (Pct 0.2)
  let hr3 = HistoryRow (Year 1873) 1 (USD 95)  (USD 101) (Pct 0.2)
  let hs = toHistories [hr1, hr2, hr3]
  let sim = simulation bondsFirst port6040 million hs
  
  length sim.years === 2

  -- withdrawals are made from bonds first
  map (\r -> r.withdrawals.stocks) sim.years === [usd 0, usd 0]

  -- withdrawals remain constant from year to year in real dollars
  map (\r -> r.withdrawals.bonds)  sim.years === [usd (-40000), usd (-40000)]

assertSimWithdrawEnd :: IO ()
assertSimWithdrawEnd = do
  -- 10% gain in both
  let hr1 = HistoryRow (Year 1871) 1 (USD 100) (USD 100) (Pct 0.1)
  let hr2 = HistoryRow (Year 1872) 1 (USD 110) (USD 200) (Pct 0.1)
  let hs = toHistories [hr1, hr2]

  -- just below the threshhold to 
  let bal = Portfolio (usd 1000) (usd 30)
  let sim = simulation (bondsFirst) port6040 bal hs
  let wda = amount swr4 (total bal)
  
  [y] <- pure sim.years

  -- returns should put us over the theshold for wda
  y.returns === Portfolio (usd 100) (usd 30)

  -- should withdraw from bonds first because they grow
  y.withdrawals === Portfolio (usd 0) (loss wda)


  -- let wa = amount swr4 (total bal)
  -- wa === usd 40

  -- -- it should withdraw from 
  -- (bondsFirst wa bal).stocks === usd 0
  -- (bondsFirst wa bal).bonds === usd (-40)



  -- -- withdrawals are made from bonds first
  -- map (\r -> r.withdrawals.stocks) sim.years === [usd 0, usd 0]

  -- -- withdrawals remain constant from year to year in real dollars
  -- map (\r -> r.withdrawals.bonds)  sim.years === [usd (-40000), usd (-40000)]

assertRebalance :: IO ()
assertRebalance = do
  let bs = Portfolio (usd 100) (usd 0)
  let bb = Portfolio (usd 0) (usd 100)
  let be = Portfolio (usd 60) (usd 40)
  fixedPortfolio (Pct 0.6) (Pct 0.4) bs === Portfolio (usd (-40)) (usd (40))
  fixedPortfolio (Pct 0.6) (Pct 0.4) bb === Portfolio (usd (60)) (usd (-60))
  fixedPortfolio (Pct 0.6) (Pct 0.4) be === Portfolio zero zero

usd :: Float -> USD a
usd f = fromFloat f