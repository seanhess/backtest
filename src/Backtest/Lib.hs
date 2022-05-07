{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Backtest.Lib where

import Backtest.Prelude
import Backtest.Types
import Data.Csv as Csv (decodeByName, Header)
import Data.List as List
import Data.ByteString.Lazy (readFile)
import Data.Vector as Vector (Vector, toList)
import Debug.Trace (trace)
import Control.Monad.State (State, MonadState, modify, execState)


run :: IO ()
run = do
    rs <- loadReturns
    let hs = toHistories rs
    -- let hs' = filter (betweenYears (Year 1925) (Year 1995)) hs

    -- COMPARE MSWR
    -----------------
    let ps = pct 60
    let pb = pct 40
    let alloc = rebalanceFixed ps pb
    let bal = alloc million

    -- oh it's doing better with longer years because we are removing some of the worst cohorts
    let years = 50

    putStrLn "Rebalance Fixed"
    putStrLn "----------------"
    runMSWR years hs bal (action alloc)
    pure ()

    putStrLn "Prime Harvesting"
    putStrLn "----------------"
    runMSWR years hs bal (action (rebalancePrime bal.stocks))

    putStrLn ""
    putStrLn ""

    putStrLn "Prime Harvesting 2"
    putStrLn "------------------"
    runMSWR years hs bal (action (rebalancePrimeNew bal.stocks))

    putStrLn "Swedroe 5/25 bands"
    putStrLn "------------------"
    runMSWR years hs bal (action (rebalance525Bands ps pb))


    -- RUN ONE SAMPLE
    --------------------
    -- let ss = samples 50 hs
    -- let start = million5050
    -- let wda = staticWithdrawalAmount (pct 4) start :: USD Withdrawal
    -- print wda
    -- let sim = simulation start $ do
    --             action $ withdrawBondsFirst wda
    --             action $ rebalancePrimeNew start.stocks
    -- let srs = map sim ss :: [SimResult]

    -- let ys = (head srs).years
    -- mapM_ print $ take 10 ys

    -- -- * Show all years
    -- mapM_ (putStrLn . showSimResult) srs

    -- * Count failures
    -- print $ (length $ filter isFailure srs, length srs)
    -- print $ successRate srs
    -- mapM_ (putStrLn . showSimResult) $ filter isFailure srs

    -- * 1966 failure year
    -- (Just s1966) <- pure $ List.find (isYear 1966) srs
    -- print $ s1966.startYear
    -- print $ s1966.endBalance
    -- mapM_ (putStrLn . showYear) $ s1966.years

  where
      showSimResult sr =
          show (sr.startYear, sr.endYear, sr.endBalance)

      isYear y sr =
          sr.startYear == Year y

      showYear yr =
          show (yr.history.year, yr.withdrawals, yr.start, yr.end)


-- runSingle :: Int -> [History] -> Actions () -> IO ()
-- runSingle yrs hs rebalance = do
--     let ss = samples yrs hs
--     let start = runActions million rebalance


runMSWR :: Int -> [History] -> Balances -> Actions () -> IO ()
runMSWR yrs hs start rebalance = do
    let ss = samples yrs hs
    let rrs = rateResults start ss allRates

    print $ map (.year) hs
    mapM_ print rrs
    print $ mswr rrs

    pure ()
    where

        allRates :: [Pct Withdrawal]
        allRates = map pct [3.5, 3.6 .. 4.5]

        rateResults :: Balances -> [[History]] -> [Pct Withdrawal] -> [RateResult]
        rateResults start' ss rates =
            map (runRate start' ss) rates

        mswr :: [RateResult] -> Maybe RateResult
        mswr rrs =
            List.find (isSuccessful . (.success)) $ reverse rrs


        runRate :: Balances -> [[History]] -> Pct Withdrawal -> RateResult
        runRate start' ss wdp =
            let srs = map (runSim start' wdp) ss
            in RateResult
              { years = yrs
              , rate = wdp
              , success = successRate srs
              }

        runSim :: Balances -> Pct Withdrawal -> [History] -> SimResult
        runSim start' wdp =
            let wda = loss $ staticWithdrawalAmount wdp start :: USD Amt Withdrawal
            in simulation start' $ do
                withdraw wda
                rebalance



loadReturns :: IO [HistoryRow]
loadReturns = do
    putStrLn "Loading"
    f <- readFile "data/data.csv"
    case Csv.decodeByName f of
        Left e -> fail $ "Decoding CSV Failed: " <> e
        Right (_, rs) -> do
            pure $ filter firstMonth $ Vector.toList rs
    where
        firstMonth rets = rets.month == 1


toHistories :: [HistoryRow] -> [History]
toHistories hr = zipWith toHistory hr (drop 1 hr)


toHistory :: HistoryRow -> HistoryRow -> History
toHistory past now =
    History
      { year = now.year
      , stocks = gainsPercent past.stocks now.stocks
      , bonds = gainsPercent past.bonds now.bonds
      }


samples :: Int -> [History] -> [[History]]
samples years hs = List.tails hs
  & fmap (take years)
  & filter (\hs' -> length hs' >= years)

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

            end = runActions bal' actions
            act = changes bal' end

            wd = toWithdrawal $ total act

        in YearResult
          { history = h
          , start = bal
          , returns = ret
          , actions = act
          , end = end
          , withdrawals = wd
          }



-- ah, we need the previous one also!
calcReturns :: History -> Balances -> Balances
calcReturns h b =
    let ds = amount h.stocks b.stocks
        db = amount h.bonds b.bonds
    in Portfolio (addToBalance ds b.stocks) (addToBalance db b.bonds)






newtype Actions a = Actions { fromActions :: State Balances a }
  deriving (Monad, Applicative, Functor, MonadState Balances)




changes :: Balances -> Balances -> Changes
changes start end =
    Portfolio (gains start.stocks end.stocks) (gains start.bonds end.bonds)


-- Run a function that produces changes
action :: (Balances -> Balances) -> Actions ()
action = modify


runActions :: Balances -> Actions () -> Balances
runActions bal (Actions st) = 
    execState st bal

staticWithdrawalAmount :: Pct Withdrawal -> Balances -> USD Amt Withdrawal 
staticWithdrawalAmount p bal1 =
    amount p (total bal1)

withdrawBondsFirst :: USD Amt Withdrawal -> Balances -> Balances
withdrawBondsFirst wd b =
    let wdb = toBonds wd :: USD Amt Bonds
        wds = toStocks $ minZero $ gains (toAmount b.bonds) (toBonds wd) :: USD Amt Stocks
    in Portfolio (addToBalance (loss wds) b.stocks) (addToBalance (loss wdb) b.bonds)

rebalanceFixed :: Pct Stocks -> Pct Bonds -> Balances -> Balances
rebalanceFixed ps pb bal =
    let tot = total bal
        ts = amountBalance ps tot :: USD Bal Stocks
        tb = amountBalance pb tot :: USD Bal Bonds
    in Portfolio ts tb

-- This is both the withdrawal AND the rebalancing
rebalancePrime :: USD Bal Stocks -> Balances -> Balances
rebalancePrime start bal =
    let target = amountBalance (pct 120) start
        extra = minZero $ gains target bal.stocks :: USD Amt Stocks
        ds = loss extra
        db = gain $ toBonds extra
    in Portfolio (addToBalance ds bal.stocks) (addToBalance db bal.bonds)

rebalancePrimeNew :: USD Bal Stocks -> Balances -> Balances
rebalancePrimeNew start bal
  | bal.stocks > amountBalance (pct 120) start = rebalancePrime start bal
  | bal.stocks < amountBalance (pct 80) start = primeBuyStocks start (pct 80) bal
  | otherwise = bal

primeBuyStocks :: USD Bal Stocks -> Pct Stocks -> Balances -> Balances
primeBuyStocks start p bal =
    let target = amountBalance p start
        short  = minZero $ gains bal.stocks target
        actual = min short (toStocks $ toAmount bal.bonds)
    in Portfolio (addToBalance (gain actual) bal.stocks) (addToBalance (loss $ toBonds actual) bal.bonds)


-- Larry Swedroe 5/25 bands
rebalance525Bands :: Pct Stocks -> Pct Bonds -> Balances -> Balances
rebalance525Bands ps pb bal
    | diffAbsPercent ps bal >= 5 = rebalanceFixed ps pb bal
    | diffRelPercent ps bal >= 25 = rebalanceFixed ps pb bal
    | otherwise = bal

diffAbsPercent :: Pct Stocks -> Balances -> Pct Stocks
diffAbsPercent ps bal =
    abs (allocationStocks bal - ps)

diffRelPercent :: Pct Stocks -> Balances -> Pct Stocks
diffRelPercent ps bal =
    abs $ (allocationStocks bal) / ps - 1


allocationStocks :: Balances -> Pct Stocks
allocationStocks bal = percentOf bal.stocks (total bal)

-- what is the current allocation?

    -- let tup = amountBalance (pct 120) start
    --     exs = minZero $ gains tup bal.stocks -- how much has it grown past the target

    --     -- but what if there aren't any bonds?
    --     tlow = amountBalance (pct 80) start
    --     exb = minZero $ gains bal.stocks tlow -- how much has the target exceeded the stocks

    --     ds = addAmounts (gain exb) (loss exs)
    --     db = addAmounts (loss exb) (gain exs)

    -- in Portfolio (addToBalance ds bal.stocks) (addToBalance db bal.bonds)



noChanges :: Balances -> Changes
noChanges _ = Portfolio mempty mempty

noActions :: Actions ()
noActions = pure ()


million :: Balances
million = Portfolio
  { stocks = usd $ 500*1000
  , bonds = usd $ 500*1000
  }

million7030 :: Balances
million7030 =
    rebalanceFixed (pct 70) (pct 30) million

million8020 :: Balances
million8020 =
    rebalanceFixed (pct 80) (pct 20) million

million6040 :: Balances
million6040 =
    rebalanceFixed (pct 60) (pct 40) million

million5050 :: Balances
million5050 =
    rebalanceFixed (pct 50) (pct 50) million


swr4 :: Pct Withdrawal
swr4 = pct 4




withdraw :: USD Amt Withdrawal -> Actions ()
withdraw wda = do
    action $ withdrawBondsFirst wda

standardWithdraw4 :: Balances -> Actions ()
standardWithdraw4 start = do
    let const4Percent = loss $ staticWithdrawalAmount swr4 start :: USD Amt Withdrawal
    action $ withdrawBondsFirst const4Percent

standard6040Withdraw4 :: Balances -> Actions ()
standard6040Withdraw4 start = do
    standardWithdraw4 start
    rebalance6040





successRate :: [SimResult] -> Pct Success
successRate srs =
    let n = length $ filter isFailure srs
    in pctFromFloat $ 1 - (fromIntegral n / fromIntegral (length srs))

isFailure :: SimResult -> Bool 
isFailure sr =
    sr.endBalance == Portfolio mempty mempty

maximumSuccessRate :: Pct Success
maximumSuccessRate = pct 99.0 

isSuccessful :: Pct Success -> Bool
isSuccessful p = p >= maximumSuccessRate

rebalance5050 :: Actions ()
rebalance5050 = action $ rebalanceFixed (pct 50) (pct 50)

rebalance6040 :: Actions ()
rebalance6040 = action $ rebalanceFixed (pct 60) (pct 40)

betweenYears :: Year -> Year -> History -> Bool
betweenYears start end h = 
    start <= h.year && h.year <= end
