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
    let alloc = rebalanceFixed (pct 70) (pct 30)
    let bal = alloc million

    putStrLn "Rebalance Fixed"
    putStrLn "----------------"
    runMSWR 50 hs bal (action alloc)
    pure ()

    putStrLn "Prime Harvesting"
    putStrLn "----------------"
    runMSWR 50 hs bal (action (rebalancePrime bal.stocks))

    putStrLn ""
    putStrLn ""

    putStrLn "Prime Harvesting 2"
    putStrLn "------------------"
    runMSWR 50 hs bal (action (rebalancePrimeNew bal.stocks))


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
        allRates = map pct [3.5, 3.6 .. 5.0]

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
            let wda = loss $ staticWithdrawalAmount wdp start :: USD Withdrawal
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

            wd = total act

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

staticWithdrawalAmount :: Pct Withdrawal -> Balances -> USD Withdrawal 
staticWithdrawalAmount p bal1 =
    fromUSD $ amount p (total bal1)

withdrawBondsFirst :: USD Withdrawal -> Balances -> Balances
withdrawBondsFirst wd b =

    let wda = loss $ toAmount wd :: USD Amount
        bf  = addToBalance wda b.bonds :: USD Bonds
        db  = gains b.bonds bf :: USD Amount
        ds  = loss $ gains wda db

    in Portfolio (addToBalance ds b.stocks) bf

rebalanceFixed :: Pct Stocks -> Pct Bonds -> Balances -> Balances
rebalanceFixed ps pb bal =
    let tot = total bal
        ts = fromUSD $ amount ps tot :: USD Stocks
        tb = fromUSD $ amount pb tot :: USD Bonds
    in Portfolio ts tb

-- This is both the withdrawal AND the rebalancing
rebalancePrime :: USD Stocks -> Balances -> Balances
rebalancePrime start bal =
    let target = amountBalance (pct 120) start
        extra = minZero $ gains target bal.stocks
    in Portfolio (addToBalance (loss extra) bal.stocks) (addToBalance (gain extra) bal.bonds)

rebalancePrimeNew :: USD Stocks -> Balances -> Balances
rebalancePrimeNew start bal
  | bal.stocks > amountBalance (pct 120) start = rebalancePrime start bal
  | bal.stocks < amountBalance (pct 80) start = primeBuyStocks start bal
  | otherwise = bal

primeBuyStocks :: USD Stocks -> Balances -> Balances
primeBuyStocks start bal =
    let target = amountBalance (pct 80) start
        short  = minZero $ gains bal.stocks target
        actual = min short (fromUSD bal.bonds)
    in Portfolio (addToBalance (gain actual) bal.stocks) (addToBalance (loss actual) bal.bonds)

  



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




withdraw :: USD Withdrawal -> Actions ()
withdraw wda = do
    action $ withdrawBondsFirst wda

standardWithdraw4 :: Balances -> Actions ()
standardWithdraw4 start = do
    let const4Percent = loss $ staticWithdrawalAmount swr4 start :: USD Withdrawal
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
