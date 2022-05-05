{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Invest.Lib where

import Invest.Prelude
import Invest.Types
import Data.Csv as Csv (decodeByName, Header)
import Data.List as List
import Data.ByteString.Lazy (readFile)
import Data.Vector as Vector (Vector, toList)
import Debug.Trace (trace)
import Control.Monad.State (State, MonadState, get, put, gets, execState)


run :: IO ()
run = do
    rs <- loadReturns
    let hs = toHistories rs
    let hs' = filter (betweenYears (Year 1925) (Year 1995)) hs
    runMSWR 30 hs' (action $ rebalanceFixed (pct 50.0) (pct 50.0))

    -- let ss = samples 30 hs

    -- let sim = simulation million (standard6040Withdraw4 million)
    -- let srs = map sim ss :: [SimResult]

    -- (h1:_) <- pure hs

    -- mapM_ print hs
    -- let ys = (head srs).years
    -- let y1 = head ys

    -- print y1
    -- print $ bp.toFloat
    -- print $ take 2 rs

    -- * Show all years
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

-- keep 6040 fixed
runMSWR :: Int -> [History] -> Actions () -> IO ()
runMSWR yrs hs rebalance = do
    let ss = samples yrs hs
    let start = million
    -- let res = map (runRate start ss) rates :: [[SimResult]]

    let rrs = rateResults start ss allRates

    print $ map (.year) hs
    mapM_ print rrs
    print $ mswr rrs

    -- putStrLn $ "Years: " <> show yrs

    -- forM_ rates $ \r -> do
    --     let srs = runRate start ss r
    --     putStrLn $ "RATE: " <> show r
    --     putStrLn "----------------"
    --     print $ successRate srs
    --     putStrLn ""


    -- let sim = simulation (standard6040Withdraw4 start) start
    -- let srs = map sim ss :: [SimResult]
    pure ()
    where

        allRates :: [Pct Withdrawal]
        allRates = [pct 3.5, pct 3.6, pct 3.7, pct 3.8, pct 3.9, pct 4.0, pct 4.1, pct 4.2, pct 4.3, pct 4.4, pct 4.5, pct 4.6, pct 4.7, pct 4.8, pct 4.9, pct 5.0]

        -- what if none are successful?
        rateResults :: Balances -> [[History]] -> [Pct Withdrawal] -> [RateResult]
        rateResults start ss rates =
            map (runRate start ss) rates

        mswr :: [RateResult] -> Maybe RateResult
        mswr rrs =
            List.find (isSuccessful . (.success)) $ reverse rrs


        runRate :: Balances -> [[History]] -> Pct Withdrawal -> RateResult
        runRate start ss wdp =
            let srs = map (runSim start wdp) ss
            in RateResult
              { years = yrs
              , rate = wdp
              , success = successRate srs
              }

        runSim :: Balances -> Pct Withdrawal -> [History] -> SimResult
        runSim start wdp =
            let wda = loss $ staticWithdrawalAmount wdp start :: USD Withdrawal
            in simulation start $ do
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
            ret =changes bal bal'

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


apply :: Changes -> Balances -> Balances
apply ch bals =
    Portfolio (addAmount ch.stocks bals.stocks) (addAmount ch.bonds bals.bonds)


-- ah, we need the previous one also!
calcReturns :: History -> Balances -> Balances
calcReturns h b =
    let ds = amount h.stocks b.stocks
        db = amount h.bonds b.bonds
    in Portfolio (addAmount ds b.stocks) (addAmount db b.bonds)






newtype Actions a = Actions { fromActions :: State Balances a }
  deriving (Monad, Applicative, Functor, MonadState Balances)




changes :: Balances -> Balances -> Changes
changes start end =
    Portfolio (gains start.stocks end.stocks) (gains start.bonds end.bonds)

-- well, wait, when we runActions/

-- Run a function that produces changes
action :: (Balances -> Balances) -> Actions ()
action f = do
    cur <- get :: Actions Balances
    put $ f cur

runActions :: Balances -> Actions () -> Balances
runActions bal (Actions st) = 
    execState st bal

staticWithdrawalAmount :: Pct Withdrawal -> Balances -> USD Withdrawal 
staticWithdrawalAmount p bal1 =
    fromUSD $ amount p (total bal1)

withdrawBondsFirst :: USD Withdrawal -> Balances -> Balances
withdrawBondsFirst wd b =

    let wda = loss $ toAmount wd :: USD Amount
        bf  = addAmount wda b.bonds :: USD Bonds
        db  = gains b.bonds bf :: USD Amount
        ds  = loss $ gains wda db

    in Portfolio (addAmount ds b.stocks) bf

rebalanceFixed :: Pct Stocks -> Pct Bonds -> Balances -> Balances
rebalanceFixed ps pb bal =
    let tot = total bal
        ts = fromUSD $ amount ps tot :: USD Stocks
        tb = fromUSD $ amount pb tot :: USD Bonds
    in Portfolio ts tb

noChanges :: Balances -> Changes
noChanges _ = Portfolio mempty mempty

noActions :: Actions ()
noActions = pure ()


million :: Balances
million = Portfolio
  { stocks = usd $ 600*1000
  , bonds = usd $ 400*1000
  }

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


-- test1 :: Balances -> Actions [Changes]
-- test1 start = do
--     f <- get
--     standardWithdraw4 start
--     f2 <- get
--     standardRebalance6040
--     f3 <- get
--     pure [f start, f2 start, f3 start]



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
