module Backtest.Optimize where

import Backtest.Prelude
import Backtest.Types hiding (low)
import Backtest.Debug (debug)
import Backtest.Simulation (Actions (), simulation)
import Backtest.Aggregate (medWithdrawal, isWithdrawalFail)
import qualified Data.List.NonEmpty as NE
import qualified Data.List as List

-- Find the optimal starting amount and raise for a given situation



data Inputs = Inputs
  { swr :: Pct Withdrawal
  , alloc :: Pct Stocks
  }

data OptimizeResult = OptimizeResult
  { alloc :: Pct Stocks
  , swr :: Pct Withdrawal
  , results :: NonEmpty SimResult
  }

type WR = Pct Withdrawal
type PS = Pct Stocks
type WDA = USD (Amt Withdrawal)

-- b = Tested
-- a = Pct Stocks
-- unfoldr :: (a -> (b, Maybe a)) -> a -> NonEmpty b

-- no, we CHOOSE the starting values
-- only the optimization variables are passed in. The rest are applied
optimize :: NonEmpty (NonEmpty History) -> Balances -> (Pct Stocks -> Pct Withdrawal -> Actions ()) -> [OptimizeResult]
optimize ss bal actions =
  optimizeAlloc startPs startRate

  where
    startPs = pct 100
    startRate = pct 2.8

    stepAlloc ps = ps - pct 5
    stepRate wr = wr + pct 0.05

    optimizeAlloc :: Pct Stocks -> Pct Withdrawal -> [OptimizeResult]
    optimizeAlloc ps wr =
      mconcat $ List.unfoldr nextAlloc (ps, wr)

    nextAlloc :: (Pct Stocks, Pct Withdrawal) -> Maybe ([OptimizeResult], (Pct Stocks, Pct Withdrawal))
    nextAlloc (ps, mwr) = do
      let res = optimizeRate ps mwr

      mwr' <- fst <$> lastMay res

      pure (map (optimizeResult ps) res, (stepAlloc ps, mwr'))

    optimizeRate :: Pct Stocks -> Pct Withdrawal -> [(Pct Withdrawal, NonEmpty SimResult)]
    optimizeRate ps wr = optimizeMax wr stepRate isSimValid (runSim ps)

    runSim :: Pct Stocks -> Pct Withdrawal -> NonEmpty SimResult
    runSim ps wr =
      let sim = simulation bal (actions ps wr)
      in fmap sim ss

    isSimValid :: NonEmpty SimResult -> Bool
    isSimValid = all (not . isWithdrawalFail)

    optimizeResult :: Pct Stocks -> (Pct Withdrawal, NonEmpty SimResult) -> OptimizeResult
    optimizeResult ps (swr, srs) =
      OptimizeResult ps swr srs


-- maximize by: wdr, then median
bestResult :: [OptimizeResult] -> Maybe OptimizeResult
bestResult = maximumByMay (comparing rateThenMed)
  where
    rateThenMed o = (o.swr, medWithdrawal o.results)



-- -- the highest one
-- bestRate :: [(Pct Stocks, Pct Withdrawal, NonEmpty SimResult)] -> Maybe (Pct Withdrawal)
-- bestRate = maximumBy snd

-- bestAlloc :: [(Pct Stocks, Pct Withdrawal, NonEmpty SimResult)] -> Maybe (Pct Stocks)
-- bestAlloc = _

-- optimizeAlloc :: NonEmpty (Pct Stocks, Pct Withdrawal, NonEmpty SimResult) -> (Pct Stocks, Pct Withdrawal, NonEmpty SimResult)
-- optimizeAlloc allocs =
--   last $ NE.sortBy (comparing score) allocs
--   where
--     score (_, swr, srs) = (swr, medWithdrawal srs)
    


-- step up and re-run each time, nothing fancy
optimizeMax :: forall x result. Show x => x -> (x -> x) -> (result -> Bool) -> (x -> result) -> [(x, result)]
optimizeMax start step isValid run =
  takeWhile (isValid . snd) $ zip steps results
  where
    steps :: [x]
    steps = take 80 $ iterate step start

    results :: [result]
    results = fmap run steps




-- optimizeWithdrawal :: Pct Withdrawal -> (Pct Withdrawal -> [[SimResult]]) -> ([[SimResult]] -> Bool)
-- optimizeWithdrawal


runAll3 :: [a] -> [b] -> [c] -> (result -> Bool) -> (a -> b -> c -> result) -> [(a, b, c, result)]
runAll3 as bs xs isValid run =
  let is = permutations3 as bs xs
      rs = map (\(a, b, c) -> run a b c) is
  in map toResult $ filter (\ar -> isValid $ snd ar) $ zip is rs
  where
    toResult ((a, b, c), res) = (a, b, c, res)


permutations :: [a] -> [b] -> [(a, b)]
permutations as bs = do
  a <- as
  b <- bs
  pure (a, b)

permutations3 :: [a] -> [b] -> [c] -> [(a, b, c)]
permutations3 as bs xs = do
  a <- as
  b <- bs
  c <- xs
  pure (a, b, c)



-- optimize :: Monad m => (Pct Withdrawal -> Pct Stocks -> USD (Amt Withdrawal) -> m [[SimResult]]) -> m ()
-- optimize = optimize' raiseRange allocRange
--   where
--     raiseRange = (pct 2.9, pct 10.0)
--     allocRange = (pct 50, pct 100)


-- optimize' :: Monad m => Range (Pct Withdrawal) -> Range (Pct Stocks) -> (Pct Withdrawal -> Pct Stocks -> USD (Amt Withdrawal) -> m [[SimResult]]) -> m ()
-- optimize' raises allocs runSim = do
--   undefined

  -- TODO creep up the starts from the starting raise


-- are we going to run all of them?
-- under what conditions are the results valid?
-- under what conditions are the results optimized?

-- wait, when do I stop?

-- searchM :: (Ord a, Ord score, Fractional a) => Range a -> (a -> m result) -> (result -> Bool) -> (result -> score) -> m (Maybe (a, result))
-- searchM r run isValid toScore = do
--   -- 1. run the algorithm at mid
--   -- 2. if valid, then low = mid, if not, the high = mid
--   -- do we need to keep track of ALL the answers? No, we are optimizing
--   -- let mid = 
--     let res = run (mid r)

--     undefined
--   where
--     -- move the range to the new parameters
--     newRange midRes =
--       if isValid midRes
--         then (mid r, high r)
--         else (low r, mid r)

-- mid :: Fractional a => Range a -> a
-- mid (l, h) = (l + h) / 2

-- high :: Range a -> a
-- high = snd

-- low :: Range a -> a
-- low = fst

