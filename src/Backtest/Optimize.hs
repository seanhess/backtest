module Backtest.Optimize where

import Backtest.Prelude
import Backtest.Types hiding (low)
import Backtest.Debug (debug, dump)
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
  { alloc :: Allocation
  , swr :: Pct Withdrawal
  , results :: NonEmpty SimResult
  }

type WR = Pct Withdrawal
type PS = Pct Stocks
type WDA = USD (Amt Withdrawal)

-- b = Tested
-- a = Pct Stocks
-- unfoldr :: (a -> (b, Maybe a)) -> a -> NonEmpty b

stepAlloc5 :: Allocation -> Maybe Allocation
stepAlloc5 S50 = Nothing
stepAlloc5 a = Just $ succ a

stepRate5 :: Pct Withdrawal -> Maybe (Pct Withdrawal)
stepRate5 = stepRate (pct 0.05)

stepRate :: Pct Withdrawal -> Pct Withdrawal -> Maybe (Pct Withdrawal)
stepRate s r
  | r >= pct 100 = Nothing
  | otherwise = Just $ min (pct 100) (r + s)


-- no, we CHOOSE the starting values
-- only the optimization variables are passed in. The rest are applied
optimize :: (Allocation -> Maybe Allocation) -> (Pct Withdrawal -> Maybe (Pct Withdrawal)) -> NonEmpty (NonEmpty History) -> Allocation -> Pct Withdrawal -> Balances -> (Allocation -> Pct Withdrawal -> Actions ()) -> [OptimizeResult]
optimize stepA stepR ss startAlc startRate bal actions =
  optimizeAlloc startAlc startRate

  where

    optimizeAlloc :: Allocation -> Pct Withdrawal -> [OptimizeResult]
    optimizeAlloc ps wr =
      mconcat $ List.unfoldr nextAlloc (ps, wr)

    nextAlloc :: (Allocation, Pct Withdrawal) -> Maybe ([OptimizeResult], (Allocation, Pct Withdrawal))
    nextAlloc (al, mwr) = do
      let res = optimizeRate al mwr
      mwr' <- fst <$> lastMay res
      al' <- stepA al

      pure (map (optimizeResult al) res, (al', mwr'))

    optimizeRate :: Allocation -> Pct Withdrawal -> [(Pct Withdrawal, NonEmpty SimResult)]
    optimizeRate al wr = maximize wr stepR isSimValid (runSim al)

    runSim :: Allocation -> Pct Withdrawal -> NonEmpty SimResult
    runSim ps wr =
      let sim = simulation bal (actions ps wr)
      in fmap sim ss

    optimizeResult :: Allocation -> (Pct Withdrawal, NonEmpty SimResult) -> OptimizeResult
    optimizeResult al (swr, srs) =
      OptimizeResult al swr srs


isSimValid :: NonEmpty SimResult -> Bool
isSimValid = all (not . isWithdrawalFail)


-- maximize by: wdr, then median
bestResult :: [OptimizeResult] -> Maybe OptimizeResult
bestResult = maximumByMay (comparing rateThenMed)
  where
    rateThenMed o = (o.swr, medWithdrawal o.results)


maximizeRate :: (NonEmpty SimResult -> Bool) -> (Pct Withdrawal -> NonEmpty SimResult) -> [(Pct Withdrawal, NonEmpty SimResult)]
maximizeRate = maximize' rateSteps


rateSteps :: [Pct Withdrawal]
rateSteps = map pct $ List.nub $ mconcat
  [ [ 0, 10 .. 50 ]
  , [ 0, 5 .. 50 ]
  , [ 1, 2 .. 50 ]
  , [ 1, 1.5 .. 50 ]
  , [ 1, 1.2 .. 50 ]
  , [ 1, 1.1 .. 50 ]
  ]


-- step up and re-run each time, nothing fancy
maximize :: forall x result. Show x => x -> (x -> Maybe x) -> (result -> Bool) -> (x -> result) -> [(x, result)]
maximize start step isValid run =
  takeWhile (isValid . snd) $ zip steps results
  where
    steps :: [x]
    steps = List.unfoldr nextStep start

    nextStep :: x -> Maybe (x, x)
    nextStep x = do
      x1 <- step x
      pure $ (x, x1)

    results :: [result]
    results = fmap run steps


-- maximizeRate :: (NonEmpty SimResult -> Bool) -> ()

-- ok, we need to start at 0 or 0.1
-- we could go to half?
-- step is ....
-- low, high, what to try next?
-- optimizes the variables via the multiplicative function. Min max
maximize' :: forall x result. (Ord x, Show x) => [x] -> (result -> Bool) -> (x -> result) -> [(x, result)]
maximize' steps isValid run =
  List.reverse $ tryStep steps []
  -- filter 
  -- [0.1, 10, 20, 30, 40, 50, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0.1]

  where
    tryStep :: [x] -> [(x, result)] -> [(x, result)]
    tryStep [] rs = rs
    tryStep (x:xs) rs =
      let r = run x
      in if isValid r
        then tryStep (onlyHigher x xs) ((x, r) : rs)
        else tryStep (onlyLower x xs) rs

    onlyHigher :: x -> [x] -> [x]
    onlyHigher x = filter (>x)

    onlyLower :: x -> [x] -> [x]
    onlyLower x = filter (<x)

    -- results :: [result]
    -- results = fmap run steps




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

