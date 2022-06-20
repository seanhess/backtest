module Backtest.Optimize where

import Backtest.Prelude
import Backtest.Types hiding (low)

-- Find the optimal starting amount and raise for a given situation





-- it needs to return something
-- [[SimResult]]

type Range a = (a, a)


-- gives you all valid permutations
-- I'm not sure this final thing is what we want
-- what's the final format?
-- it's ((x, y, y), results)?
-- yeah that works!

-- filters out the bad ones!
-- runAll :: forall a result. [a] -> (result -> Bool) -> (a -> result) -> [(a, result)]
-- runAll as isValid run =
--   let rs = map run as :: [result]
--   in filter (\ar -> isValid $ snd ar) $ zip as rs



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

