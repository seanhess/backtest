module Backtest.Prelude
  ( module Prelude
  , (&)
  , (<|>)
  , Text, cs
  , MonadIO, liftIO
  -- , MonadTrans, lift
  , MonadCatch, MonadThrow, Exception, try, throwM
  , fromMaybe, listToMaybe, mapMaybe, catMaybes, isJust, isNothing
  , identity
  , Generic

  , Map

  -- * List functions
  , module Data.List

  -- * Monadic functions
  , unless, guard, zipWithM_, forM_

  -- * Safe functions
  , headMay, headDef
  , tailMay, tailDef
  , lastMay
  , maximumMay, maximumDef
  , minimumMay, minimumDef

  -- * lifted IO
  , putStrLn, print, putStr
  ) where

import Prelude hiding (id, head, print, putStrLn, readFile, writeFile, Real, putStr)
import qualified Prelude
import Data.Text (Text)
import Data.Map (Map)
-- import Data.HashMap (HashMap)
import Data.String.Conversions(cs)
import Control.Monad.IO.Class (MonadIO, liftIO)
-- import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad (unless, guard, zipWithM_, mapM_, forM_)
import Control.Monad.Catch (MonadCatch, try, throwM, Exception, MonadThrow)
import Data.Function ((&))
import Data.List (group, sort, sortOn, find, groupBy, tails, head)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, catMaybes, isJust, isNothing)
import GHC.Generics (Generic)
import Safe

identity :: a -> a
identity x = x

print :: (MonadIO m, Show a) => a -> m ()
print = liftIO . Prelude.print

putStrLn :: (MonadIO m) => String -> m ()
putStrLn = liftIO . Prelude.putStrLn

putStr :: (MonadIO m) => String -> m ()
putStr = liftIO . Prelude.putStr



