{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
module Tailwind.Options
  ( Option(..)
  , Segment(..)
  , Seg(..)
  , (-)
  , Class(..)
  , segHyphens
  , segDropPrefix
  ) where

import Prelude hiding ((-))
import Data.Function ((&))
import GHC.Exts (IsList(..))
import Data.Text (Text, pack)
import Data.String (IsString(..))
import Control.Applicative ((<|>))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Map as Map
import Text.Casing as Casing (kebab)

newtype Seg a = Seg { fromSeg :: Text }
  deriving (Eq, Show, IsString, Semigroup)

-- fromText :: Text -> Seg a
-- fromText t = Seg t

-- arghm. 
segHyphens :: Show a => a -> Seg b
segHyphens a = Seg $ Text.toLower $ pack $ Casing.kebab $ show a

segDropPrefix :: Show a => a -> Seg b
segDropPrefix a = Seg $ pack $ drop 1 $ show a


class Segment a where
  seg :: a -> Seg b

instance Segment () where
  seg _ = ""

class Option k a where
  option :: a -> Seg k

  default option :: Segment a => a -> Seg k
  option = seg

(-) :: Seg a -> Seg b -> Seg a
a - "" = a
(Seg a) - (Seg b) = Seg $ a <> "-" <> b

newtype Class = Class { fromClass :: Text }
  deriving (Show, Eq, IsString)

-- we aren't doing ANYTHING higher level here
-- you must specify classes individually
-- padding S1, padding (X S2)

-- instance Option k a => Option k [a] where
--   option as = mconcat (map option as)

-- instance (Option k a, Option k b) => Option k (a, b) where
--   option (a, b) = mconcat [option a, option b]

-- instance (Option k a, Option k b, Option k c) => Option k (a, b, c) where
--   option (a, b, c) = mconcat [option a, option b, option c]

-- instance (Option k a, Option k b, Option k c, Option k d) => Option k (a, b, c, d) where
--   option (a, b, c, d) = mconcat [option a, option b, option c, option d]

-- accept a prefix?
-- you can add a prefix to each of them, but maybe we don't want to allow it
-- separated by '-', I think?
-- Favor the flags of the second, combine the names
-- instance Semigroup (Class a) where
--   (Class _ _ _ n) <> (Class h ap k n') = Class h ap k (n <> n')

-- prefixRaw :: (Text -> Text) -> (Text -> Text) -> Text -> Classes a -> Classes a
-- prefixRaw cleanClass cleanFinal t (Classes cs) = Classes $ fmap add cs
--   where
--     add c = t <> cleanClass c
--       & cleanFinal
