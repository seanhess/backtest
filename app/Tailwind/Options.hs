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
  , cls, merge
  ) where

import Data.Function ((&))
import Data.List (nub)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.String (IsString(..))
import Data.Text (Text, pack)
import GHC.Exts (IsList(..))
import Prelude hiding ((-))
import Text.Casing as Casing (kebab)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

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



-- * Utilties

-- | Add a class prefixed with a space so they concatenate
cls :: Seg a -> [Class]
cls (Seg t) = [Class t]

merge :: [[Class]] -> [Class]
merge css = nub $ mconcat css