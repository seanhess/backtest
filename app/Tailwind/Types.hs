{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
module Tailwind.Types where

import Data.Function ((&))
import Data.List (nub)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.String (IsString(..))
import Data.Text (Text, pack)
import Data.Char (isLower, isUpper)
import GHC.Exts (IsList(..))
import Prelude hiding ((-))
import Text.Casing as Casing (kebab)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

newtype Seg a = Seg { fromSeg :: Text }
  deriving (Eq, Show, IsString, Semigroup)

class Segment a where
  seg :: a -> Seg b
  default seg :: Show a => a -> Seg b
  seg = segHyphens

instance Segment () where
  seg _ = ""

class Option k a where
  option :: a -> Seg k

  default option :: Segment a => a -> Seg k
  option = seg

segHyphens :: Show a => a -> Seg b
segHyphens a = Seg $ hyphenate $ show a

hyphenate :: String -> Text
hyphenate = Text.toLower . pack . Casing.kebab

-- drops a prefix caps etc
dropPrefix :: String -> String
dropPrefix = dropWhile isLower . dropWhile isUpper

-- drop until second cap
segPrefix :: Show a => a -> Seg b
segPrefix a = Seg $ hyphenate $ dropPrefix $ show a


(-) :: Seg a -> Seg b -> Seg a
a - "" = a
(Seg a) - (Seg b) = Seg $ a <> "-" <> b

newtype Class = Class { fromClass :: Text }
  deriving (Show, Eq, IsString)


-- * Utilties

cls :: Seg a -> [Class]
cls (Seg t) = [Class t]

