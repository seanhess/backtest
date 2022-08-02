{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
module Tailwind.Options where

import Data.Function ((&))
import Data.List (nub)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.String (IsString(..))
import Data.Text (Text, pack)
import Data.Char (isLower)
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
segHyphens a = Seg $ hyphenate $ show a

hyphenate :: String -> Text
hyphenate = Text.toLower . pack . Casing.kebab

-- drop until second cap
segDropPrefix :: Show a => a -> Seg b
segDropPrefix a = Seg $ hyphenate $ dropWhile isLower $ drop 1 $ show a


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

cls :: Seg a -> [Class]
cls (Seg t) = [Class t]




data Side a
  = T a
  | B a
  | L a
  | R a

instance (Segment a) => Segment (Side a) where
  seg (T a) = "t" - seg a
  seg (B a) = "b" - seg a
  seg (L a) = "l" - seg a
  seg (R a) = "r" - seg a


data Axis a
  = X a
  | Y a

instance (Segment a) => Segment (Axis a) where
  seg (X a) = "x" - seg a
  seg (Y a) = "y" - seg a


data Corner a
  = TL a
  | TR a
  | BL a
  | BR a

instance (Segment a) => Segment (Corner a) where
  seg (TL a) = "tl" - seg a
  seg (TR a) = "tr" - seg a
  seg (BL a) = "bl" - seg a
  seg (BR a) = "br" - seg a


data Auto = Auto
  deriving (Enum, Bounded)

instance Segment Auto where
  seg Auto = "auto"




data Full = Full
  deriving (Enum, Bounded, Show)
instance Segment Full where
  seg = segHyphens


-- SOME of the sizes. Dumb
data RelSize
  = R1_2
  | R1_3
  | R2_3
  | R1_4
  | R3_4
  deriving (Bounded, Enum)
instance Segment RelSize where
  seg R1_2   = "1/2"
  seg R1_3   = "1/3"
  seg R2_3   = "2/3"
  seg R1_4   = "1/4"
  seg R3_4   = "3/4"

data ExtSize
  = R1_5
  | R2_5
  | R3_5
  | R4_5
  | R1_6
  | R5_6
  | R1_12
  | R5_12
  | R7_12
  | R11_12
  | Screen
  | Min
  | Max
  deriving (Bounded, Enum)
instance Segment ExtSize where
  seg R1_5   = "1/5"
  seg R2_5   = "2/5"
  seg R3_5   = "3/5"
  seg R4_5   = "4/5"
  seg R1_6   = "1/6"
  seg R5_6   = "5/6"
  seg R1_12  = "1/12"
  seg R5_12  = "5/12"
  seg R7_12  = "7/12"
  seg R11_12 = "11/12"
  seg Screen = "screen"
  seg Min    = "min"
  seg Max    = "max"
  
data Size
  = Px
  | S0
  | S0_5
  | S1
  | S1_5
  | S2
  | S2_5
  | S3
  | S3_5
  | S4
  | S5
  | S6
  | S7
  | S8
  | S9
  | S10
  | S11
  | S12
  | S14
  | S16
  | S20
  | S24
  | S28
  | S32
  | S36
  | S40
  | S44
  | S48
  | S52
  | S56
  | S60
  | S64
  | S72
  | S80
  | S96
  deriving (Eq, Enum, Bounded, Show)
instance Segment Size where
  seg Px = "px"
  seg s = Seg $ Text.replace "_" "." $ pack $ drop 1 $ show s


data XSML
  = Xs
  | Base
  | Xl4
  | Xl5
  | Xl6
  | Xl7
  | Xl8
  | Xl9
  deriving (Enum, Bounded)
instance Segment XSML where
  seg Xs = "xs"
  seg Base = "base"
  seg Xl4 = "4xl"
  seg Xl5 = "5xl"
  seg Xl6 = "6xl"
  seg Xl7 = "7xl"
  seg Xl8 = "8xl"
  seg Xl9 = "9xl"


data SML
  = Sm
  | Md
  | Lg
  | Xl
  | Xl2
  | Xl3
  deriving (Enum, Bounded)
instance Segment SML where
  seg Sm = "sm"
  seg Md = "md"
  seg Lg = "lg"
  seg Xl = "xl"
  seg Xl2 = "2xl"
  seg Xl3 = "3xl"



data BorderSize
  = B0
  | B1
  | B2
  | B4
  | B6
  | B8
  deriving (Enum, Bounded)
instance Segment BorderSize where
  seg B0 = "0"
  seg B1 = ""
  seg B2 = "2"
  seg B4 = "4"
  seg B6 = "6"
  seg B8 = "8"



data None
  = None
  deriving (Show, Bounded, Enum)
instance Segment None where
  seg = segHyphens


-- Color needs to be a concrete datatype
-- otherwise you need to specify all your colors for this to work at all


-- AppColor yay
newtype Color = Color Text
instance Segment Color where
  seg (Color n) = Seg n



data Weight
  = CW50
  | CW100
  | CW200
  | CW300
  | CW400
  | CW500
  | CW600
  | CW700
  | CW800
  | CW900
  deriving (Show, Eq)

instance Segment Weight where
  seg w = Seg $ pack $ drop 2 $ show w


data FontWeight
  = Thin
  | ExtraLight
  | Light
  | Normal
  | Medium
  | Semibold
  | Bold
  | Extrabold
  -- | Black
  deriving (Show, Bounded, Enum)

instance Segment FontWeight where
  seg fw = Seg $ Text.toLower $ pack $ show fw



data AlignSEC
  = Start
  | End
  | Center
  deriving (Bounded, Enum, Show)
instance Segment AlignSEC where
  seg = segHyphens

data AlignSB
  = Stretch
  | Baseline
  deriving (Bounded, Enum, Show)
instance Segment AlignSB where
  seg = segHyphens

data AlignBAE
  = Between
  | Around
  | Evenly
  deriving (Bounded, Enum, Show)
instance Segment AlignBAE where
  seg = segHyphens



data Direction
  = Row
  | Col
  | RowReverse
  | ColReverse
  deriving (Bounded, Enum, Show)
instance Segment Direction where
  seg = segHyphens

data Wrap
  = Wrap
  | NoWrap
  | WrapReverse
  deriving (Bounded, Enum, Show)
instance Segment Wrap where 
  seg = segHyphens


data Pos
  = Static
  | Fixed
  | Absolute
  | Relative
  | Sticky
  deriving (Enum, Bounded, Show)
instance Segment Pos where
  seg = segHyphens


data Rot = R0 | R1 | R2 | R3 | R6 | R12 | R45 | R90 | R180
  deriving (Show, Bounded, Enum)
instance Segment Rot where
  seg = segDropPrefix


data Property
  = All
  | Colors
  | Transform
  | Shadow
  | Opacity
  deriving (Show, Bounded, Enum)
instance Segment Property where
  seg = segHyphens


data Dur
  = D75
  | D100
  | D150
  | D200
  | D300
  | D500
  | D700
  | D1000
  deriving (Show, Bounded, Enum)
instance Segment Dur where
  seg = segDropPrefix


data Z
  = Z0
  | Z10
  | Z20
  | Z30
  | Z40
  | Z50
  deriving (Show, Bounded, Enum)
instance Segment Z where
  seg = segDropPrefix


data Opacity
  = O0
  | O5
  | O10
  | O20
  | O25
  | O30
  | O40
  | O50
  | O60
  | O70
  | O75
  | O80
  | O90
  | O100
  deriving (Show, Bounded, Enum)
instance Segment Opacity where
  seg = segDropPrefix


data Ease
  = Linear
  | InOut
  | Out
  | In
  deriving (Show, Bounded, Enum)
instance Segment Ease where
  seg = segHyphens

