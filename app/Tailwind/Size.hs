{-# LANGUAGE OverloadedStrings #-}
module Tailwind.Size where

import Prelude hiding ((-))
import Data.Text as Text (Text, replace, toLower, pack)
import Tailwind.Options

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


data Corners a
  = TL a
  | TR a
  | BL a
  | BR a

instance (Segment a) => Segment (Corners a) where
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
  | S4_5
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
  deriving (Show)
instance Segment None where
  seg = segHyphens