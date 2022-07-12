module Web.UI.Style where

import Prelude
import Tailwind as Tailwind
import Tailwind.Size as Tailwind


-- better units, in pixels

data PixelSize
  = P0 | PX | P2 | P4 | P6 | P8 | P10 | P12 | P14 | P16
  | P20 | P24 | P28 | P32 | P36 | P40 | P44 | P48 | P56 | P64
  | P80 | P96 | P112 | P128 | P144 | P160 | P176 | P192 | P208 | P224 | P240 | P256
  | P288 | P320 | P384

instance Segment PixelSize where
  seg = seg . convert
    where
      convert P0 = S0
      convert PX = Px
      convert P2 = S0_5
      convert P4 = S1
      convert P6 = S1_5
      convert P8 = S2
      convert P10 = S2_5
      convert P12 = S3
      convert P14 = S3_5
      convert P16 = S4
      convert P20 = S5
      convert P24 = S6
      convert P28 = S7
      convert P32 = S8
      convert P36 = S9
      convert P40 = S10
      convert P44 = S11
      convert P48 = S12
      convert P56 = S14
      convert P64 = S16
      convert P80 = S20
      convert P96 = S24
      convert P112 = S28
      convert P128 = S32
      convert P144 = S36
      convert P160 = S40
      convert P176 = S44
      convert P192 = S48
      convert P208 = S52
      convert P224 = S56
      convert P240 = S60
      convert P256 = S64
      convert P288 = S72
      convert P320 = S80
      convert P384 = S96

instance Option Padding PixelSize
instance Option Padding (Axis PixelSize)
instance Option Padding (Side PixelSize)
instance Option Gap PixelSize
instance Option Dimensions PixelSize


