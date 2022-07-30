module Web.UI.Size where

import Prelude


-- better units, in pixels

-- it could round? no
data PixelSize
  = P0 | Px | P2 | P4 | P6 | P8 | P10 | P12 | P14 | P16
  | P20 | P24 | P28 | P32 | P36 | P40 | P44 | P48 | P56 | P64
  | P80 | P96 | P112 | P128 | P144 | P160 | P176 | P192 | P208 | P224 | P240 | P256
  | P288 | P320 | P384
  deriving (Eq)

-- convert to tailwind's weird pixel sizes
instance Show PixelSize where
  show P0 = "0"
  show Px = "px"
  show P2 = "0.5"
  show P4 = "1"
  show P6 = "1.5"
  show P8 =  "2"
  show P10 = "2.5"
  show P12 = "3"
  show P14 = "3.5"
  show P16 = "4"
  show P20 = "5"
  show P24 = "6"
  show P28 = "7"
  show P32 = "8"
  show P36 = "9"
  show P40 = "10"
  show P44 = "11"
  show P48 = "12"
  show P56 = "14"
  show P64 = "16"
  show P80 = "20"
  show P96 = "24"
  show P112 = "28"
  show P128 = "32"
  show P144 = "36"
  show P160 = "40"
  show P176 = "44"
  show P192 = "48"
  show P208 = "52"
  show P224 = "56"
  show P240 = "60"
  show P256 = "64"
  show P288 = "72"
  show P320 = "80"
  show P384 = "96"
