module Web.UI where

import Prelude hiding ((-))
import Tailwind hiding (Black)
import Tailwind.Lucid
import Tailwind.Options
import Lucid


data AppColor
  = Green
  | Black
  | White
instance Segment AppColor where
  seg Green = "green-500"
  seg Black = "black"
  seg White = "white"
instance Option Color AppColor

example :: [[Class]]
example = [flex Row, bg Green, hover |: font Bold, borderWidth (T B2), borderColor Black, hover |: rotate R90 ]

example' :: Attribute
example' = classes example