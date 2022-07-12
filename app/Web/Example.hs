module Web.Example where

import Prelude
import Web.UI
import Lucid.Html5
import Data.Text (Text)

import Tailwind hiding (Black, text)


data AppColor
  = Green
  | Black
  | White
instance Segment AppColor where
  seg Green = "green-500"
  seg Black = "black"
  seg White = "white"
instance Option Color AppColor



-- turn off overloaded lists, wahoo
test :: UI ()
test = col [id_ "asdf"] [bg Green, padding S10, gap S10, inset S1] $ do
  el [ bg Black ] (text "hello")
  space
  el (text "nothing")
  space
  el "goodbye"

