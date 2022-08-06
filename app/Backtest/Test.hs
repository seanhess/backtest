module Backtest.Test where
  
import Prelude
import Tailwind ()
import Tailwind.UI
import Tailwind.Options
import Tailwind.Values
import Tailwind.UI.Classes

black = Color "black"
white = Color "white"
red   = Color "red"
green = Color "green"

testUI :: UI t ()
testUI = col (gap S4 . bg black . text white . p S2) $ do
  row (bg red . p S0 . px S1 . py Px) $ do
    space
    str "EXAMPLE"
    space

  row (gap S2 . bg red . border black . border B1 . border (R B8) . text black . justify Evenly) $ do
      el (bg white . basis S40) $ str "one"
      el (bg white . basis R1_2) $ str "two"
      el (bg white . basis S6)  $ str "three"

  el (bg green . w S12) $ str "hello"

