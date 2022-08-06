
example :: UI t ()
example = col (gap S4 . bg black . text white . pad S2 . flex WrapReverse) $ do
  row (bg red . pad S0 . pad (X S1) . pad (Y Px)) $ do
    space
    str "EXAMPLE"
    space

  row (gap S2 . bg red . border black . border B1 . border (R B8) . text black . justify Evenly) $ do
      el (bg white . basis S40) $ str "one"
      el (bg white . basis R1_2) $ str "two"
      el (bg white . basis S6)  $ str "three"

  el (bg green . width S12) $ str "hello"