{-# LANGUAGE DeriveAnyClass #-}
module Backtest.LayoutTest where

import Backtest.Prelude
import Tailwind.UI


-- this is a bit obnoxious!
data AppColor
  = Black
  | White
  | Green
  | Red
  | Blue
  | BlueLight
  | BlueDark
  deriving (Show, Segment)

instance Option Background AppColor
instance Option Border AppColor
instance Option FontText AppColor

yellow400 :: Color
yellow400 = Color "yellow-500"

dataItems :: [[Text]]
dataItems = [["one", "row"], ["two"], ["three"], ["four"], ["five"], ["six"], ["seven"], ["eight"], ["nine"], ["ten"], ["eleven"]]

-- TODO align
layoutTest :: UI t ()
layoutTest = col (bg Black) $ do

  row (bg Red . p S0. shrink . text White) $ do
    space
    str "EXAMPLE"
    space

  row (p S2 . gap S2 . items Center . h S48 . bg BlueLight . flex Wrap) $ do
    el (bg Green . grow) $ str "MANUAL"
    forM_ dataItems $ \c -> do
      col (bg White . w S20 . gap S1) $ do
        forM_ c $ \item -> do
          el (bg Green . basis S4) $ str item

  el (bg yellow400 . w S12) $ str "hello"


-- can you do it with space? or align?
-- it's much simpler than remembering?


-- THINGS I DON'T WANT TO REMEMBER
-- width = basis when inside a container, but only when along the primary axis, yikes