{-# LANGUAGE DeriveAnyClass #-}
module Backtest.App.Style where

import Backtest.Prelude
import Web.UI

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

