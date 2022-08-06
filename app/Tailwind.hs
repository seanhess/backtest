{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}

module Tailwind
  (

  -- * Display
    bg, BgSize(..), Background
  , Color(..), Weight(..)
  , height, width, Dimensions
  , border, Border, BorderSize(..)
  , rounded, Rounded
  , font, Font, FontWeight(..)
  , text, FontText
  , outline, Outline

  -- * Spacing
  , pad, Padding
  , gap, Gap

  -- * Layout
  , flex, Flex, Direction(..), Wrap(..)
  , basis
  , self, items, content, Self, Items, Content
  , grow -- , row, col, space
  , justify, Justify

  -- * Position
  , position, Position, Pos(..)
  , top, bottom, left, right
  , inset
  , zIndex, Z(..)

  -- * Transforms
  , translate
  , transform
  , transition, rotate, easing, duration, delay, Duration, Dur(..), Rotate, Rot(..), Easing, Ease(..), Property(..)

  -- * Effects
  , shadow, Shadow
  , opacity, Opacity(..)


  -- * Prefixes
  , module Tailwind.Prefix

  -- * Option
  , module Tailwind.Options
  )
  where

import Prelude hiding ((-))
import Data.String.Conversions (cs)
import Data.Text as Text (Text, pack, toLower)
import Tailwind.Options
import Tailwind.Values
import Tailwind.Prefix
import Data.List (nub)
import qualified Data.Text as Text



data Background

-- yes, it's a standard color
bg :: Option Background o => o -> [Class]
bg o = 
  cls $ "bg" - (option o :: Seg Background)

instance Option Background BgSize
instance Option Background Color

data BgSize
  = BgCover
  | BgContain
  | BgAuto
  deriving (Show, Eq, Bounded, Enum)
instance Segment BgSize where
  seg = segDropPrefix



data Padding

-- PROBLEM:
-- px-0
-- p-0
-- Padding doesn't hyphenate the x and y for some reason
instance Option Padding Size where
  option s = "p" - (seg s)
instance Option Padding (Axis Size) where
  option s = "p" <> (seg s)
instance Option Padding (Side Size) where
  option s = "p" <> (seg s)

pad :: Option Padding o => o -> [Class]
pad o =
  cls $ (option o :: Seg Padding)


data Border

border :: Option Border o => o -> [Class]
border o = 
  cls $ "border" - (option o :: Seg Border)

instance Option Border BorderSize
instance Option Border (Side BorderSize)
instance Option Border (Axis BorderSize)

-- lots of instances to make custom colors or sizes
instance Option Border Color
instance Option Border (Side Color)
instance Option Border (Axis Color)



-- | The distance between child elements
data Gap
instance Option Gap Size
instance Option Gap (Axis Size)

gap :: Option Gap o => o -> [Class]
gap o =
  cls $ "gap" - (option o :: Seg Gap)

-- | Width and Height
data Dimensions
instance Option Dimensions Auto
instance Option Dimensions Full
instance Option Dimensions Size
instance Option Dimensions RelSize
instance Option Dimensions ExtSize

height :: Option Dimensions o => o -> [Class]
height o =
  cls $ "h" - (option o :: Seg Dimensions)

width :: Option Dimensions o => o -> [Class]
width o =
  cls $ "w" - (option o :: Seg Dimensions)







flex :: Option Flex o => o -> [Class]
flex opts = 
  cls ("flex"-(option opts :: Seg Flex))




data Flex
instance Option Flex Direction
instance Option Flex Wrap
instance Option Flex ()


basis :: Option Dimensions o => o -> [Class]
basis o = cls ("basis"-(option o :: Seg Dimensions))





-- it's not obvious that we are aligning Items vs Content
-- align-items: "items-"     start, end, center, baseline, stretch
-- align-content: "content-" start, end, center, between, around, evenly



self :: Option Self o => o -> [Class]
self opts = 
  cls $ "self"-(option opts :: Seg Self)

data Self
instance Option Self Auto
instance Option Self AlignSEC
instance Option Self AlignSB

items :: Option Items o => o -> [Class]
items opts = 
  cls $ "items"-(option opts :: Seg Items)

data Items
instance Option Items AlignSEC
instance Option Items AlignSB

content :: Option Content o => o -> [Class]
content opts = 
  cls $ "content"-(option opts :: Seg Content)

data Content
instance Option Content AlignSEC
instance Option Content AlignBAE

justify :: Option Justify o => o -> [Class]
justify opts = 
  cls $ "justify"-(option opts :: Seg Justify)

data Justify
instance Option Justify AlignSEC
instance Option Justify AlignBAE

-- | Child should grow to fill available space in a flex container
-- TODO grow-0?
grow :: [Class]
grow = cls "flex-grow"


data Position
instance Option Position Pos

position :: Option Position o => o -> [Class]
position o = cls $ (option o :: Seg Position)


data Offset
instance Option Offset Size
instance Option Offset (Axis Size)

top :: Option Offset o => o -> [Class]
top o =
  cls $ "top" - (option o :: Seg Offset)

bottom :: Option Offset o => o -> [Class]
bottom o =
  cls $ "bottom" - (option o :: Seg Offset)

left :: Option Offset o => o -> [Class]
left o =
  cls $ "left" - (option o :: Seg Offset)

right :: Option Offset o => o -> [Class]
right o =
  cls $ "right" - (option o :: Seg Offset)


-- TODO inset
data Inset
instance Option Inset Size
instance Option Inset (Axis Size)

inset :: Option Inset o => o -> [Class]
inset o = cls $ "inset" - (option o :: Seg Inset)






-- | Transforms
-- > active |: translate (X Px, Y Px)

-- the "transform" property is required
translate :: Option Translate o => o -> [Class]
translate o = 
  cls $ "translate" - (option o :: Seg Translate)

data Translate
instance Option Translate (Axis Size)
instance Option Translate (Axis RelSize)

rotate :: Option Rotate o => o -> [Class]
rotate o =
  cls $ "rotate" - (option o :: Seg Rotate)

data Rotate
instance Option Rotate Rot


transform :: Option Transform o => o -> [Class]
transform o = cls $ "transform" - (option o :: Seg Transform)

data Transform
instance Option Transform None






transition :: Option Transition o => o -> [Class]
transition o = 
  cls $ "transition" - (option o :: Seg Transition)

data Transition
instance Option Transition ()
instance Option Transition Property
instance Option Transition None






duration :: Option Duration o => o -> [Class]
duration o = 
  cls $ "duration" - (option o :: Seg Duration)

data Duration
instance Option Duration Dur




data Easing
instance Option Easing Ease

easing :: Option Easing o => o -> [Class]
easing o =
  cls $ "easing" - (option o :: Seg Easing)


delay :: Option Duration o => o -> [Class]
delay o = 
  cls $ "delay" - (option o :: Seg Duration)


data Rounded

instance Option Rounded ()
instance Option Rounded None
instance Option Rounded Full
instance Option Rounded SML
instance Option Rounded (Side None)
instance Option Rounded (Side Full)
instance Option Rounded (Side SML)
instance Option Rounded (Side ())
instance Option Rounded (Corner None)
instance Option Rounded (Corner Full)
instance Option Rounded (Corner SML)
instance Option Rounded (Corner ())

rounded :: Option Rounded o => o -> [Class]
rounded o = 
  cls $ "rounded" - (option o :: Seg Rounded)





font :: Option Font o => o -> [Class]
font o =
  cls $ "font" - (option o :: Seg Font)

data Font
instance Option Font FontWeight





data FontText
instance Option FontText SML
instance Option FontText XSML
instance Option FontText Color

text :: Option FontText o => o -> [Class]
text o = cls $ "text" - (option o :: Seg FontText)





data Outline
instance Option Outline None

outline :: Option Outline o => o -> [Class]
outline o = cls $ "outline" - (option o :: Seg Outline)


data Shadow
instance Option Shadow None
instance Option Shadow ()
instance Option Shadow SML
shadow :: Option Shadow o => o -> [Class]
shadow o = cls $ "shadow" - (option o :: Seg Shadow)



zIndex :: Option ZIndex o => o -> [Class]
zIndex o = cls $ "z" - (option o :: Seg ZIndex)


data ZIndex
instance Option ZIndex Z
instance Option ZIndex Auto


instance Option Opacity Opacity

opacity :: Option Opacity o => o -> [Class]
opacity o = cls $ "opacity" - (option o :: Seg Opacity)

