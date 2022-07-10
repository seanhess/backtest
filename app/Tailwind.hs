{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}

module Tailwind
  (
  -- * Display
    bg, Color
  , height, width, Dimensions
  , borderWidth, borderColor, BorderWidth, BorderSize(..)
  , rounded, Rounded
  , font, Font, FontWeight(..)
  , text, FontText
  , outline, Outline

  -- * Spacing
  , padding, Padding
  , gap, Gap

  -- * Layout
  , flex, Direction(..), Align(..)
  , self
  , grow -- , row, col, space

  -- * Position
  , position, Position(..)
  , top, bottom, left, right
  , inset
  , zIndex, Z(..)

  -- * Transforms
  , translate
  , transform
  , transition, rotate, easing, duration, Duration(..), Rotate(..), Easing(..), Property(..)

  -- * Effects
  , shadow, Shadow
  , opacity, Opacity(..)


  -- * Prefixes
  , (|:), active, hover, focus

  -- * Option
  , Option(..), Segment(..)
  , Auto(..), Full(..)
  , Size(..), RelSize(..), ExtSize(..)
  , SML(..), XSML(..)
  , Side(..)
  , Axis(..)
  , None(..)
  )
  where

import Prelude hiding ((-))
import Data.String.Conversions (cs)
import Data.Text as Text (Text, pack, toLower)
import Tailwind.Options
import Tailwind.Size
import Tailwind.Prefix
import qualified Data.Text as Text



-- | you should create your own app colors, but if you haven't set them yet, you can use a color via it's text name
newtype Color = Color Text
instance Segment Color where
  seg (Color c) = Seg c

bg :: Option Color o => o -> [Class]
bg o = 
  cls $ "bg" - (option o :: Seg Color)




data Padding

-- Padding doesn't hyphenate the x and y for some reason
instance Option Padding Size where
  option s = "p" - (seg s)
instance Option Padding (Axis Size) where
  option s = "p" <> (seg s)
instance Option Padding (Side Size) where
  option s = "p" <> (seg s)

padding :: Option Padding o => o -> [Class]
padding o =
  cls $ (option o :: Seg Padding)

-- PROBLEM:
-- px-0
-- p-0

-- border-x-0
-- border-0


borderWidth :: Option BorderWidth o => o -> [Class]
borderWidth o = 
  cls $ "border" - (option o :: Seg BorderWidth)

data BorderWidth
instance Option BorderWidth BorderSize
instance Option BorderWidth (Side BorderSize)
instance Option BorderWidth (Axis BorderSize)

borderColor :: Option Color o => o -> [Class]
borderColor o = 
  cls $ "border" - (option o :: Seg Color)




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






instance Segment Direction where
  seg = segHyphens

data Align
  = Stretch
  | Center
  | Start
  | End
  | Baseline
  deriving (Bounded, Enum, Show)
instance Segment Align where
  seg = segHyphens



flex :: Option Flex o => o -> [Class]
flex opts = 
  "flex" : cls ("flex"-(option opts :: Seg Flex))


data Flex
instance Option Flex Direction
instance Option Flex ()

data Direction
  = Row
  | Col
  deriving (Bounded, Enum, Show)



self :: Option Self o => o -> [Class]
self opts = 
  cls $ "self"-(option opts :: Seg Self)

data Self
instance Option Self Auto
instance Option Self Align

-- | Child should grow to fill available space in a flex container
-- TODO grow-0?
grow :: [Class]
grow = cls "flex-grow"


data Position
  = Static
  | Fixed
  | Absolute
  | Relative
  | Sticky
  deriving (Enum, Bounded, Show)
instance Segment Position where
  seg = segHyphens

instance Option Position Position

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




-- -- * Layout Helpers
-- -- | Equivalent to div_ [ flex Row ]
-- row :: [ Attribute ] -> Html () -> Html ()
-- row as = div_ (as <> [flex Row])

-- -- | Equivalent to div_ [ flex Col ]
-- col :: [ Attribute ] -> Html () -> Html ()
-- col as = div_ (as <> [flex Col])

-- -- | Create a spacer, useful for aligning elements to the right, bottom, or center
-- -- > row [] $ do "left"; space; "center"; space; "right"
-- -- > row [] $ do space; "right"
-- -- > col [] $ do "top"; space; "bottom"
-- space :: Html ()
-- space = div_ [ grow ] ""






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

instance Option Rotate Rotate

data Rotate = R0 | R1 | R2 | R3 | R6 | R12 | R45 | R90 | R180
  deriving (Show)
instance Segment Rotate where
  seg = segDropPrefix

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

data Property
  = All
  | Colors
  | Transform
  | Shadow
  | Opacity
  deriving (Show)
instance Segment Property where
  seg = segHyphens





duration :: Option Duration o => o -> [Class]
duration o = 
  cls $ "duration" - (option o :: Seg Duration)

instance Option Duration Duration

data Duration
  = D75
  | D100
  | D150
  | D200
  | D300
  | D500
  | D700
  | D1000
  deriving (Show)
instance Segment Duration where
  seg = segDropPrefix




easing :: Option Easing o => o -> [Class]
easing o =
  cls $ "easing" - (option o :: Seg Easing)

instance Option Easing Easing

data Easing
  = Linear
  | InOut
  | Out
  | In
  deriving (Show)
instance Segment Easing where
  seg = segHyphens


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
instance Option Rounded (Corners None)
instance Option Rounded (Corners Full)
instance Option Rounded (Corners SML)
instance Option Rounded (Corners ())

rounded :: Option Rounded o => o -> [Class]
rounded o = 
  cls $ "rounded" - (option o :: Seg Rounded)





font :: Option Font o => o -> [Class]
font o =
  cls $ "font" - (option o :: Seg Font)

instance Option Font FontWeight

data Font
data FontWeight
  = Thin
  | ExtraLight
  | Light
  | Normal
  | Medium
  | Semibold
  | Bold
  | Extrabold
  | Black
  deriving Show

instance Segment FontWeight where
  seg fw = Seg $ Text.toLower $ pack $ show fw


data FontText
instance Option FontText SML
instance Option FontText XSML

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



zIndex :: Option Z o => o -> [Class]
zIndex o = cls $ "z" - (option o :: Seg Z)

data Z
  = Z0
  | Z10
  | Z20
  | Z30
  | Z40
  | Z50
  deriving (Show)
instance Segment Z where
  seg = segDropPrefix

instance Option Z Z
instance Option Z Auto


data Opacity
  = O0
  | O5
  | O10
  | O20
  | O25
  | O30
  | O40
  | O50
  | O60
  | O70
  | O75
  | O80
  | O90
  | O100
  deriving (Show)
instance Segment Opacity where
  seg = segDropPrefix
instance Option Opacity Opacity

opacity :: Option Opacity o => o -> [Class]
opacity o = cls $ "opacity" - (option o :: Seg Opacity)



-- * Utilties

-- | Add a class prefixed with a space so they concatenate
cls :: Seg a -> [Class]
cls (Seg t) = [Class t]
