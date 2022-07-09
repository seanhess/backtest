{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}

module Lucid.Tailwind
  -- (
  -- -- * Display
  --   bg, Color
  -- , height, width, Dimensions
  -- , border, Border, BorderSize(..)
  -- , rounded, Rounded
  -- , font, Font, FontWeight(..)
  -- , text, FontText
  -- , outline, Outline

  -- -- * Spacing
  -- , padding, Padding
  -- , gap, Gap

  -- -- * Layout
  -- , flex, Direction(..), Align(..), AlignItems(..)
  -- , self
  -- , grow, row, col, space

  -- -- * Position
  -- , position, Position(..)
  -- , top, bottom, left, right
  -- , inset
  -- , zIndex, Z(..)

  -- -- * Transforms
  -- , translate
  -- , transition, Duration(..), Easing(..), Property(..)

  -- -- * Effects
  -- , shadow, Shadow
  -- , opacity, Opacity(..)


  -- -- * Prefixes
  -- , (|:), active, hover, focus

  -- -- * Option
  -- , Option(..), Segment(..)
  -- , Auto(..), Full(..)
  -- , Size(..), RelSize(..), ExtSize(..)
  -- , SML(..), XSML(..)
  -- , Sides(..)
  -- , Axes(..)
  -- , None(..)
  -- )
  where

import Prelude hiding ((-))
import Data.String.Conversions (cs)
import Data.Text (Text)
-- import Lucid
-- import Lucid.Base (Attribute(..))
-- import Lucid.Html5
import Lucid.Tailwind.Options
import Lucid.Tailwind.Size
import qualified Data.Text as Text



data Color

-- these should all be classes, not attributes!
bg :: Option Color o => o -> Class
bg o = 
  cls $ "bg" - (option o :: Seg Color)


data FontText
instance Option FontText SML
instance Option FontText XSML

text :: Option FontText o => o -> Class
text o = cls $ "text" - (option o :: Seg FontText)


-- | Space around child content
-- p-8
-- px-8
-- MOST: do NOT add hyphen
-- Size: DO add a hyphen


data Padding
instance Option Padding Size where
  option s = "p" - (seg s)
instance Option Padding (Axes Size) where
  option s = "p" <> (seg s)
instance Option Padding (Sides Size) where
  option s = "p" <> (seg s)

padding :: Option Padding o => o -> Class
padding o =
  cls $ (option o :: Seg Padding)

-- PROBLEM:
-- px-0
-- p-0

-- border-x-0
-- border-0

data BorderWidth
instance Option BorderWidth BorderSize
instance Option BorderWidth (Sides BorderSize)
instance Option BorderWidth (Axes BorderSize)

borderWidth :: Option BorderWidth o => o -> Class
borderWidth o = 
  cls $ "border" - (option o :: Seg BorderWidth)



-- -- | The distance between child elements
data Gap
instance Option Gap Size
instance Option Gap (Axes Size)

gap :: Option Gap o => o -> Class
gap o =
  cls $ "gap" - (option o :: Seg Gap)

-- | Width and Height
data Dimensions
instance Option Dimensions Auto
instance Option Dimensions Full
instance Option Dimensions Size
instance Option Dimensions RelSize
instance Option Dimensions ExtSize

height :: Option Dimensions o => o -> Class
height o =
  cls $ "h" - (option o :: Seg Dimensions)

width :: Option Dimensions o => o -> Class
width o =
  cls $ "w" - (option o :: Seg Dimensions)




-- | Flex allows for all kinds of layout options
data Flex

data Direction
  = Row
  | Col
  deriving (Bounded, Enum)

instance Segment Direction where
  seg Row = "row"
  seg Col = "col"

data Align
  = Stretch
  | Center
  | Start
  | End
  | Baseline
  deriving (Bounded, Enum)

instance Segment Align where
  seg Stretch = "stretch"
  seg Start   = "start"
  seg End     = "end"
  seg Center  = "center"
  seg Baseline = "baseline"


-- really, I'm going to make them do both?
instance Option Flex Direction
instance Option Flex ()


flex :: Option Flex o => o -> Class
flex opts = 
  cls $ "flex"-(option opts :: Seg Flex)

data Self
instance Option Self Auto
instance Option Self Align

self :: Option Self o => o -> Class
self opts = 
  cls $ "self"-(option opts :: Seg Self)

-- | Child should grow to fill available space in a flex container
-- TODO grow-0?
grow :: Class
grow = cls "flex-grow"


data Position
  = Static
  | Fixed
  | Absolute
  | Relative
  | Sticky
  deriving (Enum, Bounded)
instance Segment Position where
  seg Static = "static"
  seg Fixed = "fixed"
  seg Absolute = "absolute"
  seg Relative = "relative"
  seg Sticky = "sticky"

instance Option Position Position

position :: Option Position o => o -> Class
position o = cls $ (option o :: Seg Position)


data Offset
instance Option Offset Size
instance Option Offset (Axes Size)

top :: Option Offset o => o -> Class
top o =
  cls $ "top" - (option o :: Seg Offset)

bottom :: Option Offset o => o -> Class
bottom o =
  cls $ "bottom" - (option o :: Seg Offset)

left :: Option Offset o => o -> Class
left o =
  cls $ "left" - (option o :: Seg Offset)

right :: Option Offset o => o -> Class
right o =
  cls $ "right" - (option o :: Seg Offset)


-- TODO inset
data Inset
instance Option Inset Size
instance Option Inset (Axes Size)

inset :: Option Inset o => o -> Class
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





-- -- * Prefixes, only apply styles in certain situations
-- -- | Apply a prefix to classes  
-- -- > [ bg Green, active |: bg Green ]
-- (|:) :: Prefix -> Class -> Class
-- prefix |: (Class "class" val) =
--   cls (Text.unwords $ map apply $ Text.words val)
--   where
--     -- ignore transform
--     apply "transform" = "transform"
--     apply c = prefix <> ":" <> c
-- -- ignore non-class attributes
-- prefix |: a = a


-- type Prefix = Text

-- active :: Prefix
-- active = "active"

-- hover :: Prefix
-- hover = "hover"

-- focus :: Prefix
-- focus = "focus"



-- -- | Transforms
-- -- > active |: translate (X Px, Y Px)
-- data Translate
-- instance Option Translate (Axes Size)
-- instance Option Translate (Axes RelSize)

-- -- add transform, it's required
-- translate :: Option Translate o => o -> Class
-- translate o = 
--   cls $ merge $ addTag "transform" $ addPrefix "translate" ((options o) :: Seg Translate)


-- data Transition

-- data Duration
--   = D75
--   | D100
--   | D150
--   | D200
--   | D300
--   | D500
--   | D700
--   | D1000
-- instance Segment Duration where
--   seg d = "duration-" <> s d
--     where s D75   = "75"
--           s D100  = "100"
--           s D150  = "150"
--           s D200  = "200"
--           s D300  = "300"
--           s D500  = "500"
--           s D700  = "700"
--           s D1000 = "1000"


-- data Easing
--    = Ease
--    | EaseInOut
--    | EaseIn
--    | EaseOut
-- instance Segment Easing where
--   seg Ease = "ease"
--   seg EaseIn = "ease-in"
--   seg EaseOut = "ease-out"
--   seg EaseInOut = "ease-in-out"


-- data Property
--   = Width
--   | Height
-- instance Segment Property where
--   seg p = "transition-" <> s p
--     where s Width = "width"
--           s Height = "height"


-- instance Option Transition ()
-- instance Option Transition Duration
-- instance Option Transition Easing
-- instance Option Transition Property

-- -- no, this isn't quite right...
-- transition :: Option Transition o => o -> Class
-- transition o = 
--   cls $ merge $ (["transition"] <> (options o) :: Seg Transition)


-- -- duration :: Option Duration o => o -> Class
-- -- duration o = 
-- --   cls $ merge $ addPrefix "duration" ((options o) :: Seg Duration)




-- data Rounded

-- instance Option Rounded None
-- instance Option Rounded Full
-- instance Option Rounded SML
-- instance Option Rounded (Sides None)
-- instance Option Rounded (Sides Full)
-- instance Option Rounded (Sides SML)
-- instance Option Rounded (Sides ()) where
--   options (T a) = ["t"]
--   options (B a) = ["b"]
--   options (L a) = ["l"]
--   options (R a) = ["r"]
-- instance Option Rounded ()

-- rounded :: Option Rounded o => o -> Class
-- rounded o = 
--   cls $ merge $ (addPrefix "rounded" (options o) :: Seg Rounded)


-- data Font
-- data FontWeight
--   = Thin
--   | ExtraLight
--   | Light
--   | Normal
--   | Medium
--   | Semibold
--   | Bold
--   | Extrabold
--   | Maxbold
--   deriving Show

-- instance Segment FontWeight where
--   seg Maxbold = "black"
--   seg fw = Text.toLower $ cs $ show fw

-- instance Option Font FontWeight

-- font :: Option Font o => o -> Class
-- font o =
--   cls $ merge $ (addPrefix "font" (options o) :: Seg Font)





-- data Outline
-- instance Option Outline None

-- outline :: Option Outline o => o -> Class
-- outline o = cls $ merge $ addPrefix "outline" (options o :: Seg Outline)


-- data Shadow
-- instance Option Shadow None
-- instance Option Shadow ()
-- instance Option Shadow SML
-- shadow :: Option Shadow o => o -> Class
-- shadow o = cls $ merge $ addPrefix "shadow" (options o :: Seg Shadow)


data Z
  = Z0
  | Z10
  | Z20
  | Z30
  | Z40
  | Z50
instance Segment Z where
  seg Z0 = "0"
  seg Z10 = "10"
  seg Z20 = "20"
  seg Z30 = "30"
  seg Z40 = "40"
  seg Z50 = "50"
instance Option Z Z
instance Option Z Auto

-- zIndex :: Option Z o => o -> Class
-- zIndex o = cls $ merge $ addPrefix "z" (options o :: Seg Z)


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
instance Segment Opacity where
  seg O0  = "0"
  seg O5  = "5"
  seg O10 = "10"
  seg O20 = "20"
  seg O25 = "25"
  seg O30 = "30"
  seg O40 = "40"
  seg O50 = "50"
  seg O60 = "60"
  seg O70 = "70"
  seg O75 = "75"
  seg O80 = "80"
  seg O90 = "90"
  seg O100 = "100"
instance Option Opacity Opacity

-- opacity :: Option Opacity o => o -> Class
-- opacity o = cls $ merge $ addPrefix "opacity" (options o :: Class Opacity)



-- -- * Utilties

-- | Add a class prefixed with a space so they concatenate
cls :: Seg a -> Class
cls (Seg t) = Class t

-- TODO Lucid helper, in separate file
-- classes :: [Class] -> Class
-- classes cns = class_ (Text.intercalate " " (map fromClass cns) <> " ")