module Web.UI.Attributes where

import Prelude hiding ((-))
import Lucid
import Data.Text as Text (Text, intercalate, pack)
import Web.UI.Types

-- | Map classes to a single class attribute. Always
-- append a space in case someone else adds to class
classes :: ToClass c => [c] -> Attribute
classes cls = class_ ((Text.intercalate " " $ map (name . toClass) cls) <> " ")

flex :: Flex -> Attribute
flex f = classes [Flex f]

bg :: (ToValue color, ClassName color) => color -> Attribute
bg c = classes [BG c]




-- layout

-- options!
-- hmm... that's interesting
-- I wonder if I like it?

-- can it accept multiple options
-- people want to work in pixels
-- but we want to scale via REM


-- this could be a natural conversion
-- it's always pixels -> rem
-- 0, 1, 2, 4, 6, 8, 10, 12, 14, 16, 20, 24, 28, 32, 36, 40, 44, 48, 56, 64, 80, 96, 112, 128, 144, 160, 176, 192, 208, 224, 240, 256, 288, 320, 384

-- this is the default
-- you can extend it by making your own
data Size = S0 | S1 | S2 | S4 | S6 | S8 | S10 | S12 | S14 | S16 | S20 | S24 | S28 | S32 | S36 | S40 | S44 | S48
  deriving (Show, Eq, Enum, Bounded)
instance ClassName Size where
  className s = pack $ drop 1 $ show s
instance ToValue Size where
  value S0 = Px 0
  value S1 = Px 1
  value S2 = Rem 0.125
  value S4 = Rem 0.25
  value S6 = Rem 0.375
  value S8 = Rem 0.5
  value S10 = Rem 0.625
  value S12 = Rem 0.75
  value S14 = Rem 0.875
  value S16 = Rem 1
  value S20 = Rem 1.25
  value S24 = Rem 1.5
  value S28 = Rem 1.75
  value S32 = Rem 2
  value S36 = Rem 2.25
  value S40 = Rem 2.5
  value S44 = Rem 2.75
  value S48 = Rem 3

data XY a = XY
  { x :: a
  , y :: a
  }

data Sides' a = Sides'
  { left :: a
  , right :: a
  }

padding :: Size -> Attribute
padding s = classes [Pad L s, Pad R s, Pad T s, Pad B s]

-- Hmm, no, we want some kind of a size attribute
-- gap :: Int -> Attribute
-- gap a = _


-- doesn't depend on color
data Class0
  = Flex Flex
  | Pad Sides Size
  deriving (Show, Eq)

instance ClassName Class0


data Class1 color
  = BW Sides BSize
  | BC color
  | BG color
  deriving (Show, Eq)

instance Show color => ClassName (Class1 color)




class ToClass a where
  toClass :: a -> Class

instance ToClass Class0 where
  toClass c@(Flex Row) = Class "fr" ["display:flex", "flex-direction:row"]
  toClass c@(Flex Col) = Class "fc" ["display:flex", "flex-direction:column"]
  toClass c@(Pad side size)    =
    Class ("p"-(className side)-(className size))
          ["padding" .: size]

instance (ToValue color, ClassName color) => ToClass (Class1 color) where
  toClass c@(BW side size) =
    Class ("bdw"-(className side)-(className size))
          ["border"-(styleName side)-"width" .: size]

  toClass c@(BC color) =
    Class ("bdc"-className color)
          ["border-color" .: color]

  toClass c@(BG color) =
    Class ("bgc"-className color) ["background-color" .: color]





-- VALUES ----
-----------------------------------

data Flex = Row | Col
  deriving (Show, Eq, Enum, Bounded)

-- put these all together

data Sides = L | R | T | B
  deriving (Show, Eq, Enum, Bounded)
instance ToStyle Sides where
  styleName L = "left"
  styleName R = "right"
  styleName T = "top"
  styleName B = "bottom"
instance ClassName Sides

data BSize = B0 | B1 | B2 | B4 | B6 | B8
  deriving (Show, Eq, Enum, Bounded)
instance ClassName BSize

instance ToValue BSize where
  value B0 = Px 0
  value B1 = Px 1
  value B2 = Px 2
  value B4 = Px 4
  value B6 = Px 6
  value B8 = Px 8
