module Web.UI where

import Prelude hiding ((-))
import Lucid
import Lucid.Html5
import Data.Text (Text, pack, unpack)
import Data.ByteString (ByteString)
import Text.RawString.QQ (r)
import Data.String (IsString(..))
import qualified Data.Text as Text


-- Inspired by Tailwind and Elm UI


data Att

newtype UI c t a = UI { fromUI :: Html a }

instance IsString (UI c t ()) where
  fromString s = UI $ (toHtml $ pack s)


-- should also do something?
layout :: UI c t () -> Html ()
layout ui = fromUI ui

none :: UI c t ()
none = UI $ pure ()

-- list are ANNOYING when it comes to composition
-- TODO Atttributes
el :: [Att] -> UI c t () -> UI c t ()
el as ct = UI $ do
  div_ [] (fromUI ct)

bg :: Show color => color -> Attribute
bg c = class_ (className (BG c))

-- seems kinda silly
border :: Show color => color -> Attribute
border c = classes [BC c, BW T B1, BW B B1, BW L B1, BW R B1 ]

classes :: ToClass a => [a] -> Attribute
classes cls = class_ ((Text.intercalate " " $ map className cls) <> " ")

-- TODO I'm stuck, I don't want to thread the type of color all the way through the program. Yuck
-- what if I need another type??
row :: forall c t a. Show c => [Attribute] -> UI c t a -> UI c t a
row as cnt = UI $ div_ (classes [Flex Row :: Class c] : as) (fromUI cnt)


data Class' = Class'
  { className' :: Text
  , definition :: Text
  }

-- this is a ToClass thing
-- we serialize the classname
-- but what about the body? 
-- we have to do it all at once
-- then we can still dump them all out


-- col :: [Attribute] -> UI content a -> UI layout a
-- col as cnt = UI $ div_ (class_ (className (Flex Col)) : as) (fromUI cnt)

-- The point of this was that I didn't want to have to specify them twice, right?
-- can I do without Class completely?
data Class color
  = Flex Flex
  | BW Sides BSize
  | BC color
  | BG color
  deriving (Show, Eq)

instance Show color => ToClass (Class color) where
  className c = Text.toLower $ Text.replace " " "-" $ pack $ show c

data Flex = Row | Col
  deriving (Show, Eq, Enum, Bounded)

data Sides = L | R | T | B
  deriving (Show, Eq, Enum, Bounded)
instance ToStyle Sides where
  styleName L = "left"
  styleName R = "right"
  styleName T = "top"
  styleName B = "bottom"

data BSize = B0 | B1 | B2 | B4 | B6 | B8
  deriving (Show, Eq, Enum, Bounded)




instance ToValue BSize where
  value B0 = Px 0
  value B1 = Px 1
  value B2 = Px 2
  value B4 = Px 4
  value B6 = Px 6
  value B8 = Px 8

toStyles :: (ToValue color) => Class color -> [Text]
toStyles (Flex Col)     = ["display:flex", "flex-direction:column"]
toStyles (Flex Row)     = ["display:flex", "flex-direction:row"]
toStyles (BW side size) = ["border"-(styleName side)-"width" .: size]
toStyles (BC color)     = ["border-color" .: color]
toStyles (BG color)     = ["background-color" .: color]

classDefinition :: Class AppColor -> Text
classDefinition c = "." <> className c <> " { " <> (Text.intercalate ";" $ toStyles c) <> " }"

(-) :: Text -> Text -> Text
a - b = a <> "-" <> b

(.:) :: ToValue val => Text -> val -> Text
a .: b = a <> ":" <> (pack $ show $ value b)

data Units
  = Px Int
  | Color Text
  deriving (Eq)
instance Show Units where
  show (Px n) = show n <> "px"
  show (Color t) = "#" <> unpack t

class ToValue a where
  value :: a -> Units

data AppColor
  = BlackSoft
  | GrayStone
  | Purple
  | PurpleLight
  | Green
  | GreenHover
  | White
  deriving (Show, Eq, Bounded, Enum)
-- instance ToClass AppColor

instance ToValue AppColor where
  value BlackSoft = Color "555"
  value GrayStone = Color "999"
  value Purple    = Color "510C76"
  value PurpleLight = Color "862F8B"
  value Green      = Color "93E852"
  value GreenHover = Color "BDF294"
  value White      = Color "FFF"


stylesheet :: Text
stylesheet = Text.intercalate "\n" generate
  where

  generate :: [Text]
  generate = map classDefinition $ mconcat 
    [ fmap Flex range
    , fmap BC range
    , do side <- range
         size <- range
         pure $ BW side size
    , fmap BG range
    ]

  range :: (Enum a, Bounded a) => [a]
  range = [minBound..maxBound]


 


-- they aren't organized very well, are they?
-- let's make them all separate!
-- data Class
--   = Flex Flex
--   | Height Height

--   | B0
--   | B1
--   deriving (Show, Eq, Enum, Bounded)

-- data Flex
--   = Row
--   | Col

-- -- here we are... same old problem. Some will also want full
-- data Height
--   | Full

-- data ClassColors
--   | BG AppColor
--   derivinf (Show, Eq)

-- data Class = Class { className :: Text,  classBody :: Text }

-- toStyle :: Style -> Class
-- toStyle F = "display:flex"
-- toStyle R = "display:flex,flex-direction:row"
-- toStyle C = "display:flex,flex-direction:column"
-- toClass HF = "height:100%"
-- toClass B0 = "border:0px"
-- toClass B1 = "border:1px"

-- what about defaults?
-- they are applied by the function
-- can they be reset?
-- yeah, they probably should be
-- toClass (BG c) = Class ("bg-" <> toLower (pack (show c))) ("background-color:" <> toColor c)

-- toLine :: Class -> Text
-- toLine (Class c b) = "." <> c <> "{" <> b <> "}"

-- classes :: [Style] -> Attribute
-- classes cls =
--   class_ $ Text.intercalate " " $ map (className . toClass) cls

-- stylesheet :: Text
-- stylesheet = Text.intercalate "\n" $ map (toLine . toClass) $ mconcat
--   [ [ Flex, Row, Col ]
--   , map Height [minBound..maxBound]
--   ]

-- TODO make these be set somewhere?
-- stylesheet :: Text
-- stylesheet =
--   [r|
--     .f { display: flex }
--     .r { display: flex; flex-direction: row }
--     .c { display: flex; flex-direction: column }
--     .h-full { height: 100% }
--     .a-center  { align-items: center }
--     .a-stretch { align-items: stretch }
--     .a-start   { align-items: start }
--     .a-end     { align-items: end }
--     .g { flex: 1 }
--   |]





class ToClass a where
  className :: a -> Text

class ToStyle a where
  styleName :: a -> Text
  -- default classSegment :: (Show a) -> Text
  -- classSegment a = Text.toLower $ Text.pack $ show a