module Web.Example where

import Prelude
import Data.Function ((&))
import Web.UI
import Web.UI.Style (PixelSize(..))
import Lucid.Html5
import Lucid (Html, with)
import Lucid.Base (makeAttribute, Attribute)
import Data.Text (Text)
import GHC.Exts (IsList(..))

-- TODO export types separately from the attributes and everything
-- move the types into Types
import Tailwind hiding (gap, Black, text, width, padding, grow, height)
import qualified Tailwind
-- import Tailwind.Options


data AppColor
  = Green
  | GreenHover
  | Black
  | White
  | Gray
  | Purple
  | PurpleLight
instance Segment AppColor where
  seg Green = "green-500"
  seg GreenHover = "green-200"
  seg Black = "black"
  seg White = "white"
  seg Gray = "gray-500"
  seg Purple = "purple-500"
  seg PurpleLight = "purple-200"
instance Option Color AppColor
instance Option FontText AppColor



-- this is beautiful!


example :: UI Node ()
example = col (bg Green . alignItems Center . gap S10 . pad S10) $ do
  text "hello"
  text "goodbye"

  -- just add a little sugar to tailwind
  col (grid (colSpan R2, rowStart GR1) . bg Green . id_ "hello")
  el (align Start) $ text "ok"
  el (align End) $ row (alignItems Start . gap S1) $ do
      el (width P40) $ text "one"
      el (width P12) $ text "two"
      el (width P6) $ text "three"

-- turn off overloaded lists, wahoo
-- test :: UI ()
-- test = col [id_ "asdf"] [bg Green, padding S10, gap S10, inset S1] $ do
--   el [ bg Black ] (text "hello")
--   space
--   el (text "nothing")
--   space
--   el "goodbye"




-- goal: unify border, so you can specify color and width together?


-- -- does it put border in front of them all?
-- data Border

-- -- option!
-- -- if we want to reuse the class, we'd better change it here
-- border :: (Option Color color, Option BorderWidth width) => color -> width -> [Class]
-- border c w = borderColor c <> borderWidth w

-- pad :: Option Padding p => p -> [Class]
-- pad = padding

-- -- yeah it doesn't like this!
-- instance (Option Border a, Option Border b) => Option Border (a, b) where
--   option (w, b) = option w

-- instance Option Border BorderSize where
--   option b = seg b 


layoutTest :: UI [Node] ()
layoutTest = el (height Full) $ col (bg Black) $ do
  -- there are almost no el tags
  row ( alignItems Center . bg Gray . pad P8 ) $ do
    -- , active |: translate (X Px, Y Px) 
    -- I want this to auto-size, which is the default
    button [ Tailwind.bgColor Purple, hover |: Tailwind.bgColor PurpleLight, Tailwind.width P8, Tailwind.padding P12, Tailwind.text White ] "Left"
    space

    -- We still want to be clear what is allowed on which element
    -- can elements have a background?, or do they need a BOX
    row (bg Green . border Black B1) $ text "border Black"
    space
    button [ Tailwind.bgColor Purple, hover |: Tailwind.bgColor PurpleLight, Tailwind.padding P12, Tailwind.width P80, Tailwind.text White ] "Right"
  el grow $ col ( bg White . pad P32 . gap P24 ) $ do
    col ( bg Gray . pad P32 . border Black B1 ) $ text "border ()"
    col ( bg Gray . pad P32 . pad (X P28) )     $ text "padding x 10"
    col ( bg Gray . pad P32 . border Black B8 ) $ text  "Border (Black, B8)"
  space
  col (bg Green) "Bottom"





----------------------------------------------
-- @: Operator example
----------------------------------------------


-- (@:) :: (ToAttributes a, AddAttributes f) => f -> a -> f
-- hf @: a = -- with hf (attributes a)



-- data TableColumn t a = TableColumn
--   { header :: UI t ()
--   , view :: a -> UI t ()
--   }

-- table :: [TableColumn t a] -> [a] -> UI t ()
-- table _ _ = undefined
--   -- 1. List of data
--   -- 2. Column template
--   -- 3. Column header template?

-- style' :: Text -> Attribute
-- style' t = style_ t

-- cols :: [TableColumn t Int]
-- cols = undefined

-- xs :: [Int]
-- xs = [1,2,3]

-- testExample :: UI t ()
-- testExample = do

--   el ! [bg Green, border Black B2, borderWidth (X B4)]
--      ! [id_ "asdf", style_ "okgo" ]
--      $ do
--     el $ text "one"
--     el $ text "two"
--     el $ text "three"

--   -- it won't work because it only works on Html -> Html :(
--   table cols xs
--     ! id_ "asdf"
--     ! [bg Green]

--   el ! id_ "asdf" $ do
--     text "one"

--   el ! [bg Green, border Black B2] $ do
--     text "one"

--   el ! (bg Green, border Black B2) $ do
--     text "one"

--   el ! bg Green $ do
--     text "one"

--   el ! [id_ "asdf"] $ do
--     text "one"

--   el ! [id_ "asdf", style_ "asdf"] $ do
--     text "one"

--   el ! id_ "asdf" ! style' "asdf" $ do
--     text "one"

--   el ! [id_ "asdf", style_ "asdf"] ! [bg Green] $ do
--     text "one"


--   el $ do
--     text "one"


  



-- well, wait a minute
-- why would you ever want a thing without specifying any attributes?
-- unless it was a specific thing?
-- I want to add a background

-- this is very similar!
-- row :: UI a -> UI a, it doesn't wrap it, it just adds the layout stuff it needs
-- row $ do

-- row $ el [bg Green] $ do
--   col $ el [bg Blue] $ do
--     "hello"
--     el [bold] "hello"     <-- this is how you apply stuff to ti
--     "goodbye"