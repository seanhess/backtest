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

import Tailwind hiding (gap, Black, text, width, padding)
-- import qualified Tailwind
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
example = col (background Green . alignItems Center . gap S10 . padding S10) $ do
  text "hello"
  text "goodbye"
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

-- TODO unify border
-- TODO fix type annotations :(
-- TODO simplify classes to only use one list. we don't get enough of a benefit. 
-- go ahead and re-export everything. We don't really want to pass straight through anyway

-- layoutTest :: UI t ()
-- layoutTest = do

--   box def { direction = Row } ! [ bg Black ] $ do
--     "HI"

--   col ! [ bg Black, height Full ] $ do

--     -- TODO it isn't obvious that "itemu Center" is an option for row
--     -- it's kind of messy either way
--     row ! [ items Center
--           , bg Gray
--           , pad P2
--           , hover |: (translate (X Px) <> translate (Y Px))
--           ] $ do
--       -- button_ ! [ bg Purple, hover |: bg PurpleLight, width P80, pad P12, Tailwind.text White ] $ text "Left"
--       space
--       el ! [ bg Green, border Black B0 ] $ text "border Black"
--       space

--       -- button_ ! [ bg Purple, hover |: bg PurpleLight, pad P12, width P80, Tailwind.text White ] $ "Right" :: UI ()
--     col ! [ bg White, grow, pad P32, gap P24 ] $ do
--       el ! id_ "asdf" ! [ bg Gray, pad P32, borderWidth B1 ] $ text "border 1"
--       el ! [ bg Gray, pad P32, pad (X P40) ] $ "pad x 10" :: UI t ()

--       el ! id_ "woot"
--          ! [ bg Gray
--            , pad P48
--            , border Black B8
--            ] $
--         "Border (Black, B8)"

--       -- TODO like this
--       -- el [ id_ "woot", onClick_ wahoo
--       --    , bg Gray , pad P48 , border Black B8
--       --    ] $
--       --   text "Border (Black, B8)"

--     space
--     -- el ! [ bg GreenHover ] $ do
--     --   el ! [ flex () ] "Bottom"

--     -- oh because I want it to happen before function composition?
--     -- this only works 
--     el ! [ flex () ] $ "Bottom"

--     -- table (TableConfig rows)





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