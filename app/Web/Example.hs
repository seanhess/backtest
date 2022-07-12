module Web.Example where

import Prelude
import Web.UI
import Web.UI.Style (PixelSize(..))
import Lucid.Html5
import Data.Text (Text)

import Tailwind hiding (Black, text)
import qualified Tailwind
import Tailwind.Options


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

-- turn off overloaded lists, wahoo
-- test :: UI ()
-- test = col [id_ "asdf"] [bg Green, padding S10, gap S10, inset S1] $ do
--   el [ bg Black ] (text "hello")
--   space
--   el (text "nothing")
--   space
--   el "goodbye"


-- goal: unify border, so you can specify color and width together?


-- does it put border in front of them all?
data Border


-- option!
-- if we want to reuse the class, we'd better change it here
border :: (Option Color color, Option BorderWidth width) => color -> width -> [Class]
border c w = borderColor c <> borderWidth w

pad :: Option Padding p => p -> [Class]
pad = padding

-- -- yeah it doesn't like this!
-- instance (Option Border a, Option Border b) => Option Border (a, b) where
--   option (w, b) = option w

-- instance Option Border BorderSize where
--   option b = seg b 

-- TODO unify border
-- TODO fix type annotations :(
-- TODO simplify classes to only use one list. we don't get enough of a benefit. 
-- go ahead and re-export everything. We don't really want to pass straight through anyway

layoutTest :: UI ()
layoutTest = do
  col [ bg Black, height Full ] $ do

    -- TODO it isn't obvious that "items Center" is an option for row
    -- it's kind of messy either way
    row [ items Center
        , bg Gray
        , pad P2
        , hover |: (translate (X Px) <> translate (Y Px))
        ] $ do
      button_ [ bg Purple, hover |: bg PurpleLight, width P80, pad P12, Tailwind.text White ] ("Left" :: UI ())
      space
      el [ bg Green, border Black B0 ] (text "border Black")
      space

      button_ [ bg Purple, hover |: bg PurpleLight, pad P12, width P80, Tailwind.text White ] ("Right" :: UI ())
    col [ bg White, grow, pad P32, gap P24 ] $ do
      el [id_ "asdf"] [ bg Gray, pad P32, borderWidth B1 ] (text "border 1")
      el [ bg Gray, pad P32, pad (X P40) ] ("pad x 10" :: UI ())

      el [id_ "woot"]
         [ bg Gray
         , pad P48
         , border Black B8
         ] $
        text "Border (Black, B8)"

      -- TODO like this
      -- el [ id_ "woot"
      --    , bg Gray , pad P48 , border Black B8
      --    ] $
      --   text "Border (Black, B8)"

    space
    el [ bg GreenHover ] $ do
      el [ flex () ] "Bottom"




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