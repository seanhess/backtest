module Web.UI.LayoutTest where

import Prelude
import Lucid
import Lucid.Html5
import Web.UI
import Web.UI.Attributes

-- TODO: Spacing vs Padding!
-- spacing = gap

-- layoutTest :: Html ()
-- layoutTest = layout $ do
--   row [ width Fill, padding 10, background GrayLighter ] $ do
--     el [] "Left"
--     space
--     el []  "Center"
--     space
--     el [] "Right"
--   row [ height Fill, width Fill, background Purple ] $ do
--     el [ ] "Content"
--     space
--     col [ height Fill, background White ] $ do
--       el [] "Right"
--       el [] "More content"
--       space
--       el [] "test"
--   row [ width Fill, background GreenLight ] $ el [] "Bottom"



layoutTest :: Html ()
layoutTest = do
  pure ()
  -- col [ bg BlackSoft, height Full ] $ do
  --   row [ flex Center, bg GrayStone, padding (Rem 0.5) ] $ do
  --     -- , active |: translate (X Px, Y Px) 
  --     button_ [ bg Purple, hover |: bg PurpleLight, width (Rem 5), padding (Rem 0.75), text White ] "Left"
  --     space
  --     div_ [ bg Green, border BlackSoft ] "border Black"
  --     space
  --     button_ [ bg Purple, hover |: bg PurpleLight, padding (Rem 0.75), width (Rem 5), text White ] "Right"
  --   col [ bg White, self Grow, padding (Rem 2), gap (Rem 1.5) ] $ do
  --     div_ [ bg GrayStone, padding (Rem 2), border ()] "border ()"
  --     div_ [ bg GrayStone, padding (Rem 2, X (Rem 2.5)) ] "padding x 10"
  --     div_ [ bg GrayStone, padding (Rem 3), border (BlackSoft, (Px 8)) ] "Border (Black, B8)"
  --   space
  --   div_ [ bg GreenHover ] $ do
  --     div_ [ flex () ] "Bottom"
