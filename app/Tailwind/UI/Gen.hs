module Tailwind.UI.Gen where

import Prelude
import Tailwind
import Tailwind.UI.Types (classText)
import Data.Text as Text (Text, unlines, pack, unpack, intercalate)

-- genenrate config file, which 


-- writes a tailwind.config.js file to your project root
-- containing all of your styles, so you can generate the css file
-- generateConfig :: IO ()
-- generateConfig = _



writeConfig :: Text -> IO ()
writeConfig t = writeFile "tailwind.config.js" (unpack t)


tailwindConfig :: [Text] -> Text
tailwindConfig clss = Text.intercalate "\n" $
  [ "module.exports = {"
  , "  content: [],"
  , "  theme: {},"
  , "  plugins: [],"
  , "  safelist: [\n    " <> list clss <> "\n]"
  , "}"
  ]
  where
    list as = Text.intercalate ",\n    " $ fmap quote as
    quote t = "\"" <> t <> "\""
    


-- all classes used
classWhitelist :: [Color] -> [Text]
classWhitelist clrs = mconcat $ map classText
  [ bg <$> clrs
  , bg <$> (range :: [BgSize])
  , padding <$> (range :: [Size])
  , padding <$> axes (range :: [Size])
  , padding <$> sides (range :: [Size])
  , gap <$> (range :: [Size])
  , gap <$> axes (range :: [Size])
  , border <$> (range :: [BorderSize])
  , border <$> axes (range :: [BorderSize])
  , border <$> sides (range :: [BorderSize])
  , border <$> clrs
  , border <$> axes clrs
  , border <$> sides clrs
  , height <$> (range :: [Auto])
  , height <$> (range :: [Full])
  , height <$> (range :: [Size])
  , height <$> (range :: [RelSize])
  , height <$> (range :: [ExtSize])
  , width <$> (range :: [Auto])
  , width <$> (range :: [Full])
  , width <$> (range :: [Size])
  , width <$> (range :: [RelSize])
  , width <$> (range :: [ExtSize])
  , flex <$> (range :: [Direction])
  , flex <$> (range :: [Wrap])
  , flex <$> [()]
  , basis <$> (range :: [Auto])
  , basis <$> (range :: [Full])
  , basis <$> (range :: [Size])
  , basis <$> (range :: [RelSize])
  , basis <$> (range :: [ExtSize])
  , self <$> (range :: [Auto])
  , self <$> (range :: [AlignSEC])
  , self <$> (range :: [AlignSB])
  , items <$> (range :: [AlignSEC])
  , items <$> (range :: [AlignSB])
  , items <$> (range :: [AlignSB])
  , content <$> (range :: [AlignSEC])
  , content <$> (range :: [AlignBAE])
  , [grow]
  , position <$> (range :: [Pos])
  , top <$> (range :: [Size])
  , top <$> axes (range :: [Size])
  , bottom <$> (range :: [Size])
  , bottom <$> axes (range :: [Size])
  , left <$> (range :: [Size])
  , left <$> axes (range :: [Size])
  , right <$> (range :: [Size])
  , right <$> axes (range :: [Size])
  , inset <$> (range :: [Size])
  , inset <$> axes (range :: [Size])
  , translate <$> axes (range :: [Size])
  , translate <$> axes (range :: [RelSize])
  , rotate <$> (range :: [Rot])
  , [transform None]
  , transition <$> (range :: [Property])
  , transition <$> (range :: [None])
  , transition <$> (range :: [()])
  , duration <$> (range :: [Dur])
  , easing <$> (range :: [Ease])
  , delay <$> (range :: [Dur])
  , rounded <$> (range :: [()])
  , rounded <$> (range :: [None])
  , rounded <$> (range :: [Full])
  , rounded <$> (range :: [SML])
  , rounded <$> sides (range :: [()])
  , rounded <$> sides (range :: [None])
  , rounded <$> sides (range :: [Full])
  , rounded <$> sides (range :: [SML])
  , rounded <$> corners (range :: [()])
  , rounded <$> corners (range :: [None])
  , rounded <$> corners (range :: [Full])
  , rounded <$> corners (range :: [SML])
  , font <$> (range :: [FontWeight])
  , text <$> (range :: [SML])
  , text <$> (range :: [XSML])
  , text <$> clrs
  , [outline None]
  , shadow <$> (range :: [None])
  , shadow <$> (range :: [()])
  , shadow <$> (range :: [SML])
  , zIndex <$> (range :: [Z])
  , zIndex <$> (range :: [Auto])
  , opacity <$> (range :: [Opacity])
  ]

axes :: [a] -> [Axis a]
axes as = mconcat [fmap X as, fmap Y as]

sides :: [a] -> [Side a]
sides as = mconcat [fmap T as, fmap L as, fmap R as, fmap B as]

corners :: [a] -> [Corner a]
corners as = mconcat [fmap TL as, fmap TR as, fmap BL as, fmap BR as]

range :: (Bounded r, Enum r) => [r]
range = [minBound..maxBound]
