{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.UI.Types where

import Prelude
import Tailwind.Types
import qualified Tailwind.Prefix as Tailwind
import Lucid
import Data.String (IsString(..))
import Data.Text as Text (Text, intercalate)

-- | UI instead of HTML. Doesn't provide any utility, unless we are restricting content to specific tags
-- Can always bring it back if that becomes useful

-- newtype UI t a = UI { fromUI :: Lucid.Html a }
--   deriving (Functor, Applicative, Monad)

-- instance IsString (UI t ()) where
--   fromString s = UI $ Lucid.toHtml s

-- instance Show (UI t a) where
--   show (UI h) = "UI " <> show h

data Opt a = Opt [Attribute] [[Class]]
  deriving (Show, Eq)


opt :: Opt a
opt = Opt [] []

type Att a = Opt a -> Opt a

-- this is a FUNCTION
addClass :: [Class] -> Opt a -> Opt a
addClass c (Opt as css) = Opt as $ c:css

addAttribute :: Attribute -> Att a
addAttribute a (Opt as css) = Opt (a:as) css

atts :: Opt a -> [Attribute]
atts (Opt as css) =
  classAttribute css : as

-- | Convert classes into an Attribute
classAttribute :: [[Class]] -> Attribute
classAttribute cns = class_ $ (Text.intercalate " " (classText cns) <> " ")

classText :: [[Class]] -> [Text]
classText cns = (map fromClass (mconcat cns)) 

-- TODO make sure purging works!
(|:) :: Tailwind.Prefix -> (Opt a -> Opt a) -> Att a
(|:) p f o = mapFirstClass (Tailwind.addPrefix p) $ f o
  where
    mapFirstClass f_ (Opt as (cs:css)) = Opt as (f_ cs:css)
    mapFirstClass f_ o' = o'
infixr 9 |:

