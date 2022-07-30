{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tailwind.UI.Types where

import Prelude
import Tailwind.Options
import Lucid
import Data.String (IsString(..))
import qualified Data.Text as Text

-- | UI instead of HTML

type Att a = Opt a -> Opt a

newtype UI a = UI { fromUI :: Lucid.Html a }
  deriving (Functor, Applicative, Monad)

instance IsString (UI ()) where
  fromString s = UI $ Lucid.toHtml s

instance Show (UI a) where
  show (UI h) = "UI " <> show h



data Opt a = Opt [Attribute] [[Class]]



opt :: Opt a
opt = Opt [] []

addClass :: [Class] -> Opt a -> Opt a
addClass c (Opt as css) = Opt as $ c:css

atts :: Opt a -> [Attribute]
atts (Opt as css) =
  classAttribute css : as

-- | Convert classes into an Attribute
classAttribute :: [[Class]] -> Attribute
classAttribute cns = class_ (Text.intercalate " " (map fromClass (mconcat cns)) <> " ")

