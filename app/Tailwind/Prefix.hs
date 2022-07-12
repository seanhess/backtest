{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tailwind.Prefix where

import Prelude
import Tailwind.Options
import Data.Text as Text (Text)
import Data.String (IsString)

newtype Prefix = Prefix { fromPrefix :: Text }
  deriving (IsString)

-- * Prefixes, only apply styles in certain situations
-- | Apply a prefix to classes  
-- > [ bg Green, active |: bg Green ]
(|:) :: Prefix -> [Class] -> [Class]
(Prefix p) |: cs =
  -- apply cs
  map apply cs
  where
    -- ignore transform
    apply "transform" = "transform"
    apply (Class c') = Class $ p <> ":" <> c'


active :: Prefix
active = "active"

hover :: Prefix
hover = "hover"

focus :: Prefix
focus = "focus"

