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
(|:) :: Prefix -> Class -> Class
(Prefix p) |: (Class c) =
  Class (apply c)
  where
    -- ignore transform
    apply "transform" = "transform"
    apply c' = p <> ":" <> c'


active :: Prefix
active = "active"

hover :: Prefix
hover = "hover"

focus :: Prefix
focus = "focus"

