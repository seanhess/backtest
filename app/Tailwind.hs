{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}

module Tailwind
  ( 
  -- * Classes
    module Tailwind.Classes

  -- * Prefixes
  , module Tailwind.Prefix

  -- * Option
  , module Tailwind.Types
  , module Tailwind.Options
  )
  where

import Prelude
import Tailwind.Types hiding ((-))
import Tailwind.Values
import Tailwind.Classes
import Tailwind.Prefix
import Tailwind.Options