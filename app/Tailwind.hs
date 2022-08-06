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
  )
  where

import Prelude hiding ((-))
import Tailwind.Types
import Tailwind.Values
import Tailwind.Classes
import Tailwind.Prefix