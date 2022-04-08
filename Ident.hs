{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ident where

import qualified Prelude as C (Eq, Ord, Show(..), Read, String)
import qualified Data.String

newtype Ident = Ident C.String
  deriving (C.Eq, C.Ord, C.Read, Data.String.IsString)

instance C.Show Ident where
    show (Ident s) = s
