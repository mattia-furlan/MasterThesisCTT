{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ident where

import Data.String

newtype Ident = Ident String
  deriving (Eq, Ord, IsString)

instance Show Ident where
    show (Ident s) = s