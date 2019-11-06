{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Acronyms where

import Acronym
import Control.Lens

newtype Acronyms =
  Acronyms
    [Acronym]
  deriving (Eq, Show)

instance Acronyms ~ a =>
  Rewrapped Acronyms a

instance Wrapped Acronyms where
  type Unwrapped Acronyms =
    [Acronym]
  _Wrapped' =
    iso (\(Acronyms x) -> x) Acronyms

acronyms :: Traversal' Acronyms Acronym
acronyms = _Wrapped . traverse

acronymsName :: Traversal' Acronyms String
acronymsName = acronyms . acronymName

acronymsMeaning :: Traversal' Acronyms String
acronymsMeaning  = acronyms . acronymMeaning

acronymsSources :: Traversal' Acronyms String
acronymsSources = acronyms . acronymSources

