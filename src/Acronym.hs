{-# OPTIONS_GHC -Wall #-}

module Acronym where

import Control.Lens

data Acronym =
  Acronym 
    String -- the acronym name
    String -- the meaning
    [String] -- the sources
  deriving (Eq, Show)

acronymName :: Lens' Acronym String
acronymName f (Acronym n m s) = fmap (\n' -> Acronym n' m s) (f n)

acronymMeaning :: Lens' Acronym String
acronymMeaning f (Acronym n m s) = fmap (\m' -> Acronym n m' s) (f m)

acronymSource :: Lens' Acronym [String]
acronymSource f (Acronym n m s) = fmap (\s' -> Acronym n m s') (f s)

acronymSources :: Traversal' Acronym String
acronymSources = acronymSource . traverse

acronymAllTheThings :: Traversal' Acronym String
acronymAllTheThings str2fstr (Acronym n m s) =
  Acronym <$> str2fstr n <*> str2fstr m <*> traverse str2fstr s
