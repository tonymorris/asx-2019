{-# LANGUAGE RankNTypes #-}

module Acronym where

import Control.Lens

data Acronym =
  Acronym {
    _name :: String -- the acronym name
  , _meaning :: String -- the meaning
  , _sources :: [String] -- the sources
  } deriving (Eq, Show)

acronymName :: Lens' Acronym String
acronymName f (Acronym n m s) = fmap (\n' -> Acronym n' m s) (f n)

acronymSource :: Lens' Acronym [String]
acronymSource f (Acronym n m s) = fmap (\s' -> Acronym n m s') (f s)

acronymSources :: Traversal' Acronym String
acronymSources = acronymSource . traverse

acronymAllTheThings :: Traversal' Acronym String
acronymAllTheThings str2fstr (Acronym n m s) =
  Acronym <$> str2fstr n <*> str2fstr m <*> traverse str2fstr s
