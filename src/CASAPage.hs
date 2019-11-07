{-# LANGUAGE OverloadedStrings #-}

module CASAPage where

import Control.Lens
import Control.Monad
import Data.ByteString.Lazy
import Network.Wreq
import Text.HTML.TagSoup.Navigate
import Control.Monad.Trans.Maybe

casaPage :: MaybeT IO (TagTreePos ByteString)
casaPage =
  MaybeT (parseTreePos . view responseBody <$> get "https://www.casa.gov.au/about-us/standard-page/aviation-abbreviations-and-acronyms-full-list")

isTagBranch s (TagBranch t _ _) = s == t
isTagBranch _ _ = False

tbody =
  let findThenFirstChild s =
        findContentUntil nextSibling (isTagBranch s) *>
        firstChild
  in  do  findThenFirstChild "html"
          findThenFirstChild "body"
          findContentUntil nextSibling (isTagBranch "div")
          replicateM 4 (findThenFirstChild "div")
          findContentUntil nextSibling (isTagBranch "div")
          nextSibling
          findThenFirstChild "div"
          findThenFirstChild "table"
          findContentUntil nextSibling (isTagBranch "tbody")
          firstChild
          findContentUntil nextSibling (isTagBranch "tr")

breadthFirst p = findContentUntil undefined
depthFirst p = findContentUntil undefined

parsing =
  do  tbody
      firstChild
      findContentUntil nextSibling (isTagBranch "td")
      firstChild
      c <- content
      c' <- maybeTagTreePosState (c ^? _TagLeaf . _TagText)
      pure c'

      -- z <- casaPage

      

debug =
  fmap (over _1 (view tagTreePosContent)) . runTagTreePosState parsing <$> casaPage
