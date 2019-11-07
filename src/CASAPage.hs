{-# LANGUAGE OverloadedStrings #-}

module CASAPage where

import Acronym
import Acronyms
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Bool
import Data.Char
import Data.ByteString.Lazy(ByteString, unpack)
import Data.ByteString.Lazy.Char8 as Char8(unpack)
import Network.Wreq
import Text.HTML.TagSoup.Navigate
import Control.Monad.Trans.Maybe
import Control.Monad.Reader.Class
import Data.List.Split

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

breadthFirst p s = 
  reader (p . view tagTreePosContent) >>= bool undefined (return s)

depthFirst p = findContentUntil undefined p

trim =
  let k = dropWhile isSpace . reverse
  in  k . k . Char8.unpack

{-
data Source =
  ICAO
  | AIP
  | Other String
  deriving (Eq, Ord, Show)

parseSource "ICAO" = ICAO
parseSource "AIP" = AIP
parseSource s = Other s
-}

parseSources s =
  splitOn "/" s

parsing =
  do  tbody
      a <- parseAcronym
      b <- parseAcronym
      return (a, b)

parseAcronyms =
  Acronyms <$> many parseAcronym  
      
parseAcronym =
  do  firstChild
      findContentUntil nextSibling (isTagBranch "td")
      firstChild
      n <- opticContent (_TagLeaf . _TagText)
      parent
      nextSibling
      nextSibling
      firstChild
      m <- opticContent (_TagLeaf . _TagText)
      parent
      nextSibling
      nextSibling
      firstChild
      s <- opticContent (_TagLeaf . _TagText)
      parent
      parent
      nextSibling
      nextSibling
      pure (Acronym (trim n) (trim m) (parseSources (trim s)))

debug2 =
  fmap (over _1 (view tagTreePosContent)) . runTagTreePosState (breadthFirst (isTagBranch "tbody") undefined) <$> casaPage

debug =
  fmap (over _1 (view tagTreePosContent)) . runTagTreePosState parsing <$> casaPage
