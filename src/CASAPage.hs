{-# LANGUAGE OverloadedStrings #-}

module CASAPage where

import Acronym
import Acronyms
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Bool
import Data.Char
import Data.ByteString.Lazy(ByteString)
import Data.ByteString.Lazy.Char8 as Char8(unpack)
import Network.Wreq
import Text.HTML.TagSoup.Navigate
import Control.Monad.Trans.Maybe
import Control.Monad.Reader.Class
import Data.List.Split

casaPage :: MaybeT IO (TagTreePos ByteString)
casaPage =
  MaybeT (parseTreePos . view responseBody <$> get "https://www.casa.gov.au/about-us/standard-page/aviation-abbreviations-and-acronyms-full-list")

isTagBranch :: Eq a => a -> TagTree a -> Bool
isTagBranch s (TagBranch t _ _) = s == t
isTagBranch _ _ = False

tbody :: TagTreePosState ByteString ()
tbody =
  let findThenFirstChild s =
        findContentUntil nextSibling (isTagBranch s) *>
        firstChild
  in  do  findThenFirstChild "html"
          findThenFirstChild "body"
          findContentUntil nextSibling (isTagBranch "div")
          replicateM_ 4 (findThenFirstChild "div")
          findContentUntil nextSibling (isTagBranch "div")
          nextSibling
          findThenFirstChild "div"
          findThenFirstChild "table"
          findContentUntil nextSibling (isTagBranch "tbody")
          firstChild
          findContentUntil nextSibling (isTagBranch "tr")

breadthFirst ::
  (HasTagTreePos s str, MonadReader s m) =>
  (TagTree str -> Bool)
  -> b
  -> m b
breadthFirst p s = 
  reader (p . view tagTreePosContent) >>= bool undefined (return s)

depthFirst ::
  Monad f =>
  (TagTree str -> Bool)
  -> TagTreePosStateT str f ()
depthFirst p = findContentUntil undefined p

trim :: ByteString -> String
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

parseSources :: String -> [String]
parseSources s =
  splitOn "/" s

parsing ::
  TagTreePosState ByteString (Acronym, Acronym)
parsing =
  do  tbody
      a <- parseAcronym
      b <- parseAcronym
      return (a, b)

parseAcronyms :: TagTreePosState ByteString Acronyms
parseAcronyms =
  Acronyms <$> some parseAcronym  
      
parseAcronym :: TagTreePosState ByteString Acronym
parseAcronym =
  let txt = opticContent (_TagLeaf . _TagText) 
  in  do  firstChild
          findContentUntil nextSibling (isTagBranch "td")
          firstChild
          n <- txt
          parent
          nextSibling
          nextSibling
          firstChild
          m <- txt
          parent
          nextSibling
          nextSibling
          firstChild
          s <- txt
          parent
          parent
          nextSibling
          nextSibling
          pure (Acronym (trim n) (trim m) (parseSources (trim s)))

debug2 :: MaybeT IO (Maybe (TagTree ByteString, a))
debug2 =
  fmap (over _1 (view tagTreePosContent)) . runTagTreePosState (breadthFirst (isTagBranch "tbody") undefined) <$> casaPage

debug :: MaybeT IO (Maybe (TagTree ByteString, (Acronym, Acronym)))
debug =
  fmap (over _1 (view tagTreePosContent)) . runTagTreePosState parsing <$> casaPage
