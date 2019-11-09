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
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Reader.Class
import Data.List.Split

findTreeT ::
  Monad f =>
  ((TagTree str -> f Bool) -> TagTreePosStateT str f ())
  -> (TagTree str -> f Bool)
  -> TagTreePosStateT str f ()
findTreeT k pr =
  do  c <- liftTagTreePosState content
      z <- lift (pr c)
      unless z (liftTagTreePosState firstChild *> k pr)

findTree ::
  ((TagTree str -> Bool) -> TagTreePosState str ())
  -> (TagTree str -> Bool)
  -> TagTreePosState str ()
findTree k pr =
  findTreeT (\z -> k (runIdentity . z)) (Identity . pr)

depthFirstFindTreeT ::
  Monad f =>
  (TagTree str -> f Bool)
  -> TagTreePosStateT str f ()
depthFirstFindTreeT =
  findTreeT depthFirstFindForestT

depthFirstFindTree ::
  (TagTree str -> Bool)
  -> TagTreePosState str ()
depthFirstFindTree pr =
  depthFirstFindTreeT (Identity . pr)

depthFirstFindForestT ::
  Monad f =>
  (TagTree str -> f Bool)
  -> TagTreePosStateT str f ()
depthFirstFindForestT pr =
  do  c <- liftTagTreePosState content
      z <- lift (pr c)
      unless z (depthFirstFindTreeT pr <|> (liftTagTreePosState nextSibling *> depthFirstFindForestT pr))

depthFirstFindForest ::
  (TagTree str -> Bool)
  -> TagTreePosState str ()
depthFirstFindForest pr =
  depthFirstFindForestT (Identity . pr)

breadthFirstFindTreeT ::
  Monad f =>
  (TagTree str -> f Bool)
  -> TagTreePosStateT str f ()
breadthFirstFindTreeT =
  findTreeT breadthFirstFindForestT
  
breadthFirstFindTree ::
  (TagTree str -> Bool)
  -> TagTreePosState str ()
breadthFirstFindTree pr =
  breadthFirstFindTreeT (Identity . pr)

breadthFirstFindForestT ::
  Monad f =>
  (TagTree str -> f Bool)
  -> TagTreePosStateT str f ()
breadthFirstFindForestT pr =
  do  c <- liftTagTreePosState content
      z <- lift (pr c)
      unless z ((liftTagTreePosState nextSibling *> breadthFirstFindForestT pr) <|> breadthFirstFindTreeT pr)

breadthFirstFindForest ::
  (TagTree str -> Bool)
  -> TagTreePosState str ()
breadthFirstFindForest pr =
  breadthFirstFindForestT (Identity . pr)

debug3 :: IO (Maybe ByteString)
debug3 =
  let move :: TagTreePosState ByteString ByteString
      move =
        do  breadthFirstFindForest (isTagBranch "tbody")
            opticContent _TagBranch_
  in  runMaybeT (fmap (view (tagTreePosContent . _TagBranch_)) (casaPage >>= MaybeT . pure . execTagTreePosState move))

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
