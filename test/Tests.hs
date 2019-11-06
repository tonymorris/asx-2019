import           Control.Lens              
import           Hedgehog                  (forAll, property, (===))
import qualified Hedgehog.Gen              as Gen
import           Hedgehog.Internal.Gen     (MonadGen)
import qualified Hedgehog.Range            as Range
import           Test.Tasty                (TestTree, defaultMain, testGroup)
import           Test.Tasty.Hedgehog       (testProperty)
import Acronym

main :: IO ()
main = defaultMain test_Acronym

test_Acronym :: TestTree
test_Acronym =
  testGroup "Acronym" [
    acronymNameTest
  ]

genString :: MonadGen m => m String
genString = Gen.list (Range.linearFrom 20 (-10) 10) Gen.unicodeAll

genAcronym :: MonadGen m => m Acronym
genAcronym =
  let r = Range.linearFrom 20 (-10) 10
  in  do  n <- genString
          m <- genString
          s <- Gen.list r genString
          pure (Acronym n m s)

acronymNameTest :: TestTree
acronymNameTest =
  testGroup "Acronym acronymName" [
    testProperty "set/view" . property $
      forAll genAcronym >>= \a ->
      set acronymName (view acronymName a) a === a
  , testProperty "view/set" . property $
      forAll genAcronym >>= \a ->
      forAll genString >>= \n ->
      view acronymName (set acronymName n a) === n
  , testProperty "set/set" . property $
      forAll genAcronym >>= \a ->
      forAll genString >>= \n ->
      forAll genString >>= \n' ->
      set acronymName n' (set acronymName n a) === set acronymName n' a
  ]
