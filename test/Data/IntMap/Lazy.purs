module Test.Data.IntMap.Lazy where

import Data.Foldable (class Foldable, foldMap, foldr, foldl, elem)
import Data.Array ((:), reverse)
import Data.Tuple (Tuple(Tuple))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Test.Data.IntMap.Internal as Internal
import Test.Unit (Test (), test)
import Test.Unit.Assert as Assert
import Test.Unit.QuickCheck (quickCheck)
import Test.QuickCheck ((===))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Prelude (
  class Show, show
, (+), (/=), ($), (#), (<$>), (<<<)
, map, bind, const, eq, pure, not)
import Data.IntMap.Lazy
import Data.IntMap as IMS
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)

err _ = unsafeThrow "ERR"
frc f = f 1

m1 = singleton 1 err

testLazy x = Assert.equal true (const true x)

testAll = test "Data.IntMap.Lazy" do
  test "Unit Tests" tests
  test "QuickCheck" props
  test "laziness" do
    test "map" $ testLazy (m1 # map frc)
    test "insert" $ testLazy (m1 # insert 1 err)
    test "t1" $ testLazy (map (_ $ 1) $ singleton 1 (\_ -> unsafeThrow "ERR"))
    test "t2" $ testLazy (map (_ $ 1) $ IMS.singleton 1 (\_ -> unsafeThrow "ERR")) -- expected to throw

tests = do
  test "insert into empty" do
    Assert.equal (singleton 1 10) (insert 1 10 empty)
  test "insert same key twice" do
    Assert.equal (singleton 1 20) (insert 1 20 $ singleton 1 10)

props = do
  test "insert into empty == singleton" $ quickCheck
    \k (v::Int) -> singleton k v === insert k v empty
  test "insert order independent" $ quickCheck
    \(a :: Array (Tuple Int Int)) ->
      fromAssocArray a === fromAssocArray (reverse a)

length :: forall a f. (Foldable f) => f a -> Int
length = foldl (\acc _ -> acc + 1) 0

newtype TIntMap = TIntMap (IntMap Int)

instance arbitraryTIntMapLazy :: Arbitrary TIntMap where
  arbitrary = (TIntMap <<< fromAssocArray) <$> arbitrary

instance showTIntMapLazy :: Show TIntMap where
  show (TIntMap t) = show t
