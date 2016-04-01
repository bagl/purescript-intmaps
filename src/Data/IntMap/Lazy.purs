module Data.IntMap.Lazy where

import Data.Foldable (class Foldable, foldl, foldr)
import Data.IntMap.Internal (
  Key, Prefix, runPrefix, Mask(Mask), mask
, branchLeft, matchPrefix, nomatch, branchingBit')
import Data.Lazy (Lazy, force)
import Data.Maybe (Maybe(Nothing, Just), maybe, fromMaybe)
import Data.Monoid (class Monoid, mempty)
import Data.Tuple (Tuple(Tuple))
import Prelude (
  class Show, show
, class Eq, eq
, class Semigroup, append
, class Functor, (<$>)
, pure
, const, id
, (<<<), ($), otherwise, (==), (<>), (&&), (+)
)

newtype IntMap a = IntMap (Lazy (Step a))

data Step a
  = Empty
  | Lf Key a
  | Br Prefix Mask (IntMap a) (IntMap a)

runIntMap :: forall a. IntMap a -> Lazy (Step a)
runIntMap (IntMap m) = m

step :: forall a. IntMap a -> Step a
step = force <<< runIntMap

unstep :: forall a. Step a -> IntMap a
unstep = IntMap <<< pure

lf :: forall a. Key -> a -> IntMap a
lf k a = unstep $ Lf k a

br :: forall a. Prefix -> Mask -> IntMap a -> IntMap a -> IntMap a
br p m t1 t2 = go (step t1) (step t2)
  where
    go Empty Empty = empty
    go Empty _     = t2
    go _     Empty = t1
    go _     _     = unstep $ Br p m t1 t2

empty :: forall a. IntMap a
empty = unstep Empty

singleton :: forall a. Key -> a -> IntMap a
singleton = lf

member :: forall a. Key -> IntMap a -> Boolean
member k m = maybe false (const true) (lookup k m)

lookup :: forall a. Key -> IntMap a -> Maybe a
lookup i t = go (step t)
  where
    go Empty = Nothing
    go (Lf k v)
      | k == i    = Just v
      | otherwise = Nothing
    go (Br p m l r)
      | nomatch p m i  = Nothing
      | branchLeft m i = lookup i l
      | otherwise      = lookup i r

lookupDefault :: forall a. Key -> a -> IntMap a -> a
lookupDefault k d m = fromMaybe d (lookup k m)

insert :: forall a. Key -> a -> IntMap a -> IntMap a
insert = insertWithKey (\_ _ a -> a)

insertWith :: forall a. (a -> a -> a) -> Key -> a -> IntMap a -> IntMap a
insertWith f = insertWithKey (\_ -> f)

insertWithKey :: forall a. (Key -> a -> a -> a)
              -> Key -> a -> IntMap a -> IntMap a
insertWithKey f k a t = go (step t)
  where
    go Empty = lf k a
    go (Lf k0 a0)
      | k0 == k   = lf k0 (f k a0 a)
      | otherwise = join k (Mask 0) (lf k a) k0 (Mask 0) t
    go (Br p m l r)
      | matchPrefix p m k = if branchLeft m k
                            then br p m (go $ step l) r
                            else br p m l (go $ step r)
      | otherwise         = join k (Mask 0) (lf k a) (runPrefix p) m t


mapWithKey :: forall a b. (Key -> a -> b) -> IntMap a -> IntMap b
mapWithKey f t = IntMap (go <$> runIntMap t)
  where
    go Empty        = Empty
    go (Lf k a)     = Lf k (f k a)
    go (Br p m l r) = Br p m (mapWithKey f l) (mapWithKey f r)

fromAssocArray :: forall a. Array (Tuple Key a) -> IntMap a
fromAssocArray = foldr (\(Tuple k v) m -> insert k v m) empty

foldMapWithKey :: forall a m. (Monoid m) => (Key -> a -> m) -> IntMap a -> m
foldMapWithKey f = go <<< step
  where
    go Empty        = mempty
    go (Lf k x)     = f k x
    go (Br _ _ l r) = go (step l) <> go (step r)

foldlWithKey :: forall a b. (Key -> b -> a -> b) -> b -> IntMap a -> b
foldlWithKey f z' = go z' <<< step
  where
    go z Empty        = z
    go z (Lf k a)     = f k z a
    go z (Br _ _ l r) = go (go z $ step l) (step r)

foldrWithKey :: forall a b. (Key -> a -> b -> b) -> b -> IntMap a -> b
foldrWithKey f z t = go z (step t)
  where
    go z Empty        = z
    go z (Lf k a)     = f k a z
    go z (Br _ _ l r) = go (go z $ step r) (step l)

filter :: forall a. (a -> Boolean) -> IntMap a -> IntMap a
filter p = filterWithKey (\_ x -> p x)

filterWithKey :: forall a. (Key -> a -> Boolean) -> IntMap a -> IntMap a
filterWithKey p t =
  case step t of
    Br pref m l r  -> br pref m (filterWithKey p l) (filterWithKey p r)
    Lf i a | p i a -> t
    _              -> empty

null :: forall a. IntMap a -> Boolean
null t = case step t of
  Empty     -> true
  otherwise -> false

size :: forall a. IntMap a -> Int
size = foldl (\c _ -> 1 + c) 0

instance showIntMap :: Show a => Show (IntMap a) where
  show t = "fromStrict (fromAssocArray [" <> go (step t) <> "])"
    where
      go Empty        = ""
      go (Lf k v)     = "(Tuple " <> show k <> ", " <> show v <> ")"
      go (Br _ _ l r) = go (step l) <> ", " <> go (step r)

instance functorIntMapLazy :: Functor IntMap where
  map f = mapWithKey (\_ a -> f a)

instance eqIntMapLazy :: (Eq a) => Eq (IntMap a) where
  eq t1 t2 = go (step t1) (step t2)
    where
      go Empty Empty = true
      go (Lf k1 v1) (Lf k2 v2) = eq k1 k2 && eq v1 v2
      go (Br p1 m1 l1 r1) (Br p2 m2 l2 r2) =
        eq p1 p2 && eq m1 m2 && eq l1 l2 && eq r1 r2
      go _ _ = false

instance semigroupIntMapLazy :: (Semigroup a) => Semigroup (IntMap a) where
  append m1 m2 = unionWith append m1 m2
    where unionWith = id -- TODO !!!

instance monoidIntMapLazy :: (Semigroup a) => Monoid (IntMap a) where
  mempty = empty

instance foldableIntMapLazy :: Foldable IntMap where
  foldMap = foldMapWithKey <<< const
  foldr   = foldrWithKey <<< const
  foldl   = foldlWithKey <<< const

-------------------------------------
-- HELPERS --------------------------
-------------------------------------

join :: forall a. Key -> Mask -> IntMap a
     -> Key -> Mask -> IntMap a -> IntMap a
join k1 m1 t1 k2 m2 t2 =
  if branchLeft m k1
  then br (mask m k1) m t1 t2
  else br (mask m k1) m t2 t1
  where
    m = branchingBit' k1 m1 k2 m2
