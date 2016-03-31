module Data.IntMap.Lazy where

import Data.IntMap.Internal (Key, Prefix, Mask, branchLeft, nomatch)
import Data.Lazy (Lazy, force)
import Data.Maybe (Maybe(Nothing, Just))
--import Data.Tuple (Tuple(Tuple))
import Prelude (
  class Show, show
, class Functor, (<$>)
, pure
, (<<<), ($), otherwise, (==), (<>)
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

mapWithKey :: forall a b. (Key -> a -> b) -> IntMap a -> IntMap b
mapWithKey f t = IntMap (go <$> runIntMap t)
  where
    go Empty        = Empty
    go (Lf k a)     = Lf k (f k a)
    go (Br p m l r) = Br p m (mapWithKey f l) (mapWithKey f r)

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

instance showIntMap :: Show a => Show (IntMap a) where
  show t = "fromStrict (fromAssocArray [" <> go (step t) <> "])"
    where
      go Empty        = ""
      go (Lf k v)     = "(Tuple " <> show k <> ", " <> show v <> ")"
      go (Br _ _ l r) = go (step l) <> ", " <> go (step r)

instance functorIntMapLazy :: Functor IntMap where
  map f = mapWithKey (\_ a -> f a)
