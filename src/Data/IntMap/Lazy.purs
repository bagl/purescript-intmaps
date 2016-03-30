module Data.IntMap.Lazy where

import Data.IntMap.Internal (Key, Prefix, Mask, branchLeft, nomatch)
import Data.Lazy (Lazy, defer, force)
import Data.Maybe (Maybe(Nothing, Just))
--import Data.Tuple (Tuple(Tuple))
import Prelude (
  class Show, show
, const, (<<<), ($), otherwise, (==), (<>)
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
unstep = IntMap <<< defer <<< const

lf :: forall a. Key -> a -> IntMap a
lf k a = unstep $ Lf k a

br :: forall a. Prefix -> Mask -> IntMap a -> IntMap a -> IntMap a
br p m t1 t2 =
  case [step t1, step t2] of
    [Empty, Empty] -> empty
    [Empty, _    ] -> t2
    [_    , Empty] -> t1
    _              -> unstep $ Br p m t1 t2

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
mapWithKey f m = go (step m)
  where
    go Empty        = empty
    go (Lf k a)     = lf k (f k a)
    go (Br p m l r) = br p m (go $ step l) (go $ step r)

foldrWithKey :: forall a b. (Key -> a -> b -> b) -> b -> IntMap a -> b
foldrWithKey f z m = go z (step m)
  where
    go z Empty = z
    go z (Lf k a) = f k a z
    go z (Br _ _ l r) = go (go z $ step r) (step l)

filter :: forall a. (a -> Boolean) -> IntMap a -> IntMap a
filter p = filterWithKey (\_ x -> p x)

filterWithKey :: forall a. (Key -> a -> Boolean) -> IntMap a -> IntMap a
filterWithKey pred t =
  case step t of
    Br p m l r -> br p m (filterWithKey pred l) (filterWithKey pred r)
    Lf i a
      | pred i a  ->  t
    otherwise  -> empty

null :: forall a. IntMap a -> Boolean
null m = case step m of
  Empty     -> true
  otherwise -> false

instance showIntMap :: Show a => Show (IntMap a) where
  show m = "fromStrict (fromAssocArray [" <> go (step m) <> "])"
    where
      go Empty        = ""
      go (Lf k v)     = "(Tuple " <> show k <> ", " <> show v <> ")"
      go (Br _ _ l r) = go (step l) <> ", " <> go (step r)
