
{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, UndecidableInstances #-}

module Vector where

import Control.Applicative
import Control.Monad

import HexStuff
import Nat


data Vector n a where
  Nil :: Vector Zero a
  Cons :: a -> Vector n a -> Vector (Succ n) a

instance Show (Vector Zero Char) where
  show _ = []

instance Show (Vector n Char) => Show (Vector (Succ n) Char) where
  show (Cons a as) = 
    let
      pad l a = let unpadStr = show (Hex a) in take (l - length unpadStr) (repeat '0') ++ unpadStr
    in pad 2 (fromEnum a) ++ " " ++ show as

instance Functor (Vector Zero) where
  fmap _ _ = Nil

instance Functor (Vector n) => Functor (Vector (Succ n)) where
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative (Vector Zero) where
  pure _ = Nil
  _ <*> _ = Nil

instance Applicative (Vector n) => Applicative (Vector (Succ n)) where
  pure a = Cons a (pure a)
  (Cons f fs) <*> (Cons a as) = Cons (f a) (fs <*> as)

instance Foldable (Vector Zero) where
  foldMap _ _ = mempty

instance Foldable (Vector n) => Foldable (Vector (Succ n)) where
  foldMap f (Cons a as) = mappend (f a) (foldMap f as)

instance Traversable (Vector Zero) where
  sequenceA _ = pure Nil

instance Traversable (Vector n) => Traversable (Vector (Succ n)) where
  sequenceA (Cons m ms) = liftA2 Cons m (sequenceA ms)


class (Applicative (Vector n), Traversable (Vector n)) => FixedReplicateA n where
  fixedReplicateA :: Applicative m => m a -> m (Vector n a)

instance (Applicative (Vector n), Traversable (Vector n)) => FixedReplicateA n where
  fixedReplicateA m = sequenceA $ pure m
  
class ListToVector n where
  listToVector :: [a] -> Vector n a

instance ListToVector Zero where
  listToVector _ = Nil

instance ListToVector n => ListToVector (Succ n) where
  listToVector (a:as) = Cons a (listToVector as)
