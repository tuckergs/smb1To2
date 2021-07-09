
{-# LANGUAGE EmptyCase, FlexibleContexts, FlexibleInstances, GADTs, LambdaCase, MultiParamTypeClasses, RankNTypes, TypeFamilies, TypeOperators #-}

-- funny free monad for implementing effect systems

module SuperF where

import Control.Applicative
import Control.Monad.Free.Church
import Data.Functor.Coyoneda
import GHC.Generics ((:+:)(..))

type SuperF f = F (Coyoneda f)
type Wr f r = forall x . f x -> (x -> r) -> r

data Void1 a
absurd1 :: Void1 a -> b
absurd1 = \case {}
voidWr :: Wr Void1 r
voidWr = absurd1


runSuperF :: SuperF f a -> (a -> r) -> Wr f r -> r
runSuperF (F ell) c wr = ell c (\(Coyoneda g f) -> wr f g)

superF :: (forall r . (a -> r) -> Wr f r -> r) -> SuperF f a
superF ell = F (\c wr -> ell c (\f g -> wr (Coyoneda g f)))

class Member f g where
  inject :: f a -> g a
  projectMaybe :: g a -> Maybe (f a)

instance Member f f where
  inject = id
  projectMaybe = Just

instance {-# OVERLAPPING #-} Member f (f :+: ell) where
  inject = L1
  projectMaybe (R1 _) = Nothing
  projectMaybe (L1 f) = Just f

instance {-# OVERLAPPABLE #-} Member h g => Member h (f :+: g) where
  inject h = R1 (inject h)
  projectMaybe (L1 _) = Nothing
  projectMaybe (R1 g) = projectMaybe g

instance {-# OVERLAPPING #-} (Member f ell, Member g ell) => Member (f :+: g) ell where
  inject (L1 f) = inject f
  inject (R1 g) = inject g
  projectMaybe ell = (L1 <$> projectMaybe ell) <|> (R1 <$> projectMaybe ell)

class Subset ell1 ell2 where
  injectMany :: ell1 a -> ell2 a

instance Subset Void1 ell where
  injectMany = absurd1

instance (Member f ell, Subset g ell) => Subset (f :+: g) ell where
  injectMany (L1 f) = inject f
  injectMany (R1 g) = injectMany g

send :: Member f ell => f a -> SuperF ell a
send f = superF $ \c wr -> wr (inject f) c

liftSF :: Subset f ell => SuperF f a -> SuperF ell a
liftSF sf = superF $ \c wr -> runSuperF sf c (\f g -> wr (injectMany f) g)

modWr :: (forall r . Wr ell2 r -> Wr ell1 r) -> SuperF ell1 a -> SuperF ell2 a
modWr nt sf = superF $ \c wr -> runSuperF sf c (nt wr)

interpose :: Member f ell => (forall r . Wr f r -> Wr f r) -> SuperF ell a -> SuperF ell a
interpose nt = modWr $ \wr ell g -> case projectMaybe ell of
  Nothing -> wr ell g
  Just f -> nt (\h -> wr (inject h)) f g

combineWr :: Wr f r -> Wr g r -> Wr (f :+: g) r
combineWr wrf wrg (L1 f) h = wrf f h
combineWr wrf wrg (R1 g) h = wrg g h
