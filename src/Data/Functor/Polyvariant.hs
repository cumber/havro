{-# LANGUAGE ConstraintKinds
           , DataKinds
           , FlexibleContexts
           , FlexibleInstances
           , GADTs
           , FunctionalDependencies
           , MultiParamTypeClasses
           , TypeFamilies
           , TypeOperators
           , UndecidableInstances
  #-}

module Data.Functor.Polyvariant
  ( dMap
  , dApplyIntermediate
  , dApplyFinal
  , (/$/)
  , (/*/)
  , (/@/)

  , (<@>)

  , Variance(Covariance, Contravariance)
  , VarianceOf

  , Tuple
  , Tuplify
  , Function
  , Functionify

  , CovariantF (runCovariantF)
  , ContravariantF (runContravariantF)

  , Reversible(Reversible)

  , Polyvariant
      ( PolyvariantConstraint
      , PolyvariantF
      , pMap
      , pApplyIntermediate
      , pApplyFinal
      )

  , (|$|)
  , (|*|)
  , (|@|)
  )
where

import Control.Applicative (Applicative((<*>)))

import Data.Functor.Contravariant.Divisible
  ( Divisible(divide)
  , conquered
  , divided
  )

import GHC.Exts (Constraint)


result :: (b -> c) -> (a -> b) -> (a -> c)
result = fmap

argument :: (a -> b) -> (b -> c) -> (a -> c)
argument = flip fmap


dMap :: Divisible f => (z -> (a, as)) -> f a -> (f as -> f z)
dMap = divide

dApplyIntermediate
 :: Divisible f
 => (f (a, (b, as)) -> f z) -> f a -> (f (b, as) -> f z)
dApplyIntermediate = flip (result . result) divided


dApplyFinal :: Divisible f => (f (a, ()) -> f z) -> f a -> f z
dApplyFinal = argument $ flip divided conquered


(/$/) :: Divisible f => (z -> (a, as)) -> f a -> (f as -> f z)
(/$/) = dMap
infixl 4 /$/

(/*/) :: Divisible f => (f (a, (b, as)) -> f z) -> f a -> (f (b, as) -> f z)
(/*/) = dApplyIntermediate
infixl 4 /*/


(/@/) :: Divisible f => (f (a, ()) -> f z) -> f a -> f z
(/@/) = dApplyFinal
infixl 4 /@/


(<@>) :: Applicative f => f (a -> z) -> f a -> f z
(<@>) = (<*>)
infixl 4 <@>


class Function f (as :: [*]) z | f as -> z, as z -> f

instance Function z '[] z
instance Function b as z => Function (a -> b) (a ': as) z


class Tuple t (as :: [*]) | t -> as, as -> t

instance Tuple () '[]
instance Tuple t as => Tuple (a, t) (a ': as)

type family Tuplify (as :: [*])

type instance Tuplify '[] = ()
type instance Tuplify (a ': as) = (a, Tuplify as)

type family Map f (as :: [*])
  where Map f '[] = '[]
        Map f (a ': as) = (f a ': Map f as)

type family Functionify (as :: [*]) z

type instance Functionify '[] z = z
type instance Functionify (a ': as) z = a -> Functionify as z


data Reversible (as :: [*]) z
  where Reversible
         :: (Function f as z, Tuple t as, Functionify as z ~ f, Tuplify as ~ t)
         => f -> (z -> t) -> Reversible as z

forwards :: Reversible as z -> Functionify as z
forwards (Reversible f _) = f

backwards :: Reversible as z -> (z -> Tuplify as)
backwards (Reversible _ b) = b


newtype ContravariantF f as z = ContravariantF { runContravariantF :: f (Tuplify as) -> f z }

newtype CovariantF f as z = CovariantF { runCovariantF :: f (Functionify as z) }


data Variance = Covariance | Contravariance
  deriving (Eq, Ord, Show)

type family VarianceOf (f :: * -> *) :: Variance


class Polyvariant (v :: Variance)
  where type PolyvariantF v :: (* -> *) -> [*] -> * -> *
        type PolyvariantConstraint v :: (* -> *) -> Constraint

        pMap
         :: (v ~ VarianceOf f, PolyvariantConstraint v f)
         => Reversible (a ': as) z -> f a -> PolyvariantF v f as z

        pApplyIntermediate
         :: (v ~ VarianceOf f, PolyvariantConstraint v f)
         =>     PolyvariantF v f (a ': b ': as) z
             -> f a
             -> PolyvariantF v f (b ': as) z

        pApplyFinal
          :: (v ~ VarianceOf f, PolyvariantConstraint v f)
          => PolyvariantF v f '[a] z -> f a -> f z


instance Polyvariant Covariance
  where type PolyvariantF Covariance = CovariantF
        type PolyvariantConstraint Covariance = Applicative

        pMap = result CovariantF . fmap . forwards
        pApplyIntermediate = result CovariantF . (<*>) . runCovariantF
        pApplyFinal = (<@>) . runCovariantF


instance Polyvariant Contravariance
  where type PolyvariantF Contravariance = ContravariantF
        type PolyvariantConstraint Contravariance = Divisible

        pMap = result ContravariantF . divide . backwards
        pApplyIntermediate = result ContravariantF . (/*/) . runContravariantF
        pApplyFinal = (/@/) . runContravariantF


(|$|)
 :: (Polyvariant (VarianceOf f), PolyvariantConstraint (VarianceOf f) f)
 => Reversible (a ': as) z -> f a -> PolyvariantF (VarianceOf f) f as z
(|$|) = pMap
infixl 4 |$|


(|*|)
 :: (Polyvariant (VarianceOf f), PolyvariantConstraint (VarianceOf f) f)
 =>     PolyvariantF (VarianceOf f) f (a ': b ': as) z
     -> f a
     -> PolyvariantF (VarianceOf f) f (b ': as) z
(|*|) = pApplyIntermediate
infixl 4 |*|

(|@|)
 :: (Polyvariant (VarianceOf f), PolyvariantConstraint (VarianceOf f) f)
 => PolyvariantF (VarianceOf f) f '[a] z -> f a -> f z
(|@|) = pApplyFinal
infixl 4 |@|
