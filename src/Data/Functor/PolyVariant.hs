{-# LANGUAGE ConstraintKinds
           , DataKinds
           , FlexibleContexts
           , FlexibleInstances
           , GADTs
           , FunctionalDependencies
           , LambdaCase
           , MultiParamTypeClasses
           , TypeFamilies
           , TypeOperators
           , UndecidableInstances
  #-}

module Data.Functor.PolyVariant
  ( DivisibleApply(dApply)

  , (|$|)
  , (|*|)
  )
where

import Control.Applicative ((<$>), (<*>), Applicative)
import Data.Functor.Contravariant.Divisible
  ( Divisible(divide)
  , conquered
  , divided
  )

import GHC.Exts (Constraint)


result :: (b -> c) -> (a -> b) -> (a -> c)
result = fmap


class DivisibleApply as
  where type DResult (f :: * -> *) as z
        dApply :: Divisible f => (f (a, as) -> f z) -> f a -> DResult f as z

instance DivisibleApply ()
  where type DResult f () z = f z
        dApply = (. flip divided conquered)

instance DivisibleApply (a, as)
  where type DResult f (a, as) z = f (a, as) -> f z
        dApply = flip (result . result) divided


(|$|) :: (Divisible f, DivisibleApply as) => (z -> (a, as)) -> f a -> f as -> f z
(|$|) = divide
infixl 4 |$|

(|*|) :: (Divisible f, DivisibleApply as) => (f (a, as) -> f z) -> f a -> DResult f as z
(|*|) = dApply
infixl 4 |*|


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
         =>   { forwards :: f
              , backwards :: z -> t
              }
             -> Reversible as z


newtype ContravariantF f as z = ContravariantF { runContravariantF :: f (Tuplify as) -> f z }

newtype CovariantF f as z = CovariantF { runCovariantF :: f (Functionify as z) }


data Variance = Covariance | Contravariance
  deriving (Eq, Ord, Show)

type family VarianceOf (f :: * -> *) :: Variance


class Polyvariant (v :: Variance)
  where type PolyvariantF v :: (* -> *) -> [*] -> * -> *
        type PolyvariantConstraint v :: (* -> *) -> Constraint

        pMap :: (v ~ VarianceOf f, PolyvariantConstraint v f) => Reversible (a ': as) z -> f a -> PolyvariantF v f as z
        pApplyIntermediate :: (v ~ VarianceOf f, PolyvariantConstraint v f) => PolyvariantF v f (a ': b ': as) z -> f a -> PolyvariantF v f (b ': as) z
        pApplyFinal :: (v ~ VarianceOf f, PolyvariantConstraint v f) => PolyvariantF v f '[a] z -> f a -> f z


instance Polyvariant Covariance
  where type PolyvariantF Covariance = CovariantF
        type PolyvariantConstraint Covariance = Applicative

        pMap (Reversible f _) = CovariantF . (f <$>)
        pApplyIntermediate = result CovariantF . (<*>) . runCovariantF
        pApplyFinal = (<*>) . runCovariantF

instance Polyvariant Contravariance
  where type PolyvariantF Contravariance = ContravariantF
        type PolyvariantConstraint Contravariance = Divisible

        pMap (Reversible _ b) = ContravariantF . divide b
        pApplyIntermediate = result ContravariantF . (|*|) . runContravariantF
        pApplyFinal = (|*|) . runContravariantF


class PolyvariantApply (as :: [*])
  where type PResult (f :: * -> *) as z
        pApply :: (Polyvariant (VarianceOf f), PolyvariantConstraint (VarianceOf f) f) => PolyvariantF (VarianceOf f) f (a ': as) z -> f a -> PResult f as z

instance PolyvariantApply '[]
  where type PResult f '[] z = f z
        pApply = pApplyFinal

instance PolyvariantApply (a ': as)
  where type PResult f (a ': as) z = PolyvariantF (VarianceOf f) f (a ': as) z
        pApply = pApplyIntermediate


(/$/) :: (Polyvariant (VarianceOf f), PolyvariantConstraint (VarianceOf f) f) => Reversible (a ': as) z -> f a -> PolyvariantF (VarianceOf f) f as z
(/$/) = pMap
infixl 4 /$/


(/*/) :: (Polyvariant (VarianceOf f), PolyvariantConstraint (VarianceOf f) f, PolyvariantApply as) => PolyvariantF (VarianceOf f) f (a ': as) z -> f a -> PResult f as z
(/*/) = pApply
infixl 4 /*/
