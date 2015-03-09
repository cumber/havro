{-# LANGUAGE ConstraintKinds
           , DataKinds
           , FlexibleContexts
           , TypeFamilies
           , TypeOperators
           , UndecidableInstances
  #-}

module Data.Functor.Polyvariant.Polyvariadic
  ( DivisibleApply(DResult, dApply)
  , (|*|)
  , PolyvariantApply(PResult, pApply)
  , (/*/)
  )
where

import Data.Functor.Polyvariant
  ( dApplyIntermediate
  , dApplyFinal
  , Polyvariant(PolyvariantConstraint, PolyvariantF, pApplyIntermediate, pApplyFinal)
  , VarianceOf
  )

import Data.Functor.Contravariant.Divisible
  ( Divisible
  )


class DivisibleApply as
  where type DResult (f :: * -> *) as z
        dApply :: Divisible f => (f (a, as) -> f z) -> f a -> DResult f as z

instance DivisibleApply ()
  where type DResult f () z = f z
        dApply = dApplyFinal

instance DivisibleApply (a, as)
  where type DResult f (a, as) z = f (a, as) -> f z
        dApply = dApplyIntermediate


(/*/)
 :: (Divisible f, DivisibleApply as)
 => (f (a, as) -> f z) -> f a -> DResult f as z
(/*/) = dApply
infixl 4 /*/


class PolyvariantApply (as :: [*])
  where type PResult (f :: * -> *) as z
        pApply :: (Polyvariant (VarianceOf f), PolyvariantConstraint (VarianceOf f) f) => PolyvariantF (VarianceOf f) f (a ': as) z -> f a -> PResult f as z

instance PolyvariantApply '[]
  where type PResult f '[] z = f z
        pApply = pApplyFinal

instance PolyvariantApply (a ': as)
  where type PResult f (a ': as) z = PolyvariantF (VarianceOf f) f (a ': as) z
        pApply = pApplyIntermediate


(|*|)
 ::   ( Polyvariant (VarianceOf f)
      , PolyvariantConstraint (VarianceOf f) f
      , PolyvariantApply as
      )
 => PolyvariantF (VarianceOf f) f (a ': as) z -> f a -> PResult f as z
(|*|) = pApply
infixl 4 |*|
