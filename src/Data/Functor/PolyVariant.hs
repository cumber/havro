{-# LANGUAGE DataKinds
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

{-
newtype CovariantF f as z
  = CovariantF { runCovariantF :: Functionify (Map f as) (f z) }

newtype ContravariantF f as z
  = ContravariantF { runContravariantF :: f (Tuplify as) -> f z }



{-
data Reversible (as :: [*]) z
  = Reversible
      { forwards :: Functionify as z
      , backwards :: z -> Tuplify as
      }
-}


data Reversible (as :: [*]) z
  where Reversible
         :: (forwards ~ Functionify as z, backwards ~ (z -> Tuplify as))
         =>   { forwards :: forwards
              , backwards :: backwards
              }
             -> Reversible as z
-}


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


class PolyVariant (as :: [*])
  where type PResult (f :: * -> *) (as :: [*]) z
        pApply :: Divisible f => ContravariantF f (a ': as) z -> f a -> PResult f as z


instance PolyVariant '[]
  where type PResult f '[] z = f z
        pApply = (. flip divided conquered) . runContravariantF

instance PolyVariant (a ': as)
  where type PResult f (a ': as) z = ContravariantF f (a ': as) z
        pApply = result ContravariantF . flip (result . result) divided . runContravariantF


(/$/) :: (Divisible f, PolyVariant as) => Reversible (a ': as) z -> f a -> ContravariantF f as z
(/$/) = result ContravariantF . divide . \case Reversible _ backwards -> backwards
infixl 4 /$/

(/*/) :: (Divisible f, PolyVariant as) => ContravariantF f (a ': as) z -> f a -> PResult f as z
(/*/) = pApply
infixl 4 /*/
