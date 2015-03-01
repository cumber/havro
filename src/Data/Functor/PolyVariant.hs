{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , MultiParamTypeClasses
           , TypeFamilies
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
