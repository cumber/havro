{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , MultiParamTypeClasses
           , TypeFamilies
           , UndecidableInstances
  #-}

module Data.Functor.PolyVariant
  ( DivisibleApply(dApply)

  , PolyVariant(pvApply)

  , (|$|)
  , (|*|)
  )
where

import Control.Applicative ((<$>), (<*>), Applicative)
import Data.Functor.Contravariant.Divisible
  ( Divisible(divide, conquer)
  , divided
  )


result :: (b -> c) -> (a -> b) -> (a -> c)
result = fmap


class Divisible f => DivisibleApply f t z r | f t z -> r, r t -> f, r t -> z
  where dApply :: (f (a, t) -> f z) -> f a -> r


instance Divisible f => DivisibleApply f () z (f z)
  where dApply = (. flip divided conquer)


instance DivisibleApply f b z r' => DivisibleApply f (a, b) z (f (a, b) -> f z)
  where dApply = flip (result.result) divided


type family Forwards f
  where Forwards (z -> ()) = z
        Forwards (z -> (a, b)) = a -> Forwards (z -> b)


class PolyVariantMap f t z r
  where pvMap :: (Forwards (z -> (a, t)), z -> (a, t)) -> f a -> r



class PolyVariantMap f t z a => PolyVariant f t z a b r | a b r -> f, a b r -> t, a b r -> z
  where pvApply :: a -> b -> r


(|*|) :: PolyVariant f t z a b r => a -> b -> r
(|*|) = pvApply
infixl 4 |*|

(|$|) :: PolyVariantMap f t z r => (Forwards (z -> (a, t)), z -> (a, t)) -> f a -> r
(|$|) = pvMap
infixl 4 |$|
