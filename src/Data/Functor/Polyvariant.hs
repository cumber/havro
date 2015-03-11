{-# LANGUAGE ConstraintKinds
           , DataKinds
           , FlexibleContexts
           , GADTs
           , TypeFamilies
           , TypeOperators
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

  , Function
  , Arguments
  , Result
  , Tuple
  , List

  , CovariantF (runCovariantF)
  , ContravariantF (runContravariantF)

  , (<~>)((:/))

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


type family Function (as :: [*]) z
  where Function '[] z = z
        Function (a ': as) z = a -> Function as z

type family Arguments f z :: [*]
  where Arguments z z = '[]
        Arguments (a -> b) z = a ': Arguments b z

type family Result f (as :: [*])
  where Result z '[] = z
        Result (a -> b) (a ': as) = Result b as

type family Tuple (as :: [*])
  where Tuple '[] = ()
        Tuple (a ': as) = (a, Tuple as)

type family List t :: [*]
  where List () = '[]
        List (a, t) = a ': List t

data (as :: [*]) <~> z
  where (:/)
         ::   ( f ~ Function as z
              , as ~ Arguments f z
              , z ~ Result f as
              , t ~ Tuple as
              , as ~ List t
              )
         => f -> (z -> t) -> as <~> z

forwards :: as <~> z -> Function as z
forwards (f :/ _) = f

backwards :: as <~> z -> (z -> Tuple as)
backwards (_ :/ b) = b


newtype ContravariantF f (as :: [*]) z
  = ContravariantF { runContravariantF :: f (Tuple as) -> f z }

newtype CovariantF f (as :: [*]) z
  = CovariantF { runCovariantF :: f (Function as z) }


data Variance = Covariance | Contravariance
  deriving (Eq, Ord, Show)

type family VarianceOf (f :: * -> *) :: Variance


class Polyvariant (v :: Variance)
  where type PolyvariantF v :: (* -> *) -> [*] -> * -> *
        type PolyvariantConstraint v :: (* -> *) -> Constraint

        pMap
         :: (v ~ VarianceOf f, PolyvariantConstraint v f)
         => (a ': as) <~> z -> f a -> PolyvariantF v f as z

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
 => (a ': as) <~> z -> f a -> PolyvariantF (VarianceOf f) f as z
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
