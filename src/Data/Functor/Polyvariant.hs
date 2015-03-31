{-# LANGUAGE ConstraintKinds
           , DataKinds
           , FlexibleContexts
           , GADTs
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

  , Function
  , Arguments
  , Result
  , Tuple
  , List

  , CovariantF (runCovariantF)
  , ContravariantF (runContravariantF)

  , (<~>)((:/))

  , Polyvariant(VarianceOf)
  , VarianceMapApply
      ( VarianceConstraint
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


class   (VarianceMapApply (VarianceOf f), VarianceConstraint (VarianceOf f) f)
 => Polyvariant (f :: * -> *)
  where type family VarianceOf (f :: * -> *) :: Variance


class VarianceMapApply (v :: Variance)
  where type PolyvariantF v :: (* -> *) -> [*] -> * -> *
        type VarianceConstraint v :: (* -> *) -> Constraint

        pMap
         :: (v ~ VarianceOf f, VarianceConstraint v f)
         => (a ': as) <~> z -> f a -> PolyvariantF v f as z

        pApplyIntermediate
         :: (v ~ VarianceOf f, VarianceConstraint v f)
         =>     PolyvariantF v f (a ': b ': as) z
             -> f a
             -> PolyvariantF v f (b ': as) z

        pApplyFinal
          :: (v ~ VarianceOf f, VarianceConstraint v f)
          => PolyvariantF v f '[a] z -> f a -> f z


instance VarianceMapApply 'Covariance
  where type PolyvariantF 'Covariance = CovariantF
        type VarianceConstraint 'Covariance = Applicative

        pMap = result CovariantF . (<$>) . forwards
        pApplyIntermediate = result CovariantF . (<*>) . runCovariantF
        pApplyFinal = (<@>) . runCovariantF


instance VarianceMapApply 'Contravariance
  where type PolyvariantF 'Contravariance = ContravariantF
        type VarianceConstraint 'Contravariance = Divisible

        pMap = result ContravariantF . (/$/) . backwards
        pApplyIntermediate = result ContravariantF . (/*/) . runContravariantF
        pApplyFinal = (/@/) . runContravariantF


(|$|)
 :: Polyvariant f
 => (a ': as) <~> z -> f a -> PolyvariantF (VarianceOf f) f as z
(|$|) = pMap
infixl 4 |$|


(|*|)
 :: Polyvariant f
 =>     PolyvariantF (VarianceOf f) f (a ': b ': as) z
     -> f a
     -> PolyvariantF (VarianceOf f) f (b ': as) z
(|*|) = pApplyIntermediate
infixl 4 |*|

(|@|) :: Polyvariant f => PolyvariantF (VarianceOf f) f '[a] z -> f a -> f z
(|@|) = pApplyFinal
infixl 4 |@|
