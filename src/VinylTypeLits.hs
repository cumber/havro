{-# LANGUAGE DataKinds
           , ExplicitNamespaces
           , TypeFamilies
           , TypeOperators
           , UndecidableInstances
  #-}

module VinylTypeLits
  ( NatToLit
  , LitToNat
  )
where

import Data.Vinyl.TypeLevel as V (Nat(Z, S))
import GHC.TypeLits as G (Nat, type (+), type (-))


type family NatToLit (n :: V.Nat) :: G.Nat
  where NatToLit 'Z = 0
        NatToLit ('S n) = 1 + NatToLit n


type family LitToNat (n :: G.Nat) :: V.Nat
  where LitToNat 0 = 'Z
        LitToNat n = 'S (LitToNat (n - 1))
