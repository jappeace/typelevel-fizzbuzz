{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables#-}

-- | Basic logic gates
module Arithmetics where

import GHC.TypeLits
import Data.Proxy
import Data.Type.Equality
import LogicGates

-- | An add component that adds two bits. The output is a two-bit value.
-- | The h output is the high bit, the l is the low bit.

class HalfAdder (a :: Axiom) (b :: Axiom) (h :: Axiom) (l :: Axiom) | a b -> h l

instance ( And a b h
         , Xor a b l) =>
  HalfAdder a b h l

halfAdderProof1 :: HalfAdder F F F F => b
halfAdderProof1 = nil
halfAdderProof2 :: HalfAdder F T F T => b
halfAdderProof2 = nil
halfAdderProof3 :: HalfAdder T F F T => b
halfAdderProof3 = nil
halfAdderProof4 :: HalfAdder T T T F => b
halfAdderProof4 = nil

x :: Int -- inhabitation means proved
x = halfAdderProof1
    halfAdderProof2
    halfAdderProof3
    halfAdderProof4
