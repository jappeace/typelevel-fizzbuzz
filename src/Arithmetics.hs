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

-- | An add component which adds three bits: a, b, and c.
-- The output is a two-bit value. The h output is the high bit, the l is the low bit.
class Adder (a :: Axiom) (b :: Axiom) (c :: Axiom) (h :: Axiom) (l :: Axiom) | a b c -> h l

instance (
          -- case of all enabled
           And b c allout1
         , And allout1 a allout2
         , Or allout2 lout2 l

         -- other cases
         , HalfAdder b c hout1 lout1
         , Or hout1 lout1 orout1
         , Or a hout1 orout2
         , HalfAdder orout2 orout1 h lout2
         ) =>
  Adder a b c h l

-- Input	Output
-- a	b	c	h	l
adderProof1 :: Adder F F F F F => b
adderProof1 = nil
adderProof2 :: Adder F F T F T => b
adderProof2 = nil
adderProof3 :: Adder F T F F T => b
adderProof3 = nil
adderProof4 :: Adder F T T T F => b
adderProof4 = nil
adderProof5 :: Adder T F F F T => b
adderProof5 = nil
adderProof6 :: Adder T F T T F => b
adderProof6 = nil
adderProof7 :: Adder T T F T F => b
adderProof7 = nil
adderProof8 :: Adder T T T T T => b
adderProof8 = nil

x :: Int -- inhabitation means proved
x = halfAdderProof1
    halfAdderProof2
    halfAdderProof3
    halfAdderProof4
    adderProof1
    adderProof2
    adderProof3
    adderProof4
    adderProof5
    adderProof6
    adderProof7
    adderProof8
