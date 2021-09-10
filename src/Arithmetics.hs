{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | Basic logic gates
module Arithmetics where

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
class Adder1 (a :: Axiom) (b :: Axiom) (c :: Axiom) (h :: Axiom) (l :: Axiom) | a b c -> h l

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
  Adder1 a b c h l

-- Input	Output
-- a	b	c	h	l
adderProof1 :: Adder1 F F F F F => b
adderProof1 = nil
adderProof2 :: Adder1 F F T F T => b
adderProof2 = nil
adderProof3 :: Adder1 F T F F T => b
adderProof3 = nil
adderProof4 :: Adder1 F T T T F => b
adderProof4 = nil
adderProof5 :: Adder1 T F F F T => b
adderProof5 = nil
adderProof6 :: Adder1 T F T T F => b
adderProof6 = nil
adderProof7 :: Adder1 T T F T F => b
adderProof7 = nil
adderProof8 :: Adder1 T T T T T => b
adderProof8 = nil


-- | Build an adder that adds two 2-bit numbers and a 1-bit carry.
-- * Input
-- a1 a0 is a 2-bit number.
-- b1 b0 is a 2-bit number.
-- c (input carry) is a 1-bit number.
-- * Output
-- The sum of the input numbers as the 3-bit number c s1 s0 where c is the high bit.

class Adder2 (a :: (Axiom, Axiom)) (b :: (Axiom, Axiom)) (c :: Axiom)
                    (cout :: Axiom) (s :: (Axiom, Axiom))
                    | a b c -> cout s

instance ( Adder1 c b0 a0 midh s0
         , Adder1 midh a1 b1 cout s1
         ) =>  Adder2 '(a1,a0) '(b1,b0) c cout '(s1,s0)

_2bitAdderProof1  :: Adder2 '(F,F) '(F,F) F F '(F,F) => b
_2bitAdderProof1  = nil
_2bitAdderProof2  :: Adder2 '(F,F) '(F,F) T F '(F,T) => b
_2bitAdderProof2  = nil
_2bitAdderProof3  :: Adder2 '(F,F) '(F,T) F F '(F,T) => b
_2bitAdderProof3  = nil
_2bitAdderProof4  :: Adder2 '(F,F) '(F,T) T F '(T,F) => b
_2bitAdderProof4  = nil
_2bitAdderProof5  :: Adder2 '(F,F) '(T,F) F F '(T,F) => b
_2bitAdderProof5  = nil
_2bitAdderProof6  :: Adder2 '(F,F) '(T,F) T F '(T,T) => b
_2bitAdderProof6  = nil
_2bitAdderProof7  :: Adder2 '(F,F) '(T,T) F F '(T,T) => b
_2bitAdderProof7  = nil
_2bitAdderProof8  :: Adder2 '(F,F) '(T,T) T T '(F,F) => b
_2bitAdderProof8  = nil
_2bitAdderProof9  :: Adder2 '(F,T) '(F,F) F F '(F,T) => b
_2bitAdderProof9  = nil
_2bitAdderProof10 :: Adder2 '(F,T) '(F,F) T F '(T,F) => b
_2bitAdderProof10 = nil

class Adder4 (a :: (Axiom, Axiom, Axiom, Axiom)) (b :: (Axiom, Axiom, Axiom, Axiom)) (c :: Axiom)
                   (cout :: Axiom) (s :: (Axiom, Axiom, Axiom, Axiom)) | a b c -> cout s

instance (
    Adder2 '(a1, a0) '(b1, b0) c midh '(s1, s0)
    , Adder2 '(a3, a2) '(b3, b2) midh cout '(s3, s2)
                      ) => Adder4 '(a3, a2, a1, a0) '(b3, b2, b1, b0) c cout '(s3, s2, s1, s0)

_4bitAdderProof3  :: Adder4 '(T , T , F , F)  '(F , F , F , T) F  F '(T, T, F, T) => b
_4bitAdderProof3  = nil
_4bitAdderProof4  :: Adder4 '(T , T , F , T)  '(F , F , F , F) T  F '(T, T, T, F) => b
_4bitAdderProof4  = nil
_4bitAdderProof5  :: Adder4 '(T , T , F , T)  '(F , F , T , F) F  F '(T, T, T, T) => b
_4bitAdderProof5  = nil
_4bitAdderProof6  :: Adder4 '(F , T , T , F)  '(F , F , T , F) T  F '(T, F, F, T) => b
_4bitAdderProof6  = nil
_4bitAdderProof7  :: Adder4 '(F , F , F , T)  '(F , T , T , T) F  F '(T, F, F, F) => b
_4bitAdderProof7  = nil
_4bitAdderProof8  :: Adder4 '(F , F , F , T)  '(F , T , T , T) T  F '(T, F, F, T) => b
_4bitAdderProof8  = nil
_4bitAdderProof9  :: Adder4 '(T , T , T , T)  '(T , T , T , T) F  T '(T, T, T, F) => b
_4bitAdderProof9  = nil
_4bitAdderProof10 :: Adder4 '(T , T , T , T)  '(T , T , T , T) T  T '(T, T, T, T) => b
_4bitAdderProof10 = nil
_4bitAdderProof11 :: Adder4 '(T , F , T , F)  '(T , F , T , F) F  T '(F, T, F, F) => b
_4bitAdderProof11 = nil
_4bitAdderProof12 :: Adder4 '(T , F , T , T)  '(T , F , T , T) F  T '(F, T, T, F) => b
_4bitAdderProof12 = nil
_4bitAdderProof13 :: Adder4 '(T , T , F , F)  '(T , T , F , F) F  T '(T, F, F, F) => b
_4bitAdderProof13 = nil
_4bitAdderProof14 :: Adder4 '(T , T , F , T)  '(T , T , F , T) F  T '(T, F, T, F) => b
_4bitAdderProof14 = nil
_4bitAdderProof15 :: Adder4 '(T , T , T , F)  '(T , T , T , F) F  T '(T, T, F, F) => b
_4bitAdderProof15 = nil
_4bitAdderProof16 :: Adder4 '(F , T , F , T)  '(F , T , F , T) T  F '(T, F, T, T) => b
_4bitAdderProof16 = nil
_4bitAdderProof17 :: Adder4 '(F , F , F , T)  '(F , F , F , T) T  F '(F, F, T, T) => b
_4bitAdderProof17 = nil

class Increment (a  :: (Axiom, Axiom, Axiom, Axiom)) (s  :: (Axiom, Axiom, Axiom, Axiom)) | a -> s
instance
  (Adder4 a '(F , F , F , F) T cout s) => Increment a s

incProof :: Increment '(F , F , F , F)  '(F , F , F , T) => b
incProof = nil

incProof2 :: Increment '(F , F , F , T)  '(F , F , T , F) => b
incProof2 = nil

incProof16 :: Increment '(T , T , T , F)  '(T , T , T , T) => b
incProof16 = nil

incProofOverflow :: Increment '(T , T , T , T)  '(F , F , F , F) => b
incProofOverflow = nil


class Invert (a  :: (Axiom, Axiom, Axiom, Axiom)) (s  :: (Axiom, Axiom, Axiom, Axiom)) | a -> s
instance (
  Not a3 s3, Not a2 s2, Not a1 s1, Not a0 s0) => Invert '(a3, a2, a1, a0) '(s3, s2, s1, s0)


invProof :: Invert '(F , F , F , F)  '(T , T , T , T) => b
invProof = nil

invProof2 :: Invert '(T , F , T , F)  '(F , T , F , T) => b
invProof2 = nil

class Subtract (a  :: (Axiom, Axiom, Axiom, Axiom)) (b  :: (Axiom, Axiom, Axiom, Axiom)) (s  :: (Axiom, Axiom, Axiom, Axiom)) | a b -> s
instance (
         Invert b bout
         , Adder4 a bout F c sout
         , Increment sout s
         ) => Subtract a b s


subtractProof0 :: Subtract '(F , F , F , F)  '(F , F , F , F) '(F , F , F , F) => b
subtractProof0 = nil
subtractProof1 :: Subtract '(F , F , F , T)  '(F , F , F , F) '(F , F , F , T) => b
subtractProof1 = nil
subtractProof2 :: Subtract '(F , F , F , T)  '(F , F , F , T) '(F , F , F , F) => b
subtractProof2 = nil
subtractProof15 :: Subtract '(F , F , F , T)  '(F , F , T , F) '(T , T , T , T) => b
subtractProof15 = nil
subtractProof14 :: Subtract '(F , F , F , T)  '(F , F , T , T) '(T , T , T , F) => b
subtractProof14 = nil
subtractProof22 :: Subtract '(F , F , T , F)  '(F , F , T , F) '(F , F , F , F) => b
subtractProof22 = nil
subtractProof30 :: Subtract '(T , F , F , F)  '(T , F , F , F) '(F , F , F , F) => b
subtractProof30 = nil
subtractProof40 :: Subtract '(T , F , F , F)  '(T , F , F , T) '(T , T , T , T) => b
subtractProof40 = nil


class IsZero (a  :: (Axiom, Axiom, Axiom, Axiom)) (s :: Axiom)

instance (
          Or a3 a2 o1
         , Or a1 a0 o2
         , Or o1 o2 o3
         , Not o3 s
         ) => IsZero '(a3, a2, a1, a0) s


zeroProof :: IsZero '(F , F , F , F)  T => b
zeroProof = nil
zeroProof1 :: IsZero '(T , F , F , F)  F => b
zeroProof1 = nil

class IsNegative (a  :: (Axiom, Axiom, Axiom, Axiom)) (s :: Axiom)
instance IsNegative '(a3, a2, a1, a0) a3

negativeProof :: IsNegative '(T , F , F , F)  T => b
negativeProof = nil
negativeProof2 :: IsNegative '(F , F , F , F)  F => b
negativeProof2 = nil

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
    _2bitAdderProof1
    _2bitAdderProof2
    _2bitAdderProof3
    _2bitAdderProof4
    _2bitAdderProof5
    _2bitAdderProof6
    _2bitAdderProof7
    _2bitAdderProof8
    _2bitAdderProof9
    _2bitAdderProof10
    _4bitAdderProof3
    _4bitAdderProof4
    _4bitAdderProof5
    _4bitAdderProof6
    _4bitAdderProof7
    _4bitAdderProof8
    _4bitAdderProof9
    _4bitAdderProof10
    _4bitAdderProof11
    _4bitAdderProof12
    _4bitAdderProof13
    _4bitAdderProof14
    _4bitAdderProof15
    _4bitAdderProof16
    _4bitAdderProof17
    incProof
    incProofOverflow
    incProof2
    incProof16
    invProof
    invProof2
    subtractProof0
    subtractProof1
    subtractProof2
    subtractProof15
    subtractProof14
    subtractProof22
    subtractProof30
    subtractProof40
    zeroProof1
    zeroProof
    negativeProof2
    negativeProof
