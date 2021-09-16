{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Plumbing where

import LogicGates

-- | A select-component selects one out of two input bits for output.
--
--   The s (select) bit indicates which input is selected:
--   If 0, d0 is selected, if 1, d1 is selected.
class Selector (s :: Axiom) (d1 :: Axiom) (d0 :: Axiom) (out :: Axiom) | s d0 d1 -> out

instance
  (
    And s d1 a1
  , Not s n1
  , And n1 d0 b1
  , Or a1 b1 out
  ) =>
  Selector  s d1 d0 out


selectorProof1 :: Selector F F F F => b
selectorProof1 = nil
selectorProof2 :: Selector F T F F => b
selectorProof2 = nil
selectorProof3 :: Selector F F T T => b
selectorProof3 = nil
selectorProof4 :: Selector F T T T => b
selectorProof4 = nil
selectorProof5 :: Selector T F F F => b
selectorProof5 = nil
selectorProof6 :: Selector T F T F => b
selectorProof6 = nil
selectorProof7 :: Selector T T F T => b
selectorProof7 = nil
selectorProof8 :: Selector T T T T => b
selectorProof8 = nil

class Switch s d c1 c0 | s d -> c1 c0

instance
  (
    And s d c1
  , Xor c1 d c0
  ) => Switch s d c1 c0
switchProof1 :: Switch F F F F => b
switchProof1 = nil
switchProof2 :: Switch F T F T => b
switchProof2 = nil
switchProof3 :: Switch T F F F => b
switchProof3 = nil
switchProof4 :: Switch T T T F => b
switchProof4 = nil

x :: Int -- inhabitation means proved
x =
   selectorProof1
   selectorProof2
   selectorProof3
   selectorProof4
   selectorProof5
   selectorProof6
   selectorProof7
   selectorProof8
   switchProof1
   switchProof2
   switchProof3
   switchProof4
