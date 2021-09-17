{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Memory where

import Plumbing
import LogicGates

class FirstOrFalse (input :: [Axiom]) (out :: Axiom ) | input -> out
instance FirstOrFalse '[] F
instance FirstOrFalse (x ': _rem) x

-- | A latch component stores and outputs a single bit
--
-- When st (store) is 1, the value on d is stored and emitted.
--
-- When st is 0, the value of d is ignored, and the previously stored value is still emitted.
--
-- we use a list of axioms to indicate the passage of time, where every item is a tick
-- first item is processed last. in [a,b,c], c will be processed firt, then b, then a
class Latch (input :: [ ( Axiom, Axiom ) ] ) (out :: [Axiom]) | input -> out

instance Latch '[] '[]
instance (
          FirstOrFalse prevOut prevMem
        , Latch rem prevOut
        , Selector st d prevMem out
        ) => Latch ('(st, d) ': rem )  (out ': prevOut)



latchStProof1 :: Latch '[ '( T, F)] '[F] => b
latchStProof1 = nil
latchStProof2 :: Latch '[ '( T, T)] '[T] => b
latchStProof2 = nil

latchSetTrueMem :: Latch '[ '( F, F), '( T, T)] '[T , T] => b
latchSetTrueMem = nil
latchSetTrueMem2 :: Latch '[ '( F, F), '( F, F), '( T, T)] '[T, T, T] => b
latchSetTrueMem2 = nil

latchSetTrueFalseMem :: Latch '[ '( F, T), '( T, F), '( F, F), '( T, T)] '[F, F, T , T] => b
latchSetTrueFalseMem = nil

x :: Int
x =
   latchStProof1
   latchStProof2
   latchSetTrueMem
   latchSetTrueMem2
   latchSetTrueFalseMem
