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

class FirstOrFalse (input :: '[ Axiom ]) (out :: Axiom ) | input -> out
instance FirstOrFalse '[] F
instance FirstOrFalse '[x ': _rem] x

-- | A latch component stores and outputs a single bit
--
-- When st (store) is 1, the value on d is stored and emitted.
--
-- When st is 0, the value of d is ignored, and the previously stored value is still emitted.
--
-- we use a list of axioms to indicate the passage of time, where every item is a tick
-- first item is processed last. in [a,b,c], c will be processed firt, then b, then a
class Latch (input :: '[ '( Axiom, Axiom ) ] ) (out :: '[ Axiom ]) | input -> out

instance Latch '[] '[]
instance Latch
  (
    FirstOrFalse prevOut prevMem
  , Latch rem prevOut
  , Selector st d prevMem out
  )
  '[ '(st, d) ': rem ]  '[ out ': prevOut]

instance (
          Selector st d out out
         ) => Latch st d out


latchProof1 :: Latch T F F => b
latchProof1 = nil
latchProof2 :: Latch T T T => b
latchProof2 = nil


--- wait, how do I make this step?
latchProof3 :: forall a b . Latch F T a => b
latchProof3 = nil

x :: Int
x =
   latchProof1
   latchProof2
   (latchProof3 @T)
   (latchProof3 @F)
