{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Memory where

import Plumbing
import LogicGates

-- | A latch component stores and outputs a single bit
--
-- When st (store) is 1, the value on d is stored and emitted.
--
-- When st is 0, the value of d is ignored, and the previously stored value is still emitted.
class Latch st d out

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
