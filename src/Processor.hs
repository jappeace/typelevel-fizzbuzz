{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables#-}

-- | a 4 bit processor at typelevel for executing fizzbuzz program
module Processor where

import GHC.TypeLits
import Data.Proxy
import Data.Type.Equality

data Axiom = Truth -- ^ when someone tells you the truth, like that.
            | Lies -- ^ when someone tells you the truth, but not.

-- our only function
class Nand (in1 :: Axiom) (in2 :: Axiom) (out :: Axiom) | in1 in2 -> out

-- a truth table
instance Nand Truth Truth Lies
instance Nand Truth Lies Truth
instance Nand Lies Truth Truth
instance Nand Lies Lies Truth

-- all other functions will be aliases
type Not input = (Nand input input)

-- shorthand for that other stuff no one cares about
nil = error "undefined"

-- tests be like this
notProof1 :: Not Truth Lies => b
notProof1 = nil

notProof2 :: Not Lies Truth => b
notProof2 = nil

-- type alias didn't work with partial application.
-- guess we just use more typeclasses
class And input input2 ultout
instance (Not x out, Nand input input2 x) =>
  And input input2 out -- note that we're not even a fundep

andProof1 :: And Truth Truth Truth => b
andProof1 = nil
andProof2 :: And Lies Truth Lies => b
andProof2 = nil
andProof3 :: And Lies Lies Lies => b
andProof3 = nil
andProof4 :: And Truth Lies Lies => b
andProof4 = nil

x :: Int
x = notProof1
    notProof2
    andProof1
    andProof2
    andProof3
    andProof4
