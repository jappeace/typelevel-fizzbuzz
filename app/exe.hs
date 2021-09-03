{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import GHC.TypeLits
import Data.Proxy
import Data.Type.Equality

type family ShowNat (x :: Nat) :: Symbol where
  ShowNat 0 = "0"
  ShowNat 1 = "1"
  ShowNat 2 = "2"
  ShowNat 3 = "3"
  ShowNat 4 = "4"
  ShowNat 5 = "5"
  ShowNat 6 = "6"
  ShowNat 7 = "7"
  ShowNat 8 = "8"
  ShowNat 9 = "9"
  ShowNat other = AppendSymbol (ShowNat (Div other 10)) (ShowNat (Mod other 10))

type family If (cond :: Bool) (truth :: k) (false :: k) :: k where
  If 'True truth _ = truth
  If _ _ false = false

type family And (a :: Bool) (b :: Bool) :: Bool where
  And 'True 'True = True
  And _ _         = False

type family FizzBuzz (x :: Nat) :: Symbol where
  FizzBuzz x =
    If (And (Mod x 3 == 0) (Mod x 5 == 0)) "Fizz Buzz"
    (If (Mod x 3 == 0) "Fizz"
    (If (Mod x 5 == 0) "Buzz" (ShowNat x)))

type family ShowOrd  (x :: Ordering) :: Symbol where
  ShowOrd  'LT = "LT"
  ShowOrd  'GT = "GT"
  ShowOrd  'EQ = "EQ"

-- I couldn't use my mcgyvered If because ghc would evaluate all
-- branches anyway. This fixed it for some reason
type family DecideBranch (start :: Nat) (end :: Nat) (ord :: Ordering) :: [Symbol] where
  DecideBranch start end 'LT = ((FizzBuzz start)) ': (FizzBuzzes (start + 1) end)
  DecideBranch start _ _ = ((FizzBuzz start) ': '[])

type family FizzBuzzes (start :: Nat) (end :: Nat) :: [Symbol] where
  FizzBuzzes start end = DecideBranch start end (CmpNat start end)


type family FoldCommas (input :: [Symbol]) :: Symbol where
  FoldCommas '[] = ""
  FoldCommas (x ': rem) = AppendSymbol (AppendSymbol x ",") (FoldCommas rem)

main :: IO ()
main = putStrLn $
  symbolVal (Proxy @(FoldCommas (FizzBuzzes 1 95)))
