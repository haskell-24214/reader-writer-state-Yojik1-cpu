module Tier2.State where

import Control.Monad.State

data Registers = Registers { ax :: Int, bx :: Int, blink :: Bool, acc :: Int }

emptyRegisters = Registers 0 0 False 0

type Calculation = State Registers Int

plus :: Calculation
plus = do
  reg <- get
  let a = ax reg
      b = bx reg
      r = a + b
  put reg { acc = r, blink = False }
  return r

minus :: Calculation
minus = do
  reg <- get
  let a = ax reg
      b = bx reg
      r = a - b
  put reg { acc = r, blink = False }
  return r

productS :: Calculation
productS = do
  reg <- get
  let a = ax reg
      b = bx reg
      r = a * b
  put reg { acc = r, blink = False }
  return r

divS :: Calculation
divS = do
  reg <- get
  let a = ax reg
      b = bx reg
  if b == 0
    then do
      put (Registers 0 0 False 0)
      return 0
    else do
      let r = a `div` b
      put reg { acc = r, blink = False }
      return r

swap :: Calculation
swap = do
  reg <- get
  let a = ax reg
      b = bx reg
  put reg { ax = b, bx = a }
  return (acc reg)

blinkS :: Calculation
blinkS = do
  reg <- get
  let bl = blink reg
  put reg { blink = not bl }
  return (acc reg)

accS :: Calculation
accS = do
  reg <- get
  if blink reg
    then put reg { bx = acc reg, blink = not (blink reg) }
    else put reg { ax = acc reg, blink = not (blink reg) }
  gets acc

number :: Int -> Calculation
number x = do
  reg <- get
  if blink reg
    then put reg { bx = x, blink = not (blink reg) }
    else put reg { ax = x, blink = not (blink reg) }
  return (acc reg)

commandToCalculation :: String -> Calculation
commandToCalculation s =
  case s of
    "+" -> plus
    "-" -> minus
    "*" -> productS
    "/" -> divS
    "swap" -> swap
    "blink" -> blinkS
    "acc" -> accS
    x -> number (read x)

buildCalculation :: [String] -> Calculation
buildCalculation xs = 
  foldl (\a x -> a >>= (\_ -> x)) (state (\s -> (0, s))) (map commandToCalculation xs)

calculate :: [String] -> Int
calculate xs = evalState (buildCalculation xs) emptyRegisters