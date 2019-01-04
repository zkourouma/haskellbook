module Stately where

newtype State s a = State
  { runState :: s -> (a, s)
  }
