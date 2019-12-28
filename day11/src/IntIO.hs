module IntIO where

import IntSystem
import Memory
import Control.Monad.State.Strict as State

import Debug.Trace

acceptInput :: Addr -> State System ()
acceptInput dst = do
  s <- get
  -- let xs = trace ("Input: " <> (show $ input s)) (input s)
  let xs = input s
  memStore dst (head xs)
  modify (\s -> s {input = tail xs})

acceptInputRelative :: RelAddr -> State System ()
acceptInputRelative addr = do
  actualAddr <- resolveAddr addr
  acceptInput actualAddr

produceOutput :: MWord -> State System ()
produceOutput value = modify (\s -> s {output = output s ++ [value]})