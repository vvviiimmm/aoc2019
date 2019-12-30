module IntIO where

import IntSystem
import Memory
import Control.Monad.State.Strict as State
import Control.Lens

import Debug.Trace

acceptInput :: Addr -> State System ()
acceptInput dst = do
  inp <- use input
  -- let xs = trace ("Input: " <> (show $ input s)) (input s)
  memStore dst (head inp)
  input .= tail inp

acceptInputRelative :: RelAddr -> State System ()
acceptInputRelative addr = do
  actualAddr <- resolveAddr addr
  acceptInput actualAddr

produceOutput :: MWord -> State System ()
produceOutput value = output %= (++ [value])