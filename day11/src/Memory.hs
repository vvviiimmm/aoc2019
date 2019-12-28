module Memory where

import IntSystem
import Control.Monad.State.Strict
import qualified Data.Map as Map
import Data.Maybe

memFetch :: Addr -> State System MWord
memFetch addr = do
  s <- get
  (pure . fromMaybe 0 . Map.lookup addr) (memory s)

memFetchRelative :: RelAddr -> State System MWord
memFetchRelative relAddr = do
  addr <- resolveAddr relAddr
  memFetch addr
  
memStore :: Addr -> MWord -> State System ()
memStore addr value =
  modify (\s -> s {memory = (Map.insert addr value (memory s))})

memStoreRelative :: RelAddr -> MWord -> State System ()
memStoreRelative relAddr value = do
  addr <- resolveAddr relAddr
  memStore addr value

resolveAddr :: RelAddr -> State System Addr
resolveAddr addr = do
  s <- get
  let base = relativeBase (registers s)
      actualAddr = Addr (base + (unRelAddr addr))
  pure actualAddr