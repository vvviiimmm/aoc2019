module Memory where

import IntSystem
import Control.Monad.State.Strict
import qualified Data.Map as Map
import Data.Maybe
import Control.Lens

memFetch :: Addr -> State System MWord
memFetch addr = do
  m <- use memory
  (pure . fromMaybe 0 . Map.lookup addr) m

memFetchRelative :: RelAddr -> State System MWord
memFetchRelative relAddr = do
  addr <- resolveAddr relAddr
  memFetch addr

memStore :: Addr -> MWord -> State System ()
memStore addr value = memory %= Map.insert addr value

memStoreRelative :: RelAddr -> MWord -> State System ()
memStoreRelative relAddr value = do
  addr <- resolveAddr relAddr
  memStore addr value

resolveAddr :: RelAddr -> State System Addr
resolveAddr addr = do
  base <- use (registers . relativeBase)
  pure $ Addr (base + unRelAddr addr)