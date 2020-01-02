module IntExecution where

import Control.Lens
import Control.Monad.State.Strict
import InstructionExecution
import IntSystem
import Memory
import Parsing

instructionSize :: Instruction -> Integer
instructionSize Add {} = 4
instructionSize Mult {} = 4
instructionSize Input {} = 2
instructionSize Output {} = 2
instructionSize JmpIfTrue {} = 3
instructionSize JmpIfFalse {} = 3
instructionSize LessThan {} = 4
instructionSize Equals {} = 4
instructionSize ModifyRelBase {} = 2
instructionSize Terminate {} = 1
instructionSize Unknown {} = 1

isIpInstruction :: Instruction -> Bool
isIpInstruction JmpIfTrue {} = True
isIpInstruction JmpIfFalse {} = True
isIpInstruction _ = False

fetchNextOp :: State System [OpCode]
fetchNextOp = do
  i <- use (registers . ip)
  words <- mapM memFetch [Addr i, Addr (i + 1), Addr (i + 2), Addr (i + 3)]
  pure $ fmap OpCode words

step :: State System ()
step = do
  nextOp <- fetchNextOp
  let inst = parseInstructions nextOp
  -- runInstructionLogged (trace (show inst) inst)
  runInstructionLogged inst
  i <- use (registers . ip)
  let is = instructionSize inst
      newIp =
        if isIpInstruction inst
          then i
          else i + is
  (registers . halt) .= (inst == Terminate)
  (registers . ip) .= newIp

runUntilTermination :: State System ()
runUntilTermination = do
  h <- use (registers . halt)
  if h
    then pure ()
    else step >> runUntilTermination

runUntil :: (System -> Bool) -> State System ()
runUntil pred = do
  s <- get
  if pred s
    then pure ()
    else do
      step
      runUntil pred
