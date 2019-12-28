module IntExecution where 

import IntSystem
import Memory
import Parsing
import InstructionExecution
import Control.Monad.State.Strict

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
  s <- get
  let p = ip (registers s)
      offsets = [Addr p, Addr (p + 1), Addr (p + 2), Addr (p + 3)] 
  words <- mapM memFetch offsets :: State System [MWord]
  pure $ fmap OpCode words  

step :: State System ()
step = do
  nextOp <- fetchNextOp
  let inst = parseInstructions nextOp
  -- runInstructionLogged (trace (show inst) inst)
  runInstructionLogged inst
  s <- get
    -- rewrite with lenses
  let is = instructionSize inst
      newIp =
        if isIpInstruction inst
          then ip (registers s)
          else ip (registers s) + is
  modify
    (\s -> s {registers = (registers s) {halt = inst == Terminate, ip = newIp}})

runUntilTermination :: State System ()
runUntilTermination = do
  currentState <- get
  if halt (registers currentState)
    then pure ()
    else do
      step
      runUntilTermination

runUntil :: (System -> Bool) -> State System ()
runUntil pred = do
  s <- get
  if pred s then pure () else do
    step
    runUntil pred