module Interpret where

import IntSystem
import Memory
import IntIO
import Control.Monad.State.Strict
import Control.Lens

runInstruction :: Instruction -> State System ()
runInstruction op =
  case op of
    Add (Position a) (Position b) (Position dst) -> do
      x <- memFetch a
      y <- memFetch b
      memStore dst (x + y)
    Add (Immediate a) (Position b) (Position dst) -> do
      x <- memFetch b
      memStore dst (x + a)
    Add (Position a) (Immediate b) (Position dst) -> do
      x <- memFetch a
      memStore dst (x + b)
    Add (Immediate a) (Immediate b) (Position dst) -> memStore dst (a + b)

    Add (Position a) (Relative b) (Position dst) -> do
      x <- memFetch a
      y <- memFetchRelative b
      memStore dst (x + y)
    Add (Relative a) (Position b) (Position dst) -> do
      x <- memFetchRelative a
      y <- memFetch b
      memStore dst (x + y)
    Add (Immediate a) (Relative b) (Position dst) -> do
      y <- memFetchRelative b
      memStore dst (a + y)
    Add (Relative a) (Immediate b) (Position dst) -> do
      x <- memFetchRelative a
      memStore dst (x + b)
    Add (Relative a) (Relative b) (Position dst) -> do
      x <- memFetchRelative a
      y <- memFetchRelative b
      memStore dst (x + y)

    -- Rel
    Add (Position a) (Position b) (Relative dst) -> do
      x <- memFetch a
      y <- memFetch b
      memStoreRelative dst (x + y)
    Add (Immediate a) (Position b) (Relative dst) -> do
      x <- memFetch b
      memStoreRelative dst (x + a)
    Add (Position a) (Immediate b) (Relative dst) -> do
      x <- memFetch a
      memStoreRelative dst (x + b)
    Add (Immediate a) (Immediate b) (Relative dst) -> memStoreRelative dst (a + b)

    Add (Position a) (Relative b) (Relative dst) -> do
      x <- memFetch a
      y <- memFetchRelative b
      memStoreRelative dst (x + y)
    Add (Relative a) (Position b) (Relative dst) -> do
      x <- memFetchRelative a
      y <- memFetch b
      memStoreRelative dst (x + y)
    Add (Immediate a) (Relative b) (Relative dst) -> do
      y <- memFetchRelative b
      memStoreRelative dst (a + y)
    Add (Relative a) (Immediate b) (Relative dst) -> do
      x <- memFetchRelative a
      memStoreRelative dst (x + b)
    Add (Relative a) (Relative b) (Relative dst) -> do
      x <- memFetchRelative a
      y <- memFetchRelative b
      memStoreRelative dst (x + y)
    --


    Mult (Position a) (Position b) (Position dst) -> do
      x <- memFetch a
      y <- memFetch b
      memStore dst (x * y)
    Mult (Immediate a) (Position b) (Position dst) -> do
      x <- memFetch b
      memStore dst (x * a)
    Mult (Position a) (Immediate b) (Position dst) -> do
      x <- memFetch a
      memStore dst (x * b)
    Mult (Immediate a) (Immediate b) (Position dst) -> memStore dst (a * b)

    Mult (Position a) (Relative b) (Position dst) -> do
      x <- memFetch a
      y <- memFetchRelative b
      memStore dst (x * y)
    Mult (Relative a) (Position b) (Position dst) -> do
      x <- memFetchRelative a
      y <- memFetch b
      memStore dst (x * y)
    Mult (Immediate a) (Relative b) (Position dst) -> do
      y <- memFetchRelative b
      memStore dst (a * y)
    Mult (Relative a) (Immediate b) (Position dst) -> do
      x <- memFetchRelative a
      memStore dst (x * b)
    Mult (Relative a) (Relative b) (Position dst) -> do
      x <- memFetchRelative a
      y <- memFetchRelative b
      memStore dst (x * y)

    -- Rel
    Mult (Position a) (Position b) (Relative dst) -> do
      x <- memFetch a
      y <- memFetch b
      memStoreRelative dst (x * y)
    Mult (Immediate a) (Position b) (Relative dst) -> do
      x <- memFetch b
      memStoreRelative dst (x * a)
    Mult (Position a) (Immediate b) (Relative dst) -> do
      x <- memFetch a
      memStoreRelative dst (x * b)
    Mult (Immediate a) (Immediate b) (Relative dst) -> memStoreRelative dst (a * b)

    Mult (Position a) (Relative b) (Relative dst) -> do
      x <- memFetch a
      y <- memFetchRelative b
      memStoreRelative dst (x * y)
    Mult (Relative a) (Position b) (Relative dst) -> do
      x <- memFetchRelative a
      y <- memFetch b
      memStoreRelative dst (x * y)
    Mult (Immediate a) (Relative b) (Relative dst) -> do
      y <- memFetchRelative b
      memStoreRelative dst (a * y)
    Mult (Relative a) (Immediate b) (Relative dst) -> do
      x <- memFetchRelative a
      memStoreRelative dst (x * b)
    Mult (Relative a) (Relative b) (Relative dst) -> do
      x <- memFetchRelative a
      y <- memFetchRelative b
      memStoreRelative dst (x * y)

    Input (Position addr) -> acceptInput addr
    Input (Relative addr) -> acceptInputRelative addr

    Output (Position addr) -> memFetch addr >>= produceOutput
    Output (Immediate value) -> produceOutput value
    Output (Relative addr) -> memFetchRelative addr >>= produceOutput

    JmpIfTrue (Position a) (Position b) -> do
      x <- memFetch a
      y <- memFetch b
      if x /= 0
        then setIp y
        else addToIp 3
    JmpIfTrue (Immediate a) (Position b) -> do
      y <- memFetch b
      if a /= 0
        then setIp y
        else addToIp 3
    JmpIfTrue (Position a) (Immediate b) -> do
      x <- memFetch a
      if x /= 0
        then setIp b
        else addToIp 3
    JmpIfTrue (Immediate a) (Immediate b) ->
      if a /= 0
        then setIp b
        else addToIp 3

    JmpIfTrue (Position a) (Relative b) -> do
      x <- memFetch a
      y <- memFetchRelative b
      if x /= 0
        then setIp y
        else addToIp 3
    JmpIfTrue (Relative a) (Position b) -> do
      x <- memFetchRelative a
      y <- memFetch b
      if x /= 0
        then setIp y
        else addToIp 3
    JmpIfTrue (Immediate a) (Relative b) -> do
      y <- memFetchRelative b
      if a /= 0
        then setIp y
        else addToIp 3
    JmpIfTrue (Relative a) (Immediate b) -> do
      x <- memFetchRelative a
      if x /= 0
        then setIp b
        else addToIp 3
    JmpIfTrue (Relative a) (Relative b) -> do
      x <- memFetchRelative a
      y <- memFetchRelative b
      if x /= 0
        then setIp y
        else addToIp 3

    JmpIfFalse (Position a) (Position b) -> do
      x <- memFetch a
      y <- memFetch b
      if x == 0
        then setIp y
        else addToIp 3
    JmpIfFalse (Immediate a) (Position b) -> do
      y <- memFetch b
      if a == 0
        then setIp y
        else addToIp 3
    JmpIfFalse (Position a) (Immediate b) -> do
      x <- memFetch a
      if x == 0
        then setIp b
        else addToIp 3
    JmpIfFalse (Immediate a) (Immediate b) ->
      if a == 0
        then setIp b
        else addToIp 3

    JmpIfFalse (Position a) (Relative b) -> do
      x <- memFetch a
      y <- memFetchRelative b
      if x == 0
        then setIp y
        else addToIp 3
    JmpIfFalse (Relative a) (Position b) -> do
      x <- memFetchRelative a
      y <- memFetch b
      if x == 0
        then setIp y
        else addToIp 3
    JmpIfFalse (Immediate a) (Relative b) -> do
      y <- memFetchRelative b
      if a == 0
        then setIp y
        else addToIp 3
    JmpIfFalse (Relative a) (Immediate b) -> do 
      x <- memFetchRelative a
      if x == 0
        then setIp b
        else addToIp 3
    JmpIfFalse (Relative a) (Relative b) -> do 
      x <- memFetchRelative a
      y <- memFetchRelative b
      if x == 0
        then setIp y
        else addToIp 3



    LessThan (Position a) (Position b) (Position dst) -> do
      x <- memFetch a
      y <- memFetch b
      if x < y
        then memStore dst 1
        else memStore dst 0
    LessThan (Immediate a) (Position b) (Position dst) -> do
      x <- memFetch b
      if a < x
        then memStore dst 1
        else memStore dst 0
    LessThan (Position a) (Immediate b) (Position dst) -> do
      x <- memFetch a
      if x < b
        then memStore dst 1
        else memStore dst 0
    LessThan (Immediate a) (Immediate b) (Position dst) ->
      if a < b
        then memStore dst 1
        else memStore dst 0

    LessThan (Position a) (Relative b) (Position dst) -> do
      x <- memFetch a
      y <- memFetchRelative b
      if x < y
        then memStore dst 1
        else memStore dst 0
    LessThan (Relative a) (Position b) (Position dst) -> do
      x <- memFetchRelative a
      y <- memFetch b
      if x < y
        then memStore dst 1
        else memStore dst 0
    LessThan (Immediate a) (Relative b) (Position dst) -> do
      y <- memFetchRelative b
      if a < y
        then memStore dst 1
        else memStore dst 0
    LessThan (Relative a) (Immediate b) (Position dst) -> do 
      x <- memFetchRelative a
      if x < b
        then memStore dst 1
        else memStore dst 0
    LessThan (Relative a) (Relative b) (Position dst) -> do 
      x <- memFetchRelative a
      y <- memFetchRelative b
      if x < y
        then memStore dst 1
        else memStore dst 0

    -- Rel
    LessThan (Position a) (Position b) (Relative dst) -> do
      x <- memFetch a
      y <- memFetch b
      if x < y
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    LessThan (Immediate a) (Position b) (Relative dst) -> do
      x <- memFetch b
      if a < x
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    LessThan (Position a) (Immediate b) (Relative dst) -> do
      x <- memFetch a
      if x < b
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    LessThan (Immediate a) (Immediate b) (Relative dst) ->
      if a < b
        then memStoreRelative dst 1
        else memStoreRelative dst 0

    LessThan (Position a) (Relative b) (Relative dst) -> do
      x <- memFetch a
      y <- memFetchRelative b
      if x < y
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    LessThan (Relative a) (Position b) (Relative dst) -> do
      x <- memFetchRelative a
      y <- memFetch b
      if x < y
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    LessThan (Immediate a) (Relative b) (Relative dst) -> do
      y <- memFetchRelative b
      if a < y
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    LessThan (Relative a) (Immediate b) (Relative dst) -> do 
      x <- memFetchRelative a
      if x < b
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    LessThan (Relative a) (Relative b) (Relative dst) -> do 
      x <- memFetchRelative a
      y <- memFetchRelative b
      if x < y
        then memStoreRelative dst 1
        else memStoreRelative dst 0



    Equals (Position a) (Position b) (Position dst) -> do
      x <- memFetch a
      y <- memFetch b
      if x == y
        then memStore dst 1
        else memStore dst 0
    Equals (Immediate a) (Position b) (Position dst) -> do
      x <- memFetch b
      if a == x
        then memStore dst 1
        else memStore dst 0
    Equals (Position a) (Immediate b) (Position dst) -> do
      x <- memFetch a
      if x == b
        then memStore dst 1
        else memStore dst 0
    Equals (Immediate a) (Immediate b) (Position dst) ->
      if a == b
        then memStore dst 1
        else memStore dst 0


    
    Equals (Position a) (Relative b) (Position dst) -> do
      x <- memFetch a
      y <- memFetchRelative b
      if x == y
        then memStore dst 1
        else memStore dst 0
    Equals (Relative a) (Position b) (Position dst) -> do
      x <- memFetchRelative a
      y <- memFetch b
      if x == y
        then memStore dst 1
        else memStore dst 0
    Equals (Immediate a) (Relative b) (Position dst) -> do
      y <- memFetchRelative b
      if a == y
        then memStore dst 1
        else memStore dst 0
    Equals (Relative a) (Immediate b) (Position dst) -> do
      x <- memFetchRelative a
      if x == b
        then memStore dst 1
        else memStore dst 0
    Equals (Relative a) (Relative b) (Position dst) -> do
      x <- memFetchRelative a
      y <- memFetchRelative b
      if x == y
        then memStore dst 1
        else memStore dst 0

    -- Rel
    Equals (Position a) (Position b) (Relative dst) -> do
      x <- memFetch a
      y <- memFetch b
      if x == y
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    Equals (Immediate a) (Position b) (Relative dst) -> do
      x <- memFetch b
      if a == x
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    Equals (Position a) (Immediate b) (Relative dst) -> do
      x <- memFetch a
      if x == b
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    Equals (Immediate a) (Immediate b) (Relative dst) ->
      if a == b
        then memStoreRelative dst 1
        else memStoreRelative dst 0


    
    Equals (Position a) (Relative b) (Relative dst) -> do
      x <- memFetch a
      y <- memFetchRelative b
      if x == y
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    Equals (Relative a) (Position b) (Relative dst) -> do
      x <- memFetchRelative a
      y <- memFetch b
      if x == y
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    Equals (Immediate a) (Relative b) (Relative dst) -> do
      y <- memFetchRelative b
      if a == y
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    Equals (Relative a) (Immediate b) (Relative dst) -> do
      x <- memFetchRelative a
      if x == b
        then memStoreRelative dst 1
        else memStoreRelative dst 0
    Equals (Relative a) (Relative b) (Relative dst) -> do
      x <- memFetchRelative a
      y <- memFetchRelative b
      if x == y
        then memStoreRelative dst 1
        else memStoreRelative dst 0


    ModifyRelBase (Immediate a) -> addToRelativeBase a
    ModifyRelBase (Position a) -> memFetch a >>= addToRelativeBase
    ModifyRelBase (Relative a) -> memFetchRelative a >>= addToRelativeBase
    Terminate -> (registers . halt) .= True
    Unknown x -> pure ()

setIp :: MWord -> State System ()
setIp newIp = (registers . ip) .= newIp 

addToIp :: MWord -> State System ()
addToIp offset = (registers . ip) += offset

addToRelativeBase :: MWord -> State System ()
addToRelativeBase offset = (registers . relativeBase) += offset

runInstructionLogged :: Instruction -> State System ()
runInstructionLogged instruction = do
  logging <>= [show instruction]
  (registers . tick) += 1
  runInstruction instruction