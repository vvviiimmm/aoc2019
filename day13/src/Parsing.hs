module Parsing where 

import IntSystem

parseInstructions :: [OpCode] -> Instruction
parseInstructions (OpCode 1:a:b:c:xs) = Add (Position (toAddr a)) (Position (toAddr b)) (Position (toAddr c))
parseInstructions (OpCode 101:a:b:c:xs) = Add (Immediate (unOpCode a)) (Position (toAddr b)) (Position (toAddr c))
parseInstructions (OpCode 1001:a:b:c:xs) = Add (Position (toAddr a)) (Immediate (unOpCode b)) (Position (toAddr c))
parseInstructions (OpCode 1101:a:b:c:xs) = Add (Immediate (unOpCode a)) (Immediate (unOpCode b)) (Position (toAddr c))

parseInstructions (OpCode 201:a:b:c:xs) = Add (Relative (toRelAddr a)) (Position (toAddr b)) (Position (toAddr c))
parseInstructions (OpCode 2001:a:b:c:xs) = Add (Position (toAddr a)) (Relative (toRelAddr b)) (Position (toAddr c))
parseInstructions (OpCode 1201:a:b:c:xs) = Add (Relative (toRelAddr a)) (Immediate (unOpCode b)) (Position (toAddr c))
parseInstructions (OpCode 2101:a:b:c:xs) = Add (Immediate (unOpCode a)) (Relative (toRelAddr b)) (Position (toAddr c))
parseInstructions (OpCode 2201:a:b:c:xs) = Add (Relative (toRelAddr a)) (Relative (toRelAddr b)) (Position (toAddr c))


parseInstructions (OpCode 2:a:b:c:xs) = Mult (Position (toAddr a)) (Position (toAddr b)) (Position (toAddr c))
parseInstructions (OpCode 102:a:b:c:xs) = Mult (Immediate (unOpCode a)) (Position (toAddr b)) (Position (toAddr c))
parseInstructions (OpCode 1002:a:b:c:xs) = Mult (Position (toAddr a)) (Immediate (unOpCode b)) (Position (toAddr c))
parseInstructions (OpCode 1102:a:b:c:xs) = Mult (Immediate (unOpCode a)) (Immediate (unOpCode b)) (Position (toAddr c))

parseInstructions (OpCode 202:a:b:c:xs) = Mult (Relative (toRelAddr a)) (Position (toAddr b)) (Position (toAddr c))
parseInstructions (OpCode 2002:a:b:c:xs) = Mult (Position (toAddr a)) (Relative (toRelAddr b)) (Position (toAddr c))
parseInstructions (OpCode 1202:a:b:c:xs) = Mult (Relative (toRelAddr a)) (Immediate (unOpCode b)) (Position (toAddr c))
parseInstructions (OpCode 2102:a:b:c:xs) = Mult (Immediate (unOpCode a)) (Relative (toRelAddr b)) (Position (toAddr c))
parseInstructions (OpCode 2202:a:b:c:xs) = Mult (Relative (toRelAddr a)) (Relative (toRelAddr b)) (Position (toAddr c))

-- Relative destination
parseInstructions (OpCode 20001:a:b:c:xs) = Add (Position (toAddr a)) (Position (toAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 20101:a:b:c:xs) = Add (Immediate (unOpCode a)) (Position (toAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 21001:a:b:c:xs) = Add (Position (toAddr a)) (Immediate (unOpCode b)) (Relative (toRelAddr c))
parseInstructions (OpCode 21101:a:b:c:xs) = Add (Immediate (unOpCode a)) (Immediate (unOpCode b)) (Relative (toRelAddr c))

parseInstructions (OpCode 20201:a:b:c:xs) = Add (Relative (toRelAddr a)) (Position (toAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 22001:a:b:c:xs) = Add (Position (toAddr a)) (Relative (toRelAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 21201:a:b:c:xs) = Add (Relative (toRelAddr a)) (Immediate (unOpCode b)) (Relative (toRelAddr c))
parseInstructions (OpCode 22101:a:b:c:xs) = Add (Immediate (unOpCode a)) (Relative (toRelAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 22201:a:b:c:xs) = Add (Relative (toRelAddr a)) (Relative (toRelAddr b)) (Relative (toRelAddr c))


parseInstructions (OpCode 20002:a:b:c:xs) = Mult (Position (toAddr a)) (Position (toAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 20102:a:b:c:xs) = Mult (Immediate (unOpCode a)) (Position (toAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 21002:a:b:c:xs) = Mult (Position (toAddr a)) (Immediate (unOpCode b)) (Relative (toRelAddr c))
parseInstructions (OpCode 21102:a:b:c:xs) = Mult (Immediate (unOpCode a)) (Immediate (unOpCode b)) (Relative (toRelAddr c))

parseInstructions (OpCode 20202:a:b:c:xs) = Mult (Relative (toRelAddr a)) (Position (toAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 22002:a:b:c:xs) = Mult (Position (toAddr a)) (Relative (toRelAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 21202:a:b:c:xs) = Mult (Relative (toRelAddr a)) (Immediate (unOpCode b)) (Relative (toRelAddr c))
parseInstructions (OpCode 22102:a:b:c:xs) = Mult (Immediate (unOpCode a)) (Relative (toRelAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 22202:a:b:c:xs) = Mult (Relative (toRelAddr a)) (Relative (toRelAddr b)) (Relative (toRelAddr c))
---

parseInstructions (OpCode 3:a:xs) = Input (Position (toAddr a))
parseInstructions (OpCode 203:a:xs) = Input (Relative (toRelAddr a))

parseInstructions (OpCode 4:a:xs) = Output (Position (toAddr a))
parseInstructions (OpCode 104:a:xs) = Output (Immediate (unOpCode a))
parseInstructions (OpCode 204:a:xs) = Output (Relative (toRelAddr a))

parseInstructions (OpCode 5:a:b:xs) = JmpIfTrue (Position (toAddr a)) (Position (toAddr b))
parseInstructions (OpCode 105:a:b:xs) = JmpIfTrue (Immediate (unOpCode a)) (Position (toAddr b))
parseInstructions (OpCode 1005:a:b:xs) = JmpIfTrue (Position (toAddr a)) (Immediate (unOpCode b))
parseInstructions (OpCode 1105:a:b:xs) = JmpIfTrue (Immediate (unOpCode a)) (Immediate (unOpCode b))

parseInstructions (OpCode 205:a:b:xs) = JmpIfTrue (Relative (toRelAddr a)) (Position (toAddr b))
parseInstructions (OpCode 2005:a:b:xs) = JmpIfTrue (Position (toAddr a)) (Relative (toRelAddr b))
parseInstructions (OpCode 1205:a:b:xs) = JmpIfTrue (Relative (toRelAddr a)) (Immediate (unOpCode b))
parseInstructions (OpCode 2105:a:b:xs) = JmpIfTrue (Immediate (unOpCode a)) (Relative (toRelAddr b))
parseInstructions (OpCode 2205:a:b:xs) = JmpIfTrue (Relative (toRelAddr a)) (Relative (toRelAddr b))


parseInstructions (OpCode 6:a:b:xs) = JmpIfFalse (Position (toAddr a)) (Position (toAddr b))
parseInstructions (OpCode 106:a:b:xs) = JmpIfFalse (Immediate (unOpCode a)) (Position (toAddr b))
parseInstructions (OpCode 1006:a:b:xs) = JmpIfFalse (Position (toAddr a)) (Immediate (unOpCode b))
parseInstructions (OpCode 1106:a:b:xs) = JmpIfFalse (Immediate (unOpCode a)) (Immediate (unOpCode b))

parseInstructions (OpCode 206:a:b:xs) = JmpIfFalse (Relative (toRelAddr a)) (Position (toAddr b))
parseInstructions (OpCode 2006:a:b:xs) = JmpIfFalse (Position (toAddr a)) (Relative (toRelAddr b))
parseInstructions (OpCode 1206:a:b:xs) = JmpIfFalse (Relative (toRelAddr a)) (Immediate (unOpCode b))
parseInstructions (OpCode 2106:a:b:xs) = JmpIfFalse (Immediate (unOpCode a)) (Relative (toRelAddr b))
parseInstructions (OpCode 2206:a:b:xs) = JmpIfFalse (Relative (toRelAddr a)) (Relative (toRelAddr b))

parseInstructions (OpCode 7:a:b:c:xs) = LessThan (Position (toAddr a)) (Position (toAddr b)) (Position (toAddr c))
parseInstructions (OpCode 107:a:b:c:xs) = LessThan (Immediate (unOpCode a)) (Position (toAddr b)) (Position (toAddr c))
parseInstructions (OpCode 1007:a:b:c:xs) = LessThan (Position (toAddr a)) (Immediate (unOpCode b)) (Position (toAddr c))
parseInstructions (OpCode 1107:a:b:c:xs) = LessThan (Immediate (unOpCode a)) (Immediate (unOpCode b)) (Position (toAddr c))

parseInstructions (OpCode 207:a:b:c:xs) = LessThan (Relative (toRelAddr a)) (Position (toAddr b)) (Position (toAddr c))
parseInstructions (OpCode 2007:a:b:c:xs) = LessThan (Position (toAddr a)) (Relative (toRelAddr b)) (Position (toAddr c))
parseInstructions (OpCode 1207:a:b:c:xs) = LessThan (Relative (toRelAddr a)) (Immediate (unOpCode b)) (Position (toAddr c))
parseInstructions (OpCode 2107:a:b:c:xs) = LessThan (Immediate (unOpCode a)) (Relative (toRelAddr b)) (Position (toAddr c))
parseInstructions (OpCode 2207:a:b:c:xs) = LessThan (Relative (toRelAddr a)) (Relative (toRelAddr b)) (Position (toAddr c))

parseInstructions (OpCode 8:a:b:c:xs) = Equals (Position (toAddr a)) (Position (toAddr b)) (Position (toAddr c))
parseInstructions (OpCode 108:a:b:c:xs) = Equals (Immediate (unOpCode a)) (Position (toAddr b)) (Position (toAddr c))
parseInstructions (OpCode 1008:a:b:c:xs) = Equals (Position (toAddr a)) (Immediate (unOpCode b)) (Position (toAddr c))
parseInstructions (OpCode 1108:a:b:c:xs) = Equals (Immediate (unOpCode a)) (Immediate (unOpCode b)) (Position (toAddr c))

parseInstructions (OpCode 208:a:b:c:xs) = Equals (Relative (toRelAddr a)) (Position (toAddr b)) (Position (toAddr c))
parseInstructions (OpCode 2008:a:b:c:xs) = Equals (Position (toAddr a)) (Relative (toRelAddr b)) (Position (toAddr c))
parseInstructions (OpCode 1208:a:b:c:xs) = Equals (Relative (toRelAddr a)) (Immediate (unOpCode b)) (Position (toAddr c))
parseInstructions (OpCode 2108:a:b:c:xs) = Equals (Immediate (unOpCode a)) (Relative (toRelAddr b)) (Position (toAddr c))
parseInstructions (OpCode 2208:a:b:c:xs) = Equals (Relative (toRelAddr a)) (Relative (toRelAddr b)) (Position (toAddr c))

-- Relative destination
parseInstructions (OpCode 20007:a:b:c:xs) = LessThan (Position (toAddr a)) (Position (toAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 20107:a:b:c:xs) = LessThan (Immediate (unOpCode a)) (Position (toAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 21007:a:b:c:xs) = LessThan (Position (toAddr a)) (Immediate (unOpCode b)) (Relative (toRelAddr c))
parseInstructions (OpCode 21107:a:b:c:xs) = LessThan (Immediate (unOpCode a)) (Immediate (unOpCode b)) (Relative (toRelAddr c))

parseInstructions (OpCode 20207:a:b:c:xs) = LessThan (Relative (toRelAddr a)) (Position (toAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 22007:a:b:c:xs) = LessThan (Position (toAddr a)) (Relative (toRelAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 21207:a:b:c:xs) = LessThan (Relative (toRelAddr a)) (Immediate (unOpCode b)) (Relative (toRelAddr c))
parseInstructions (OpCode 22107:a:b:c:xs) = LessThan (Immediate (unOpCode a)) (Relative (toRelAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 22207:a:b:c:xs) = LessThan (Relative (toRelAddr a)) (Relative (toRelAddr b)) (Relative (toRelAddr c))

parseInstructions (OpCode 20008:a:b:c:xs) = Equals (Position (toAddr a)) (Position (toAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 20108:a:b:c:xs) = Equals (Immediate (unOpCode a)) (Position (toAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 21008:a:b:c:xs) = Equals (Position (toAddr a)) (Immediate (unOpCode b)) (Relative (toRelAddr c))
parseInstructions (OpCode 21108:a:b:c:xs) = Equals (Immediate (unOpCode a)) (Immediate (unOpCode b)) (Relative (toRelAddr c))

parseInstructions (OpCode 20208:a:b:c:xs) = Equals (Relative (toRelAddr a)) (Position (toAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 22008:a:b:c:xs) = Equals (Position (toAddr a)) (Relative (toRelAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 21208:a:b:c:xs) = Equals (Relative (toRelAddr a)) (Immediate (unOpCode b)) (Relative (toRelAddr c))
parseInstructions (OpCode 22108:a:b:c:xs) = Equals (Immediate (unOpCode a)) (Relative (toRelAddr b)) (Relative (toRelAddr c))
parseInstructions (OpCode 22208:a:b:c:xs) = Equals (Relative (toRelAddr a)) (Relative (toRelAddr b)) (Relative (toRelAddr c))
---

parseInstructions (OpCode 9:a:xs) = ModifyRelBase (Position (toAddr a))
parseInstructions (OpCode 109:a:xs) = ModifyRelBase (Immediate (unOpCode a))
parseInstructions (OpCode 209:a:xs) = ModifyRelBase (Relative (toRelAddr a))

parseInstructions (OpCode 99:xs) = Terminate
parseInstructions z = error (show z)

toAddr :: OpCode -> Addr
toAddr = Addr . unOpCode

toRelAddr :: OpCode -> RelAddr
toRelAddr = RelAddr . unOpCode