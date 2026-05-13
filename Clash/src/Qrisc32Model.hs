module Qrisc32Model
  ( VectorSpec(..)
  , vectorSpec
  , runVector
  ) where

import Data.Bits
import Data.Char (isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Numeric (readHex)

data VectorSpec = VectorSpec
  { vectorName :: String
  , hexFile :: FilePath
  , dmemInit :: Maybe FilePath
  , expectedDmem :: [(Int, Int)]
  , passLine :: String
  }

data Cpu = Cpu
  { pc :: Int
  , regs :: Map Int Int
  , dmem :: Map Int Int
  , flagZ :: Bool
  , flagC :: Bool
  }

mask32 :: Int
mask32 = 0xffffffff

u32 :: Int -> Int
u32 x = x .&. mask32

reg :: Cpu -> Int -> Int
reg cpu idx = Map.findWithDefault 0 idx (regs cpu)

setReg :: Int -> Int -> Cpu -> Cpu
setReg idx value cpu = cpu { regs = Map.insert idx (u32 value) (regs cpu) }

sext :: Int -> Int -> Int
sext bits value =
  let sign = 1 `shiftL` (bits - 1)
      masked = value .&. ((1 `shiftL` bits) - 1)
  in u32 $ if masked .&. sign /= 0 then masked - (1 `shiftL` bits) else masked

incrValue :: Int -> (Int, Bool)
incrValue 0 = (0, False)
incrValue 1 = (1, True)
incrValue 2 = (2, True)
incrValue 3 = (4, True)
incrValue 4 = (0, False)
incrValue 5 = (-1, True)
incrValue 6 = (-2, True)
incrValue _ = (-4, True)

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

loadWords :: FilePath -> IO [Int]
loadWords path = do
  text <- readFile path
  pure [value | line <- lines text, let t = trim line, not (null t), let value = readHexWord t]
  where
    readHexWord s =
      case readHex s of
        [(value, "")] -> value
        _ -> error ("bad hex word: " ++ s)

vectorSpec :: String -> Maybe VectorSpec
vectorSpec "smoke" =
  Just $ VectorSpec "smoke" "../tests/smoke.hex" (Just "../tests/smoke_reversed.hex") (zip [0..] [1..10]) "PASS: qrisc32 sort smoke"
vectorSpec "flags" =
  Just $ VectorSpec "flags" "../tests/flags.hex" Nothing [(0, 5), (1, 7)] "PASS: dmem[0]=5 dmem[1]=7"
vectorSpec "isa" =
  Just $ VectorSpec "isa" "../tests/isa.hex" (Just "../tests/dmem_zero.hex") (zip [0..] isaExpected) "PASS: ISA parity vector"
vectorSpec "axi" =
  Just $ VectorSpec "axi" "../tests/axi_smoke.hex" Nothing [(0, 12)] "PASS: AXI smoke dmem[0] = 12"
vectorSpec _ = Nothing

isaExpected :: [Int]
isaExpected =
  [ 0, 1, 0, 0, 0, 1, 0, 1
  , 0, 0, 0, 1, 1, 0, 0, 1
  , 0, 1, 1, 0, 0, 1, 1, 0
  , 1, 0, 0, 1
  ]

runVector :: VectorSpec -> IO (Map Int Int)
runVector spec = do
  imem <- loadWords (hexFile spec)
  initWords <- maybe (pure []) loadWords (dmemInit spec)
  let initial = Cpu
        { pc = 0
        , regs = Map.empty
        , dmem = Map.fromList (zip [0..] initWords)
        , flagZ = False
        , flagC = False
        }
  pure $ dmem $ runCpu imem initial 4096 0 (-1)

runCpu :: [Int] -> Cpu -> Int -> Int -> Int -> Cpu
runCpu _ cpu 0 _ _ = cpu
runCpu imem cpu cycles samePc lastPc =
  let oldPc = pc cpu
      cpu' = step imem cpu
      samePc' = if pc cpu' == oldPc && oldPc == lastPc then samePc + 1 else 0
  in if samePc' >= 4
       then cpu'
       else runCpu imem cpu' (cycles - 1) samePc' oldPc

step :: [Int] -> Cpu -> Cpu
step imem cpu =
  let idx = pc cpu `shiftR` 2
      inst = if idx >= 0 && idx < length imem then imem !! idx else 0
      currentPc = pc cpu
      cpuBase = cpu { pc = u32 (pc cpu + 4) }
      op = (inst `shiftR` 28) .&. 0xf
      mode = (inst `shiftR` 26) .&. 0x3
      dst = inst .&. 0x1f
      src1 = (inst `shiftR` 5) .&. 0x1f
      src2 = (inst `shiftR` 10) .&. 0x1f
      imm16 = (inst `shiftR` 5) .&. 0xffff
      off15 = (inst `shiftR` 10) .&. 0x7fff
      (incr, incrEn0) = incrValue ((inst `shiftR` 22) .&. 0x7)
      r1 = reg cpu src1
      r2 = reg cpu src2
      dstVal = reg cpu dst
      offset = if testBit inst 25 then r2 else sext 15 off15
      writeFinal writeReg writeValue incTarget incSource c =
        let c1 = if writeReg then setReg dst writeValue c else c
        in maybe c1 (\target -> setReg target (incSource + incr) c1) incTarget
      defaultInc = if incrEn0 then Just src2 else Nothing
  in case op of
      0x0 -> case mode of
        0 -> writeFinal (dst /= src1) r1 defaultInc r2 cpuBase
        1 -> writeFinal True (((dstVal .&. 0x0000ffff) .|. (imm16 `shiftL` 16))) Nothing r2 cpuBase
        2 -> writeFinal True (((dstVal .&. 0xffff0000) .|. imm16)) Nothing r2 cpuBase
        _ ->
          let addr = u32 (r1 + offset)
              value = Map.findWithDefault 0 (addr `shiftR` 2) (dmem cpu)
              incTarget = if testBit inst 25 then Just src2 else Nothing
          in writeFinal True value incTarget r2 cpuBase
      0x1 ->
        let addr = u32 (r1 + offset)
            incTarget = if testBit inst 25 then Just src2 else Nothing
            c = cpuBase { dmem = Map.insert (addr `shiftR` 2) dstVal (dmem cpuBase) }
        in writeFinal False 0 incTarget r2 c
      0x2 -> case mode of
        0 -> writeFinal False 0 Nothing r2 cpuBase { pc = inst .&. 0x03ffffff }
        1 ->
          let c = cpuBase { pc = u32 (currentPc + offset) }
              incTarget = if testBit inst 25 then Just src2 else Nothing
          in writeFinal False 0 incTarget r2 c
        2 ->
          let c = cpuBase { pc = u32 (currentPc + offset) }
              incTarget = if testBit inst 25 then Just src2 else Nothing
          in writeFinal True currentPc incTarget r2 c
        _ -> writeFinal False 0 (if testBit inst 25 then Just src2 else Nothing) r2 cpuBase { pc = dstVal }
      0x3 ->
        let cond = case mode of
              0 -> flagZ cpu
              1 -> not (flagZ cpu)
              2 -> flagC cpu
              _ -> not (flagC cpu)
            c = if cond then cpuBase { pc = u32 (currentPc + offset) } else cpuBase
            incTarget = if testBit inst 25 then Just src2 else Nothing
        in writeFinal False 0 incTarget r2 c
      0x4 ->
        let alu = (inst `shiftR` 25) .&. 0x7
        in execAlu alu dst src2 incr defaultInc r1 r2 cpuBase
      0x5 ->
        let cond = case mode of
              0 -> flagZ cpu
              1 -> not (flagZ cpu)
              2 -> flagC cpu
              _ -> not (flagC cpu)
            value = if cond then r1 else r2
        in writeFinal True value defaultInc r2 cpuBase
      _ -> cpuBase

execAlu :: Int -> Int -> Int -> Int -> Maybe Int -> Int -> Int -> Cpu -> Cpu
execAlu alu dst _src2 incr incTarget r1 r2 cpu =
  let write value c = setReg dst value c
      inc c = maybe c (\target -> setReg target (r2 + incr) c) incTarget
  in case alu of
      0 -> inc $ write (r1 .&. r2) cpu { flagC = False, flagZ = (r1 .&. r2) == 0 }
      1 -> inc $ write (r1 .|. r2) cpu { flagC = False, flagZ = (r1 .|. r2) == 0 }
      2 -> inc $ write (xor r1 r2) cpu { flagC = False, flagZ = xor r1 r2 == 0 }
      3 ->
        let result = r1 + r2
            value = u32 result
        in inc $ write value cpu { flagC = result > mask32, flagZ = value == 0 }
      4 ->
        let value = u32 (r1 * r2)
        in inc $ write value cpu { flagC = False, flagZ = value == 0 }
      5 ->
        let value = u32 (r1 `shiftL` (r2 .&. 31))
        in inc $ write value cpu { flagC = False, flagZ = value == 0 }
      6 ->
        let shiftAmt = r2 .&. 31
            combined = (r1 `shiftL` 1) .&. 0x1ffffffff
            shr = combined `shiftR` shiftAmt
            value = (shr `shiftR` 1) .&. mask32
        in inc $ write value cpu { flagC = testBit shr 0, flagZ = value == 0 }
      _ -> inc cpu { flagZ = r1 == r2, flagC = r1 < r2 }
