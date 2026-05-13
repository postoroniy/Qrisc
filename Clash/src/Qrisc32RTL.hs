{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Qrisc32RTL where

import Clash.Prelude

type U32 = BitVector 32
type RegIdx = Index 32

data AxiInputs = AxiInputs
  { iArReady :: Bit
  , iRId :: BitVector 1
  , iRData :: U32
  , iRResp :: BitVector 2
  , iRLast :: Bit
  , iRUser :: BitVector 1
  , iRValid :: Bit
  , dArReady :: Bit
  , dRId :: BitVector 1
  , dRData :: U32
  , dRResp :: BitVector 2
  , dRLast :: Bit
  , dRUser :: BitVector 1
  , dRValid :: Bit
  , dAwReady :: Bit
  , dWReady :: Bit
  , dBId :: BitVector 1
  , dBResp :: BitVector 2
  , dBUser :: BitVector 1
  , dBValid :: Bit
  , verboseIn :: Bit
  }
  deriving (Generic, NFDataX)

data AxiOutputs = AxiOutputs
  { axi_instruction_arid :: BitVector 1
  , axi_instruction_araddr :: U32
  , axi_instruction_arlen :: BitVector 8
  , axi_instruction_arsize :: BitVector 3
  , axi_instruction_arburst :: BitVector 2
  , axi_instruction_arlock :: Bit
  , axi_instruction_arcache :: BitVector 4
  , axi_instruction_arprot :: BitVector 3
  , axi_instruction_arqos :: BitVector 4
  , axi_instruction_arregion :: BitVector 4
  , axi_instruction_aruser :: BitVector 1
  , axi_instruction_arvalid :: Bit
  , axi_instruction_rready :: Bit
  , axi_data_arid :: BitVector 1
  , axi_data_araddr :: U32
  , axi_data_arlen :: BitVector 8
  , axi_data_arsize :: BitVector 3
  , axi_data_arburst :: BitVector 2
  , axi_data_arlock :: Bit
  , axi_data_arcache :: BitVector 4
  , axi_data_arprot :: BitVector 3
  , axi_data_arqos :: BitVector 4
  , axi_data_arregion :: BitVector 4
  , axi_data_aruser :: BitVector 1
  , axi_data_arvalid :: Bit
  , axi_data_rready :: Bit
  , axi_data_awid :: BitVector 1
  , axi_data_awaddr :: U32
  , axi_data_awlen :: BitVector 8
  , axi_data_awsize :: BitVector 3
  , axi_data_awburst :: BitVector 2
  , axi_data_awlock :: Bit
  , axi_data_awcache :: BitVector 4
  , axi_data_awprot :: BitVector 3
  , axi_data_awqos :: BitVector 4
  , axi_data_awregion :: BitVector 4
  , axi_data_awuser :: BitVector 1
  , axi_data_awvalid :: Bit
  , axi_data_wdata :: U32
  , axi_data_wstrb :: BitVector 4
  , axi_data_wlast :: Bit
  , axi_data_wuser :: BitVector 1
  , axi_data_wvalid :: Bit
  , axi_data_bready :: Bit
  }
  deriving (Generic, NFDataX)

data Phase
  = FetchAddr
  | FetchResp
  | Decode
  | Execute
  | DataReadAddr
  | DataReadResp
  | DataWriteAddrData Bool Bool
  | DataWriteResp
  | WriteBack
  deriving (Generic, NFDataX)

data Pending
  = NoPending
  | LoadPending RegIdx Bool RegIdx U32 U32
  | StorePending Bool RegIdx U32 U32
  deriving (Generic, NFDataX)

data CpuState = CpuState
  { pc :: U32
  , regs :: Vec 32 U32
  , flagZ :: Bool
  , flagC :: Bool
  , phase :: Phase
  , instReg :: U32
  , memAddr :: U32
  , memWData :: U32
  , pending :: Pending
  , wbNextPc :: U32
  , wbWriteReg :: Bool
  , wbDst :: RegIdx
  , wbValue :: U32
  , wbIncEn :: Bool
  , wbIncReg :: RegIdx
  , wbIncValue :: U32
  , wbFlagsWrite :: Bool
  , wbFlagZ :: Bool
  , wbFlagC :: Bool
  }
  deriving (Generic, NFDataX)

initialState :: CpuState
initialState =
  CpuState
    { pc = 0
    , regs = repeat 0
    , flagZ = False
    , flagC = False
    , phase = FetchAddr
    , instReg = 0
    , memAddr = 0
    , memWData = 0
    , pending = NoPending
    , wbNextPc = 0
    , wbWriteReg = False
    , wbDst = 0
    , wbValue = 0
    , wbIncEn = False
    , wbIncReg = 0
    , wbIncValue = 0
    , wbFlagsWrite = False
    , wbFlagZ = False
    , wbFlagC = False
    }

idleOutputs :: AxiOutputs
idleOutputs =
  AxiOutputs
    { axi_instruction_arid = 0
    , axi_instruction_araddr = 0
    , axi_instruction_arlen = 0
    , axi_instruction_arsize = 2
    , axi_instruction_arburst = 1
    , axi_instruction_arlock = 0
    , axi_instruction_arcache = 0
    , axi_instruction_arprot = 0
    , axi_instruction_arqos = 0
    , axi_instruction_arregion = 0
    , axi_instruction_aruser = 0
    , axi_instruction_arvalid = 0
    , axi_instruction_rready = 1
    , axi_data_arid = 0
    , axi_data_araddr = 0
    , axi_data_arlen = 0
    , axi_data_arsize = 2
    , axi_data_arburst = 1
    , axi_data_arlock = 0
    , axi_data_arcache = 0
    , axi_data_arprot = 0
    , axi_data_arqos = 0
    , axi_data_arregion = 0
    , axi_data_aruser = 0
    , axi_data_arvalid = 0
    , axi_data_rready = 1
    , axi_data_awid = 0
    , axi_data_awaddr = 0
    , axi_data_awlen = 0
    , axi_data_awsize = 2
    , axi_data_awburst = 1
    , axi_data_awlock = 0
    , axi_data_awcache = 0
    , axi_data_awprot = 0
    , axi_data_awqos = 0
    , axi_data_awregion = 0
    , axi_data_awuser = 0
    , axi_data_awvalid = 0
    , axi_data_wdata = 0
    , axi_data_wstrb = 0b1111
    , axi_data_wlast = 1
    , axi_data_wuser = 0
    , axi_data_wvalid = 0
    , axi_data_bready = 1
    }

bitSet :: Bit -> Bool
bitSet b = b == 1

idx5 :: BitVector 5 -> RegIdx
idx5 x = fromIntegral (unpack x :: Unsigned 5)

dstIdx :: U32 -> RegIdx
dstIdx inst = idx5 (slice d4 d0 inst)

src1Idx :: U32 -> RegIdx
src1Idx inst = idx5 (slice d9 d5 inst)

src2Idx :: U32 -> RegIdx
src2Idx inst = idx5 (slice d14 d10 inst)

regAt :: Vec 32 U32 -> RegIdx -> U32
regAt rs idx = rs !! idx

writeReg :: RegIdx -> U32 -> Vec 32 U32 -> Vec 32 U32
writeReg = replace

incrWord :: BitVector 3 -> U32
incrWord code =
  case code of
    1 -> 1
    2 -> 2
    3 -> 4
    5 -> 0xffff_ffff
    6 -> 0xffff_fffe
    7 -> 0xffff_fffc
    _ -> 0

incrEnabled :: BitVector 3 -> Bool
incrEnabled code =
  case code of
    1 -> True
    2 -> True
    3 -> True
    5 -> True
    6 -> True
    7 -> True
    _ -> False

commitWriteThenInc :: RegIdx -> U32 -> Bool -> RegIdx -> U32 -> U32 -> Vec 32 U32 -> Vec 32 U32
commitWriteThenInc dst value incEn incReg incBase incDelta rs =
  let rs1 = writeReg dst value rs
  in if incEn then writeReg incReg (incBase + incDelta) rs1 else rs1

commitIncOnly :: Bool -> RegIdx -> U32 -> U32 -> Vec 32 U32 -> Vec 32 U32
commitIncOnly incEn incReg incBase incDelta rs =
  if incEn then writeReg incReg (incBase + incDelta) rs else rs

sext15 :: BitVector 15 -> U32
sext15 x =
  if msb x == 1
    then 0xffff_8000 .|. zeroExtend x
    else zeroExtend x

low26 :: U32 -> U32
low26 inst = zeroExtend (slice d25 d0 inst :: BitVector 26)

shiftLeft32 :: U32 -> U32 -> U32
shiftLeft32 x amount =
  case (slice d4 d0 amount :: BitVector 5) of
    0 -> x
    1 -> x `shiftL` (1 :: Int)
    2 -> x `shiftL` (2 :: Int)
    3 -> x `shiftL` (3 :: Int)
    4 -> x `shiftL` (4 :: Int)
    5 -> x `shiftL` (5 :: Int)
    6 -> x `shiftL` (6 :: Int)
    7 -> x `shiftL` (7 :: Int)
    8 -> x `shiftL` (8 :: Int)
    9 -> x `shiftL` (9 :: Int)
    10 -> x `shiftL` (10 :: Int)
    11 -> x `shiftL` (11 :: Int)
    12 -> x `shiftL` (12 :: Int)
    13 -> x `shiftL` (13 :: Int)
    14 -> x `shiftL` (14 :: Int)
    15 -> x `shiftL` (15 :: Int)
    16 -> x `shiftL` (16 :: Int)
    17 -> x `shiftL` (17 :: Int)
    18 -> x `shiftL` (18 :: Int)
    19 -> x `shiftL` (19 :: Int)
    20 -> x `shiftL` (20 :: Int)
    21 -> x `shiftL` (21 :: Int)
    22 -> x `shiftL` (22 :: Int)
    23 -> x `shiftL` (23 :: Int)
    24 -> x `shiftL` (24 :: Int)
    25 -> x `shiftL` (25 :: Int)
    26 -> x `shiftL` (26 :: Int)
    27 -> x `shiftL` (27 :: Int)
    28 -> x `shiftL` (28 :: Int)
    29 -> x `shiftL` (29 :: Int)
    30 -> x `shiftL` (30 :: Int)
    _ -> x `shiftL` (31 :: Int)

aluShrBy :: U32 -> Int -> (U32, Bool)
aluShrBy a amount =
  let value = a `shiftR` amount
      carry = amount /= 0 && a ! (amount - 1) == 1
  in (value, carry)

aluAdd :: U32 -> U32 -> (U32, Bool)
aluAdd a b =
  let wide = (zeroExtend a :: BitVector 33) + (zeroExtend b :: BitVector 33)
  in (truncateB wide, msb wide == 1)

aluShr :: U32 -> U32 -> (U32, Bool)
aluShr a b =
  case (slice d4 d0 b :: BitVector 5) of
    0 -> aluShrBy a 0
    1 -> aluShrBy a 1
    2 -> aluShrBy a 2
    3 -> aluShrBy a 3
    4 -> aluShrBy a 4
    5 -> aluShrBy a 5
    6 -> aluShrBy a 6
    7 -> aluShrBy a 7
    8 -> aluShrBy a 8
    9 -> aluShrBy a 9
    10 -> aluShrBy a 10
    11 -> aluShrBy a 11
    12 -> aluShrBy a 12
    13 -> aluShrBy a 13
    14 -> aluShrBy a 14
    15 -> aluShrBy a 15
    16 -> aluShrBy a 16
    17 -> aluShrBy a 17
    18 -> aluShrBy a 18
    19 -> aluShrBy a 19
    20 -> aluShrBy a 20
    21 -> aluShrBy a 21
    22 -> aluShrBy a 22
    23 -> aluShrBy a 23
    24 -> aluShrBy a 24
    25 -> aluShrBy a 25
    26 -> aluShrBy a 26
    27 -> aluShrBy a 27
    28 -> aluShrBy a 28
    29 -> aluShrBy a 29
    30 -> aluShrBy a 30
    _ -> aluShrBy a 31

queueWb :: CpuState -> U32 -> Bool -> RegIdx -> U32 -> Bool -> RegIdx -> U32 -> U32 -> Bool -> Bool -> Bool -> CpuState
queueWb s nextPc writeEn dst value incEn incReg incBase incDelta flagsWrite zValue cValue =
  s { phase = WriteBack
    , pending = NoPending
    , wbNextPc = nextPc
    , wbWriteReg = writeEn
    , wbDst = dst
    , wbValue = value
    , wbIncEn = incEn
    , wbIncReg = incReg
    , wbIncValue = incBase + incDelta
    , wbFlagsWrite = flagsWrite
    , wbFlagZ = zValue
    , wbFlagC = cValue
    }

queueWbValue :: CpuState -> U32 -> Bool -> RegIdx -> U32 -> Bool -> RegIdx -> U32 -> Bool -> Bool -> Bool -> CpuState
queueWbValue s nextPc writeEn dst value incEn incReg incValue flagsWrite zValue cValue =
  s { phase = WriteBack
    , pending = NoPending
    , wbNextPc = nextPc
    , wbWriteReg = writeEn
    , wbDst = dst
    , wbValue = value
    , wbIncEn = incEn
    , wbIncReg = incReg
    , wbIncValue = incValue
    , wbFlagsWrite = flagsWrite
    , wbFlagZ = zValue
    , wbFlagC = cValue
    }

commitWb :: CpuState -> CpuState
commitWb s@CpuState{..} =
  let regsWrite = if wbWriteReg then writeReg wbDst wbValue regs else regs
      regsFinal = if wbIncEn then writeReg wbIncReg wbIncValue regsWrite else regsWrite
      flagZFinal = if wbFlagsWrite then wbFlagZ else flagZ
      flagCFinal = if wbFlagsWrite then wbFlagC else flagC
  in s { pc = wbNextPc
       , regs = regsFinal
       , flagZ = flagZFinal
       , flagC = flagCFinal
       , phase = FetchAddr
       , pending = NoPending
       }

decodeExecute :: CpuState -> U32 -> CpuState
decodeExecute s@CpuState{..} inst =
  let op = slice d31 d28 inst :: BitVector 4
      mode = slice d27 d26 inst :: BitVector 2
      alu = slice d27 d25 inst :: BitVector 3
      dst = dstIdx inst
      src1 = src1Idx inst
      src2 = src2Idx inst
      r1 = regAt regs src1
      r2 = regAt regs src2
      dstVal = regAt regs dst
      imm16 = slice d20 d5 inst :: BitVector 16
      off15 = slice d24 d10 inst :: BitVector 15
      useRegOffset = inst ! (25 :: Int) == 1
      incCode = slice d24 d22 inst :: BitVector 3
      incDelta = incrWord incCode
      defaultIncEn = incrEnabled incCode
      offset = if useRegOffset then r2 else sext15 off15
      seqPc = pc + 4
      writeMaybe doWrite value incEn =
        queueWb s seqPc doWrite dst value incEn src2 r2 incDelta False False False
      jumpState target incEn writeRet =
        queueWb s target writeRet dst pc incEn src2 r2 incDelta False False False
      branch cond =
        let target = pc + offset
        in queueWb s (if cond then target else seqPc) False dst 0 useRegOffset src2 r2 incDelta False False False
      ldrf cond =
        let value = if cond then r1 else r2
        in queueWb s seqPc True dst value defaultIncEn src2 r2 incDelta False False False
      memRead =
        s { pc = seqPc
          , phase = DataReadAddr
          , memAddr = r1 + offset
          , pending = LoadPending dst useRegOffset src2 r2 incDelta
          }
      memWrite =
        s { pc = seqPc
          , phase = DataWriteAddrData False False
          , memAddr = r1 + offset
          , memWData = dstVal
          , pending = StorePending useRegOffset src2 r2 incDelta
          }
      aluResult value carry =
        queueWb s seqPc True dst value defaultIncEn src2 r2 incDelta True (value == 0) carry
  in case op of
      0x0 ->
        case mode of
          0 -> writeMaybe (dst /= src1) r1 defaultIncEn
          1 ->
            let value = (dstVal .&. 0x0000_ffff) .|. (zeroExtend imm16 `shiftL` 16)
            in writeMaybe True value False
          2 ->
            let value = (dstVal .&. 0xffff_0000) .|. zeroExtend imm16
            in writeMaybe True value False
          _ -> memRead
      0x1 -> memWrite
      0x2 ->
        case mode of
          0 -> jumpState (low26 inst) False False
          1 -> jumpState (pc + offset) useRegOffset False
          2 -> jumpState (pc + offset) useRegOffset True
          _ -> jumpState dstVal useRegOffset False
      0x3 ->
        case mode of
          0 -> branch flagZ
          1 -> branch (not flagZ)
          2 -> branch flagC
          _ -> branch (not flagC)
      0x4 ->
        case alu of
          0 ->
            let value = r1 .&. r2
            in aluResult value False
          1 ->
            let value = r1 .|. r2
            in aluResult value False
          2 ->
            let value = r1 `xor` r2
            in aluResult value False
          3 ->
            let (value, carry) = aluAdd r1 r2
            in aluResult value carry
          4 ->
            let value = r1 * r2
            in aluResult value False
          5 ->
            let value = shiftLeft32 r1 r2
            in aluResult value False
          6 ->
            let (value, carry) = aluShr r1 r2
            in aluResult value carry
          _ ->
            queueWb s seqPc False dst 0 defaultIncEn src2 r2 incDelta True (r1 == r2) (r1 < r2)
      0x5 ->
        case mode of
          0 -> ldrf flagZ
          1 -> ldrf (not flagZ)
          2 -> ldrf flagC
          _ -> ldrf (not flagC)
      _ -> queueWb s seqPc False dst 0 defaultIncEn src2 r2 incDelta False False False

commitLoad :: CpuState -> U32 -> CpuState
commitLoad s@CpuState{..} value =
  case pending of
    LoadPending dst incEn incReg incBase incDelta ->
      queueWb s pc True dst value incEn incReg incBase incDelta False False False
    _ -> s { phase = FetchAddr, pending = NoPending }

commitStore :: CpuState -> CpuState
commitStore s@CpuState{..} =
  case pending of
    StorePending incEn incReg incBase incDelta ->
      queueWb s pc False 0 0 incEn incReg incBase incDelta False False False
    _ -> s { phase = FetchAddr, pending = NoPending }

step :: CpuState -> AxiInputs -> (CpuState, AxiOutputs)
step s@CpuState{..} i =
  case phase of
    FetchAddr ->
      let out = idleOutputs { axi_instruction_araddr = pc, axi_instruction_arvalid = 1 }
          s' = if bitSet (iArReady i) then s { phase = FetchResp } else s
      in (s', out)
    FetchResp ->
      let s' = if bitSet (iRValid i) then s { instReg = iRData i, phase = Decode } else s
      in (s', idleOutputs)
    Decode ->
      (s { phase = Execute }, idleOutputs)
    Execute ->
      let s' = decodeExecute s instReg
      in (s', idleOutputs)
    DataReadAddr ->
      let out = idleOutputs { axi_data_araddr = memAddr, axi_data_arvalid = 1 }
          s' = if bitSet (dArReady i) then s { phase = DataReadResp } else s
      in (s', out)
    DataReadResp ->
      let s' = if bitSet (dRValid i) then commitLoad s (dRData i) else s
      in (s', idleOutputs)
    DataWriteAddrData awDone wDone ->
      let awValid = not awDone
          wValid = not wDone
          awFire = awValid && bitSet (dAwReady i)
          wFire = wValid && bitSet (dWReady i)
          awDone' = awDone || awFire
          wDone' = wDone || wFire
          out = idleOutputs
            { axi_data_awaddr = memAddr
            , axi_data_awvalid = if awValid then 1 else 0
            , axi_data_wdata = memWData
            , axi_data_wvalid = if wValid then 1 else 0
            }
          sWait =
            if awDone' && wDone'
              then s { phase = DataWriteResp }
              else s { phase = DataWriteAddrData awDone' wDone' }
          s' =
            if awDone' && wDone' && bitSet (dBValid i)
              then commitStore s
              else sWait
      in (s', out)
    DataWriteResp ->
      let s' = if bitSet (dBValid i) then commitStore s else s
      in (s', idleOutputs)
    WriteBack ->
      (commitWb s, idleOutputs)

{-# ANN topEntity
  (Synthesize
    { t_name = "qrisc32"
    , t_inputs =
      [ PortName "clk"
      , PortName "areset"
      , PortName "axi_instruction_arready"
      , PortName "axi_instruction_rid"
      , PortName "axi_instruction_rdata"
      , PortName "axi_instruction_rresp"
      , PortName "axi_instruction_rlast"
      , PortName "axi_instruction_ruser"
      , PortName "axi_instruction_rvalid"
      , PortName "axi_data_arready"
      , PortName "axi_data_rid"
      , PortName "axi_data_rdata"
      , PortName "axi_data_rresp"
      , PortName "axi_data_rlast"
      , PortName "axi_data_ruser"
      , PortName "axi_data_rvalid"
      , PortName "axi_data_awready"
      , PortName "axi_data_wready"
      , PortName "axi_data_bid"
      , PortName "axi_data_bresp"
      , PortName "axi_data_buser"
      , PortName "axi_data_bvalid"
      , PortName "verbose"
      ]
    , t_output =
      PortProduct ""
      [ PortName "axi_instruction_arid"
      , PortName "axi_instruction_araddr"
      , PortName "axi_instruction_arlen"
      , PortName "axi_instruction_arsize"
      , PortName "axi_instruction_arburst"
      , PortName "axi_instruction_arlock"
      , PortName "axi_instruction_arcache"
      , PortName "axi_instruction_arprot"
      , PortName "axi_instruction_arqos"
      , PortName "axi_instruction_arregion"
      , PortName "axi_instruction_aruser"
      , PortName "axi_instruction_arvalid"
      , PortName "axi_instruction_rready"
      , PortName "axi_data_arid"
      , PortName "axi_data_araddr"
      , PortName "axi_data_arlen"
      , PortName "axi_data_arsize"
      , PortName "axi_data_arburst"
      , PortName "axi_data_arlock"
      , PortName "axi_data_arcache"
      , PortName "axi_data_arprot"
      , PortName "axi_data_arqos"
      , PortName "axi_data_arregion"
      , PortName "axi_data_aruser"
      , PortName "axi_data_arvalid"
      , PortName "axi_data_rready"
      , PortName "axi_data_awid"
      , PortName "axi_data_awaddr"
      , PortName "axi_data_awlen"
      , PortName "axi_data_awsize"
      , PortName "axi_data_awburst"
      , PortName "axi_data_awlock"
      , PortName "axi_data_awcache"
      , PortName "axi_data_awprot"
      , PortName "axi_data_awqos"
      , PortName "axi_data_awregion"
      , PortName "axi_data_awuser"
      , PortName "axi_data_awvalid"
      , PortName "axi_data_wdata"
      , PortName "axi_data_wstrb"
      , PortName "axi_data_wlast"
      , PortName "axi_data_wuser"
      , PortName "axi_data_wvalid"
      , PortName "axi_data_bready"
      ]
    }) #-}
topEntity
  :: Clock System
  -> Reset System
  -> Signal System Bit
  -> Signal System (BitVector 1)
  -> Signal System U32
  -> Signal System (BitVector 2)
  -> Signal System Bit
  -> Signal System (BitVector 1)
  -> Signal System Bit
  -> Signal System Bit
  -> Signal System (BitVector 1)
  -> Signal System U32
  -> Signal System (BitVector 2)
  -> Signal System Bit
  -> Signal System (BitVector 1)
  -> Signal System Bit
  -> Signal System Bit
  -> Signal System Bit
  -> Signal System (BitVector 1)
  -> Signal System (BitVector 2)
  -> Signal System (BitVector 1)
  -> Signal System Bit
  -> Signal System Bit
  -> Signal System AxiOutputs
topEntity clk rst
  axi_instruction_arready
  axi_instruction_rid
  axi_instruction_rdata
  axi_instruction_rresp
  axi_instruction_rlast
  axi_instruction_ruser
  axi_instruction_rvalid
  axi_data_arready
  axi_data_rid
  axi_data_rdata
  axi_data_rresp
  axi_data_rlast
  axi_data_ruser
  axi_data_rvalid
  axi_data_awready
  axi_data_wready
  axi_data_bid
  axi_data_bresp
  axi_data_buser
  axi_data_bvalid
  verbose =
    exposeClockResetEnable
      (mealy step initialState inputs)
      clk
      rst
      enableGen
  where
    inputs =
      AxiInputs
        <$> axi_instruction_arready
        <*> axi_instruction_rid
        <*> axi_instruction_rdata
        <*> axi_instruction_rresp
        <*> axi_instruction_rlast
        <*> axi_instruction_ruser
        <*> axi_instruction_rvalid
        <*> axi_data_arready
        <*> axi_data_rid
        <*> axi_data_rdata
        <*> axi_data_rresp
        <*> axi_data_rlast
        <*> axi_data_ruser
        <*> axi_data_rvalid
        <*> axi_data_awready
        <*> axi_data_wready
        <*> axi_data_bid
        <*> axi_data_bresp
        <*> axi_data_buser
        <*> axi_data_bvalid
        <*> verbose
