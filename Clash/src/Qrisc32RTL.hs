{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Qrisc32RTL where

import Clash.Prelude
import GHC.Generics (Generic)

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
  | DataReadAddr
  | DataReadResp
  | DataWriteAddrData Bool Bool
  | DataWriteResp
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
  , memAddr :: U32
  , memWData :: U32
  , pending :: Pending
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
    , memAddr = 0
    , memWData = 0
    , pending = NoPending
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

shiftAmount :: U32 -> Int
shiftAmount x = fromIntegral (unpack (slice d4 d0 x :: BitVector 5) :: Unsigned 5)

aluAdd :: U32 -> U32 -> (U32, Bool)
aluAdd a b =
  let wide = (zeroExtend a :: BitVector 33) + (zeroExtend b :: BitVector 33)
  in (truncateB wide, msb wide == 1)

aluShr :: U32 -> U32 -> (U32, Bool)
aluShr a b =
  let shifted = ((a ++# (0 :: BitVector 1)) :: BitVector 33) `shiftR` shiftAmount b
  in (slice d32 d1 shifted, shifted ! 0 == 1)

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
      useRegOffset = inst ! 25 == 1
      incCode = slice d24 d22 inst :: BitVector 3
      incDelta = incrWord incCode
      defaultIncEn = incrEnabled incCode
      offset = if useRegOffset then r2 else sext15 off15
      seqPc = pc + 4
      normal rs' z' c' = s { pc = seqPc, regs = rs', flagZ = z', flagC = c', phase = FetchAddr, pending = NoPending }
      normalKeepFlags rs' = normal rs' flagZ flagC
      writeMaybe doWrite value incEn =
        let rs1 = if doWrite
                    then commitWriteThenInc dst value incEn src2 r2 incDelta regs
                    else commitIncOnly incEn src2 r2 incDelta regs
        in normalKeepFlags rs1
      jumpState target incEn writeRet =
        let rs1 = if writeRet
                    then commitWriteThenInc dst pc incEn src2 r2 incDelta regs
                    else commitIncOnly incEn src2 r2 incDelta regs
        in s { pc = target, regs = rs1, phase = FetchAddr, pending = NoPending }
      branch cond =
        let target = pc + offset
            rs1 = commitIncOnly useRegOffset src2 r2 incDelta regs
        in s { pc = if cond then target else seqPc, regs = rs1, phase = FetchAddr, pending = NoPending }
      ldrf cond =
        let value = if cond then r1 else r2
            rs1 = commitWriteThenInc dst value defaultIncEn src2 r2 incDelta regs
        in normalKeepFlags rs1
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
                rs1 = commitWriteThenInc dst value defaultIncEn src2 r2 incDelta regs
            in normal rs1 (value == 0) False
          1 ->
            let value = r1 .|. r2
                rs1 = commitWriteThenInc dst value defaultIncEn src2 r2 incDelta regs
            in normal rs1 (value == 0) False
          2 ->
            let value = r1 `xor` r2
                rs1 = commitWriteThenInc dst value defaultIncEn src2 r2 incDelta regs
            in normal rs1 (value == 0) False
          3 ->
            let (value, carry) = aluAdd r1 r2
                rs1 = commitWriteThenInc dst value defaultIncEn src2 r2 incDelta regs
            in normal rs1 (value == 0) carry
          4 ->
            let value = r1 * r2
                rs1 = commitWriteThenInc dst value defaultIncEn src2 r2 incDelta regs
            in normal rs1 (value == 0) False
          5 ->
            let value = r1 `shiftL` shiftAmount r2
                rs1 = commitWriteThenInc dst value defaultIncEn src2 r2 incDelta regs
            in normal rs1 (value == 0) False
          6 ->
            let (value, carry) = aluShr r1 r2
                rs1 = commitWriteThenInc dst value defaultIncEn src2 r2 incDelta regs
            in normal rs1 (value == 0) carry
          _ ->
            let rs1 = commitIncOnly defaultIncEn src2 r2 incDelta regs
            in normal rs1 (r1 == r2) (r1 < r2)
      0x5 ->
        case mode of
          0 -> ldrf flagZ
          1 -> ldrf (not flagZ)
          2 -> ldrf flagC
          _ -> ldrf (not flagC)
      _ -> s { pc = seqPc, phase = FetchAddr, pending = NoPending }

commitLoad :: CpuState -> U32 -> CpuState
commitLoad s@CpuState{..} value =
  case pending of
    LoadPending dst incEn incReg incBase incDelta ->
      s { regs = commitWriteThenInc dst value incEn incReg incBase incDelta regs
        , phase = FetchAddr
        , pending = NoPending
        }
    _ -> s { phase = FetchAddr, pending = NoPending }

commitStore :: CpuState -> CpuState
commitStore s@CpuState{..} =
  case pending of
    StorePending incEn incReg incBase incDelta ->
      s { regs = commitIncOnly incEn incReg incBase incDelta regs
        , phase = FetchAddr
        , pending = NoPending
        }
    _ -> s { phase = FetchAddr, pending = NoPending }

step :: CpuState -> AxiInputs -> (CpuState, AxiOutputs)
step s@CpuState{..} i =
  case phase of
    FetchAddr ->
      let out = idleOutputs { axi_instruction_araddr = pc, axi_instruction_arvalid = 1 }
          s' = if bitSet (iArReady i) then s { phase = FetchResp } else s
      in (s', out)
    FetchResp ->
      let s' = if bitSet (iRValid i) then decodeExecute s (iRData i) else s
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

{-# ANN topEntity
  (Synthesize
    { t_name = "qrisc32_clash"
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
