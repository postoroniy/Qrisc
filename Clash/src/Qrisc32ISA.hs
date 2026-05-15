{-# LANGUAGE DeriveGeneric #-}

module Qrisc32ISA
  ( Opcode(..)
  , LoadMode(..)
  , JumpMode(..)
  , FlagMode(..)
  , AluOp(..)
  , IncrMode(..)
  , OpcodeField(..)
  , LoadModeField(..)
  , JumpModeField(..)
  , FlagModeField(..)
  , AluOpField(..)
  , IncrModeField(..)
  , opcodeFromInt
  , loadModeFromInt
  , jumpModeFromInt
  , flagModeFromInt
  , aluOpFromInt
  , incrModeFromInt
  , incrValue
  , incrEnabled
  , flagCondition
  ) where

import GHC.Generics (Generic)
import Prelude (Bool(..), Bounded, Enum, Eq, Int, Num, Show, not, toEnum)

data Opcode
  = OpLdr | OpStr | OpJmpUnc | OpJmpF | OpAlu | OpLdrF
  | OpReserved6 | OpReserved7 | OpReserved8 | OpReserved9
  | OpReserved10 | OpReserved11 | OpReserved12 | OpReserved13
  | OpReserved14 | OpReserved15
  deriving (Generic, Eq, Show, Enum, Bounded)

data LoadMode = LoadReg | LoadHigh | LoadLow | LoadMem
  deriving (Generic, Eq, Show, Enum, Bounded)

data JumpMode = JumpAbs | JumpRel | JumpCall | JumpRet
  deriving (Generic, Eq, Show, Enum, Bounded)

data FlagMode = FlagZMode | FlagNzMode | FlagCMode | FlagNcMode
  deriving (Generic, Eq, Show, Enum, Bounded)

data AluOp = AluAnd | AluOr | AluXor | AluAdd | AluMul | AluShl | AluShr | AluCmp
  deriving (Generic, Eq, Show, Enum, Bounded)

data IncrMode = IncrNone | IncrOne | IncrTwo | IncrFour | IncrNoneAlt | DecrOne | DecrTwo | DecrFour
  deriving (Generic, Eq, Show, Enum, Bounded)

class OpcodeField a where
  opcodeOf :: a -> Opcode

class LoadModeField a where
  loadModeOf :: a -> LoadMode

class JumpModeField a where
  jumpModeOf :: a -> JumpMode

class FlagModeField a where
  flagModeOf :: a -> FlagMode

class AluOpField a where
  aluOpOf :: a -> AluOp

class IncrModeField a where
  incrModeOf :: a -> IncrMode

instance OpcodeField Int where
  opcodeOf = opcodeFromInt

instance LoadModeField Int where
  loadModeOf = loadModeFromInt

instance JumpModeField Int where
  jumpModeOf = jumpModeFromInt

instance FlagModeField Int where
  flagModeOf = flagModeFromInt

instance AluOpField Int where
  aluOpOf = aluOpFromInt

instance IncrModeField Int where
  incrModeOf = incrModeFromInt

opcodeFromInt :: Int -> Opcode
opcodeFromInt = toEnum

loadModeFromInt :: Int -> LoadMode
loadModeFromInt = toEnum

jumpModeFromInt :: Int -> JumpMode
jumpModeFromInt = toEnum

flagModeFromInt :: Int -> FlagMode
flagModeFromInt = toEnum

aluOpFromInt :: Int -> AluOp
aluOpFromInt = toEnum

incrModeFromInt :: Int -> IncrMode
incrModeFromInt = toEnum

incrValue :: Num a => IncrMode -> (a, Bool)
incrValue IncrOne = (1, True)
incrValue IncrTwo = (2, True)
incrValue IncrFour = (4, True)
incrValue DecrOne = (-1, True)
incrValue DecrTwo = (-2, True)
incrValue DecrFour = (-4, True)
incrValue _ = (0, False)

incrEnabled :: IncrMode -> Bool
incrEnabled IncrOne = True
incrEnabled IncrTwo = True
incrEnabled IncrFour = True
incrEnabled DecrOne = True
incrEnabled DecrTwo = True
incrEnabled DecrFour = True
incrEnabled _ = False

flagCondition :: FlagMode -> Bool -> Bool -> Bool
flagCondition FlagZMode z _ = z
flagCondition FlagNzMode z _ = not z
flagCondition FlagCMode _ c = c
flagCondition FlagNcMode _ c = not c
