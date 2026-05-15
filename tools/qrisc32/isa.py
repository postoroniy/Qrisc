"""Shared qrisc32 ISA encoding enums and register aliases."""

from __future__ import annotations

from enum import IntEnum


class Opcode(IntEnum):
    LDR = 0
    STR = 1
    JMPUNC = 2
    JMPF = 3
    ALU = 4
    LDRF = 5


class LoadMode(IntEnum):
    REG = 0
    HIGH = 1
    LOW = 2
    MEM = 3


class OffsetMode(IntEnum):
    CODE = 0
    REG = 1


class IncrMode(IntEnum):
    INCR_0 = 0
    DECR_0 = 0
    INCR_1 = 1
    INCR_2 = 2
    INCR_4 = 3
    DECR_1 = 5
    DECR_2 = 6
    DECR_4 = 7


class AluOp(IntEnum):
    AND = 0
    OR = 1
    XOR = 2
    ADD = 3
    MUL = 4
    SHL = 5
    SHR = 6
    CMP = 7


class JumpOp(IntEnum):
    JMP = 0
    JMPR = 1
    CALL = 2
    RET = 3


class FlagMode(IntEnum):
    Z = 0
    NZ = 1
    C = 2
    NC = 3


LDR = Opcode.LDR
STR = Opcode.STR
JMPUNC = Opcode.JMPUNC
JMPF = Opcode.JMPF
ALU = Opcode.ALU
LDRF = Opcode.LDRF

OFFSET_CODE = OffsetMode.CODE
OFFSET_R = OffsetMode.REG

INCR_0 = IncrMode.INCR_0
DECR_0 = IncrMode.DECR_0
INCR_1 = IncrMode.INCR_1
INCR_2 = IncrMode.INCR_2
INCR_4 = IncrMode.INCR_4
DECR_1 = IncrMode.DECR_1
DECR_2 = IncrMode.DECR_2
DECR_4 = IncrMode.DECR_4

AND = AluOp.AND
OR = AluOp.OR
XOR = AluOp.XOR
ADD = AluOp.ADD
MUL = AluOp.MUL
SHL = AluOp.SHL
SHR = AluOp.SHR
CMP = AluOp.CMP

JMP = JumpOp.JMP
JMPR = JumpOp.JMPR
CALL = JumpOp.CALL
RET = JumpOp.RET

JMPZ = FlagMode.Z
JMPNZ = FlagMode.NZ
JMPC = FlagMode.C
JMPNC = FlagMode.NC

LDRZ = FlagMode.Z
LDRNZ = FlagMode.NZ
LDRC = FlagMode.C
LDRNC = FlagMode.NC

REGISTERS = {f"r{i}": i for i in range(32)}
globals().update({f"R{i}": i for i in range(32)})
