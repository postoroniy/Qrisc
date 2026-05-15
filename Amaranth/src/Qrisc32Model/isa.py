from __future__ import annotations

from enum import IntEnum


class Opcode(IntEnum):
    LDR = 0
    STR = 1
    JMPUNC = 2
    JMPF = 3
    ALU = 4
    LDRF = 5
    INVALID = 15


class LoadMode(IntEnum):
    REG = 0
    HIGH = 1
    LOW = 2
    MEM = 3


class JumpMode(IntEnum):
    ABS = 0
    REL = 1
    CALL = 2
    RET = 3


class FlagMode(IntEnum):
    Z = 0
    NZ = 1
    C = 2
    NC = 3


class AluOp(IntEnum):
    AND = 0
    OR = 1
    XOR = 2
    ADD = 3
    MUL = 4
    SHL = 5
    SHR = 6
    CMP = 7


class IncrMode(IntEnum):
    NONE = 0
    ONE = 1
    TWO = 2
    FOUR = 3
    NONE_ALT = 4
    NEG_ONE = 5
    NEG_TWO = 6
    NEG_FOUR = 7


def opcode_from_bits(value: int) -> Opcode:
    try:
        return Opcode(value)
    except ValueError:
        return Opcode.INVALID
