#!/usr/bin/env python3
"""Generate qrisc32 sort smoke vectors from the original SV test programs."""

from pathlib import Path


LDR = 0
STR = 1
JMPUNC = 2
JMPF = 3
ALU = 4
LDRF = 5

OFFSET_CODE = 0
INCR_0 = 0
DECR_0 = 0

AND = 0
OR = 1
XOR = 2
ADD = 3
MUL = 4
SHL = 5
SHR = 6
CMP = 7

JMP = 0
JMPZ = 0
JMPNZ = 1
JMPC = 2
JMPNC = 3

LDRZ = 0
LDRNZ = 1
LDRC = 2
LDRNC = 3

R0 = 0
R1 = 1
R2 = 2
R3 = 3
R4 = 4
R5 = 5
R6 = 6
R7 = 7
R8 = 8
R9 = 9
R10 = 10
R11 = 11
R12 = 12
R13 = 13
R14 = 14
R15 = 15
R16 = 16
R17 = 17
R18 = 18
R19 = 19
R20 = 20
R21 = 21
R22 = 22
R23 = 23


def mask(value: int, bits: int) -> int:
    return value & ((1 << bits) - 1)


def ldrh(imm: int, dst: int) -> int:
    return (LDR << 28) | (1 << 26) | (mask(imm, 16) << 5) | dst


def ldrl(imm: int, dst: int) -> int:
    return (LDR << 28) | (2 << 26) | (mask(imm, 16) << 5) | dst


def ldrp(off15: int, base: int, dst: int) -> int:
    return (LDR << 28) | (3 << 26) | (OFFSET_CODE << 25) | (mask(off15, 15) << 10) | (base << 5) | dst


def ldrr(inc: int, src2: int, src1: int, dst: int) -> int:
    return (LDR << 28) | (0 << 26) | (0 << 25) | (inc << 22) | (src2 << 10) | (src1 << 5) | dst


def strp(off15: int, base: int, dst: int) -> int:
    return (STR << 28) | (3 << 26) | (OFFSET_CODE << 25) | (mask(off15, 15) << 10) | (base << 5) | dst


def alu(subop: int, inc: int, src2: int, src1: int, dst: int) -> int:
    return (ALU << 28) | (subop << 25) | (inc << 22) | (src2 << 10) | (src1 << 5) | dst


def ldrf(mode: int, inc: int, src2: int, src1: int, dst: int) -> int:
    return (LDRF << 28) | (mode << 26) | (inc << 22) | (src2 << 10) | (src1 << 5) | dst


def jmp_abs(addr: int) -> int:
    return (JMPUNC << 28) | (JMP << 26) | mask(addr, 26)


def jmp_flag(mode: int, off15: int) -> int:
    return (JMPF << 28) | (mode << 26) | (OFFSET_CODE << 25) | (mask(off15, 15) << 10)


def bubble_sort() -> list[int]:
    code: list[int] = []

    def emit(inst: int) -> None:
        code.append(inst)

    emit(ldrh(0x0000, R12))
    emit(alu(XOR, DECR_0, R13, R13, R13))
    emit(ldrh(0xFFFF, R14))
    emit(ldrl(0x0024, R12))

    emit(ldrp(0, R13, R0))
    emit(ldrp(4, R13, R1))
    emit(ldrp(8, R13, R2))
    emit(ldrp(12, R13, R3))
    emit(ldrp(16, R13, R4))
    emit(ldrp(20, R13, R5))
    emit(ldrp(24, R13, R6))
    emit(ldrp(28, R13, R7))
    emit(ldrp(32, R13, R8))
    emit(ldrp(36, R13, R9))

    emit(ldrl(-4, R14))

    emit(alu(CMP, INCR_0, R0, R1, R1))
    emit(ldrf(LDRC, INCR_0, R0, R1, R16))

    emit(alu(CMP, INCR_0, R3, R2, R2))
    emit(ldrf(LDRC, INCR_0, R3, R2, R17))

    emit(alu(CMP, INCR_0, R5, R4, R4))
    emit(ldrf(LDRC, INCR_0, R5, R4, R18))

    emit(alu(CMP, INCR_0, R7, R6, R6))
    emit(ldrf(LDRC, INCR_0, R7, R6, R19))

    emit(alu(CMP, INCR_0, R9, R8, R8))
    emit(ldrf(LDRC, INCR_0, R9, R8, R20))

    emit(alu(CMP, INCR_0, R17, R16, R16))
    emit(ldrf(LDRC, INCR_0, R17, R16, R21))

    emit(alu(CMP, INCR_0, R19, R18, R18))
    emit(ldrf(LDRC, INCR_0, R19, R18, R22))

    emit(alu(CMP, INCR_0, R21, R20, R20))
    emit(ldrf(LDRC, INCR_0, R21, R20, R23))
    emit(0)

    emit(alu(CMP, INCR_0, R23, R22, R22))
    emit(ldrf(LDRC, INCR_0, R23, R22, R11))

    emit(0)
    emit(strp(0, R13, R11))

    emit(alu(CMP, INCR_0, R0, R1, R1))
    emit(ldrf(LDRNC, INCR_0, R0, R1, R16))

    emit(alu(CMP, INCR_0, R3, R2, R2))
    jmpr_label0 = 4 * len(code)
    emit(ldrf(LDRNC, INCR_0, R3, R2, R17))

    emit(alu(CMP, INCR_0, R5, R4, R4))
    emit(ldrf(LDRNC, INCR_0, R5, R4, R18))

    emit(alu(CMP, INCR_0, R7, R6, R6))
    emit(ldrf(LDRNC, INCR_0, R7, R6, R19))

    emit(alu(CMP, INCR_0, R9, R8, R8))
    emit(ldrf(LDRNC, INCR_0, R9, R8, R20))

    emit(alu(CMP, INCR_0, R17, R16, R16))
    emit(ldrf(LDRNC, INCR_0, R17, R16, R21))

    emit(alu(CMP, INCR_0, R19, R18, R18))
    emit(ldrf(LDRNC, INCR_0, R19, R18, R22))

    emit(alu(CMP, INCR_0, R21, R20, R20))
    emit(ldrf(LDRNC, INCR_0, R21, R20, R23))
    emit(0)

    emit(alu(CMP, INCR_0, R23, R22, R22))
    emit(ldrf(LDRNC, INCR_0, R23, R22, R10))
    emit(0)

    emit(strp(0, R12, R10))
    emit(alu(ADD, DECR_0, R14, R12, R12))

    for reg in (R0, R1, R2, R3, R4, R5, R6, R7, R8, R9):
        emit(alu(CMP, INCR_0, reg, R10, R10))
        emit(ldrf(LDRZ, INCR_0, reg, R11, reg))

    emit(alu(CMP, INCR_0, R13, R12, R12))
    offset = jmpr_label0 - 4 * len(code) - 8
    emit(jmp_flag(JMPNZ, offset))
    emit(ldrr(DECR_0, R11, R11, R10))

    emit(alu(CMP, INCR_0, R0, R1, R1))
    emit(ldrf(LDRNC, INCR_0, R0, R1, R16))
    emit(alu(CMP, INCR_0, R3, R2, R2))
    return code


def shell_sort_modified() -> list[int]:
    code: list[int] = []

    def emit(inst: int) -> None:
        code.append(inst)

    emit(ldrh(0x0000, R5))
    emit(ldrh(0x0000, R8))
    emit(ldrh(0x0000, R9))
    emit(ldrh(0x0000, R10))
    emit(ldrh(0x0000, R12))
    emit(ldrh(0xFFFF, R13))
    emit(ldrh(0x0000, R14))

    emit(ldrl(0x0005, R5))
    emit(ldrl(0x0001, R8))
    emit(ldrl(0x0002, R9))
    emit(ldrl(0x0004, R10))
    emit(ldrl(0x0028, R12))
    emit(ldrl(0xFFFF, R13))
    emit(ldrl(0x0003, R14))

    jmpr_label0 = 4 * len(code)
    emit(alu(SHL, INCR_0, R9, R5, R5))

    emit(jmp_flag(JMPZ, 1000 * 4))

    emit(alu(XOR, DECR_0, R13, R5, R6))
    emit(ldrr(DECR_0, R5, R5, R4))
    emit(alu(ADD, DECR_0, R8, R6, R6))
    emit(alu(SHR, INCR_0, R14, R5, R5))

    emit(alu(CMP, INCR_0, R12, R4, R4))
    ilabel = 4 * len(code)

    offset = jmpr_label0 - 4 * len(code) - 8
    emit(jmp_flag(JMPZ, offset))

    emit(ldrp(0, R4, R1))

    emit(ldrr(DECR_0, R4, R4, R3))
    emit(alu(ADD, DECR_0, R4, R6, R7))

    emit(alu(CMP, INCR_0, R5, R3, R3))
    jlabel = 4 * len(code)
    emit(ldrp(0, R7, R0))

    emit(jmp_flag(JMPC, 13 * 4))
    emit(0)
    emit(0)
    emit(0)

    emit(alu(CMP, INCR_0, R1, R0, R0))

    emit(jmp_flag(JMPC, 8 * 4))
    emit(0)
    emit(0)
    emit(0)
    emit(0)

    emit(jmp_abs(jlabel))
    emit(ldrr(DECR_0, R7, R7, R3))
    emit(strp(0, R3, R0))
    emit(alu(ADD, DECR_0, R3, R6, R7))
    emit(alu(CMP, INCR_0, R5, R3, R3))

    emit(jmp_abs(ilabel))
    emit(alu(ADD, DECR_0, R10, R4, R4))
    emit(strp(0, R3, R1))
    emit(alu(CMP, INCR_0, R12, R4, R4))
    emit(0)
    return code


def shell_sort_first() -> list[int]:
    code: list[int] = []

    def emit(inst: int) -> None:
        code.append(inst)

    emit(ldrh(0x0000, R10))
    emit(ldrh(0xFFFF, R11))
    emit(ldrh(0x0000, R12))
    emit(jmp_abs(4 * len(code) + 4 * 8))

    emit(alu(XOR, INCR_0, R4, R4, R4))
    emit(ldrl(0x0004, R10))
    emit(ldrl(0xFFFC, R11))
    emit(ldrl(0x0024, R12))

    jmpr_label0 = 4 * len(code)
    emit(jmp_flag(JMPZ, 4 * len(code) + 32 * 8))
    emit(ldrr(DECR_0, R5, R4, R7))
    emit(strp(4, R7, R0))

    emit(ldrp(0, R4, R15))

    emit(alu(XOR, DECR_0, R7, R11, R2))
    emit(ldrp(4, R4, R0))

    jmp_label1 = 4 * len(code)
    offset = jmpr_label0 - 4 * len(code) - 8
    emit(jmp_flag(JMPZ, offset))
    emit(alu(ADD, DECR_0, R10, R4, R4))
    emit(ldrr(DECR_0, R15, R15, R1))
    emit(alu(XOR, INCR_0, R12, R4, R2))
    emit(ldrp(-4, R7, R15))

    emit(alu(CMP, INCR_0, R0, R1, R2))
    offset = jmpr_label0 - 4 * len(code) - 8
    emit(jmp_flag(JMPC, offset))
    emit(0)
    emit(0)
    emit(0)
    emit(alu(XOR, INCR_0, R12, R4, R2))

    emit(jmp_abs(jmp_label1))
    emit(alu(ADD, DECR_0, R7, R11, R7))
    emit(alu(ADD, DECR_0, R11, R4, R4))
    emit(strp(8, R7, R1))
    emit(alu(XOR, DECR_0, R7, R11, R2))
    return code


def write_hex(path: Path, words: list[int]) -> None:
    path.write_text("".join(f"{word & 0xFFFF_FFFF:08x}\n" for word in words))


def main() -> int:
    repo = Path(__file__).resolve().parents[1]
    tests = repo / "tests"
    write_hex(tests / "smoke_bubble.hex", bubble_sort())
    write_hex(tests / "smoke_shell_mdf.hex", shell_sort_modified())
    write_hex(tests / "smoke_shell_first.hex", shell_sort_first())
    write_hex(tests / "smoke.hex", bubble_sort())
    write_hex(tests / "smoke_reversed.hex", list(range(10, 0, -1)))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
