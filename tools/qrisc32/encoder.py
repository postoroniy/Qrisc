"""Bit-exact qrisc32 instruction encoders shared by repo tools."""

from __future__ import annotations

from pathlib import Path

from .isa import *  # re-export ISA names for existing tool users


def mask(value: int, bits: int) -> int:
    return value & ((1 << bits) - 1)


def check_range(name: str, value: int, bits: int, signed: bool = False) -> None:
    if signed:
        lo = -(1 << (bits - 1))
        hi = (1 << (bits - 1)) - 1
    else:
        lo = 0
        hi = (1 << bits) - 1
    if not lo <= value <= hi:
        raise ValueError(f"{name}={value} does not fit in {'signed ' if signed else ''}{bits} bits")


def check_reg(reg: int) -> None:
    check_range("register", reg, 5)


def check_word(name: str, value: int, bits: int) -> None:
    if not -(1 << (bits - 1)) <= value <= (1 << bits) - 1:
        raise ValueError(f"{name}={value} does not fit in {bits} bits")


def ldrh(imm: int, dst: int) -> int:
    check_reg(dst)
    check_word("immediate", imm, 16)
    return (LDR << 28) | (LoadMode.HIGH << 26) | (mask(imm, 16) << 5) | dst


def ldrl(imm: int, dst: int) -> int:
    check_reg(dst)
    check_word("immediate", imm, 16)
    return (LDR << 28) | (LoadMode.LOW << 26) | (mask(imm, 16) << 5) | dst


def ldrr(inc: int, src2: int, src1: int, dst: int) -> int:
    for reg in (src2, src1, dst):
        check_reg(reg)
    check_range("inc", inc, 3)
    return (LDR << 28) | (inc << 22) | (src2 << 10) | (src1 << 5) | dst


def ldrp(offset: int, base: int, dst: int, offset_is_reg: bool = False) -> int:
    check_reg(base)
    check_reg(dst)
    if offset_is_reg:
        check_reg(offset)
        return (LDR << 28) | (LoadMode.MEM << 26) | (OFFSET_R << 25) | (offset << 10) | (base << 5) | dst
    check_range("offset", offset, 15, signed=True)
    return (LDR << 28) | (LoadMode.MEM << 26) | (OFFSET_CODE << 25) | (mask(offset, 15) << 10) | (base << 5) | dst


def strp(offset: int, base: int, src: int, offset_is_reg: bool = False) -> int:
    check_reg(base)
    check_reg(src)
    if offset_is_reg:
        check_reg(offset)
        return (STR << 28) | (LoadMode.MEM << 26) | (OFFSET_R << 25) | (offset << 10) | (base << 5) | src
    check_range("offset", offset, 15, signed=True)
    return (STR << 28) | (LoadMode.MEM << 26) | (OFFSET_CODE << 25) | (mask(offset, 15) << 10) | (base << 5) | src


def alu(subop: int, inc: int, src2: int, src1: int, dst: int) -> int:
    for reg in (src2, src1, dst):
        check_reg(reg)
    check_range("subop", subop, 3)
    check_range("inc", inc, 3)
    return (ALU << 28) | (subop << 25) | (inc << 22) | (src2 << 10) | (src1 << 5) | dst


def ldrf(mode: int, inc: int, src2: int, src1: int, dst: int) -> int:
    for reg in (src2, src1, dst):
        check_reg(reg)
    check_range("mode", mode, 2)
    check_range("inc", inc, 3)
    return (LDRF << 28) | (mode << 26) | (inc << 22) | (src2 << 10) | (src1 << 5) | dst


def jmp_abs(addr: int) -> int:
    check_range("address", addr, 26)
    if addr & 3:
        raise ValueError(f"address={addr} is not word-aligned")
    return (JMPUNC << 28) | (JMP << 26) | mask(addr, 26)


def jmp_flag(mode: int, off15: int) -> int:
    check_range("mode", mode, 2)
    check_range("offset", off15, 15, signed=True)
    if off15 & 3:
        raise ValueError(f"offset={off15} is not word-aligned")
    return (JMPF << 28) | (mode << 26) | (OFFSET_CODE << 25) | (mask(off15, 15) << 10)


def load_imm_words(dst: int, value: int) -> list[int]:
    check_word("value", value, 32)
    value = mask(value, 32)
    return [ldrh((value >> 16) & 0xFFFF, dst), ldrl(value & 0xFFFF, dst)]


def write_hex(path: Path, words: list[int]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    content = "".join(f"{word & 0xFFFF_FFFF:08x}\n" for word in words)
    if not path.exists() or path.read_text() != content:
        path.write_text(content)
