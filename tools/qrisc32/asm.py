"""qrisc32 repo-native assembler."""

from __future__ import annotations

import argparse
import re
from dataclasses import dataclass
from pathlib import Path

from . import encoder as enc


class AsmError(ValueError):
    pass


@dataclass(frozen=True)
class SourceLine:
    path: Path
    lineno: int
    text: str


@dataclass
class Program:
    imem: list[int]
    dmem: list[int]


ALU_OPS = {
    "AND": enc.AND,
    "OR": enc.OR,
    "XOR": enc.XOR,
    "ADD": enc.ADD,
    "MUL": enc.MUL,
    "SHL": enc.SHL,
    "SHR": enc.SHR,
    "CMP": enc.CMP,
}

LDRF_OPS = {
    "LDRZ": enc.LDRZ,
    "LDRNZ": enc.LDRNZ,
    "LDRC": enc.LDRC,
    "LDRNC": enc.LDRNC,
}

JMPF_OPS = {
    "JMPZ": enc.JMPZ,
    "JMPNZ": enc.JMPNZ,
    "JMPC": enc.JMPC,
    "JMPNC": enc.JMPNC,
}


def strip_comment(line: str) -> str:
    in_string = False
    out: list[str] = []
    for ch in line:
        if ch == '"':
            in_string = not in_string
        if ch == "#" and not in_string:
            break
        out.append(ch)
    return "".join(out).strip()


def parse_int(text: str) -> int:
    text = text.strip().replace("_", "")
    if not text:
        raise AsmError("empty integer")
    return int(text, 0)


def split_operands(text: str) -> list[str]:
    items: list[str] = []
    depth = 0
    start = 0
    for idx, ch in enumerate(text):
        if ch == "[":
            depth += 1
        elif ch == "]":
            depth -= 1
            if depth < 0:
                raise AsmError("unmatched closing bracket")
        elif ch == "," and depth == 0:
            item = text[start:idx].strip()
            if not item:
                raise AsmError("empty operand")
            items.append(item)
            start = idx + 1
    if depth:
        raise AsmError("unmatched opening bracket")
    tail = text[start:].strip()
    if tail:
        items.append(tail)
    elif items:
        raise AsmError("trailing comma")
    return items


def parse_reg(text: str) -> int:
    name = text.strip().lower()
    if name not in enc.REGISTERS:
        raise AsmError(f"expected register r0..r31, got {text!r}")
    return enc.REGISTERS[name]


def parse_addr(text: str) -> tuple[int, int, bool]:
    match = re.fullmatch(r"\[\s*([A-Za-z][A-Za-z0-9_]*)\s*(?:([+-])\s*([^\]]+))?\s*\]", text.strip())
    if not match:
        raise AsmError(f"expected address like [r0 + 4] or [r0 + r1], got {text!r}")
    base = parse_reg(match.group(1))
    sign = match.group(2) or "+"
    rhs = (match.group(3) or "0").strip()
    if rhs.lower() in enc.REGISTERS:
        if sign == "-":
            raise AsmError("register memory offsets cannot be negated")
        return base, parse_reg(rhs), True
    value = parse_int(rhs)
    if sign == "-":
        value = -value
    return base, value, False


def load_lines(path: Path, seen: tuple[Path, ...] = ()) -> list[SourceLine]:
    path = path.resolve()
    if path in seen:
        chain = " -> ".join(str(p) for p in (*seen, path))
        raise AsmError(f"recursive include: {chain}")
    lines: list[SourceLine] = []
    try:
        source = path.read_text()
    except OSError as exc:
        raise AsmError(f"cannot read {path}: {exc.strerror}") from exc
    for lineno, raw in enumerate(source.splitlines(), 1):
        text = strip_comment(raw)
        if not text:
            continue
        if text.lower().startswith(".include"):
            match = re.fullmatch(r'\.include\s+"([^"]+)"', text, re.IGNORECASE)
            if not match:
                raise AsmError(f"{path}:{lineno}: bad .include syntax")
            try:
                lines.extend(load_lines(path.parent / match.group(1), (*seen, path)))
            except AsmError as exc:
                raise AsmError(f"{path}:{lineno}: {exc}") from exc
        else:
            lines.append(SourceLine(path, lineno, text))
    return lines


def extract_labels(lines: list[SourceLine]) -> tuple[dict[str, int], list[SourceLine]]:
    labels: dict[str, int] = {}
    items: list[SourceLine] = []
    pc_words = 0
    for line in lines:
        text = line.text
        while True:
            match = re.match(r"^([A-Za-z_][A-Za-z0-9_]*):\s*(.*)$", text)
            if not match:
                break
            name = match.group(1)
            if name in labels:
                raise AsmError(f"{line.path}:{line.lineno}: duplicate label {name}")
            labels[name] = pc_words * 4
            text = match.group(2).strip()
            if not text:
                break
        if text:
            items.append(SourceLine(line.path, line.lineno, text))
            if text.split(None, 1)[0].lower() != ".dmem":
                pc_words += 1
    return labels, items


def resolve_value(token: str, labels: dict[str, int]) -> int:
    token = token.strip()
    if token in labels:
        return labels[token]
    try:
        return parse_int(token)
    except ValueError as exc:
        if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", token):
            raise AsmError(f"undefined label {token}") from exc
        raise AsmError(f"invalid integer {token!r}") from exc


def encode_item(line: SourceLine, labels: dict[str, int], pc_words: int, dmem: list[int]) -> int | None:
    parts = line.text.split(None, 1)
    mnemonic = parts[0].upper()
    operands = split_operands(parts[1] if len(parts) > 1 else "")

    try:
        if mnemonic == ".WORD":
            if len(operands) != 1:
                raise AsmError(".word takes exactly one operand")
            value = resolve_value(operands[0], labels)
            enc.check_word(".word", value, 32)
            return value

        if mnemonic == ".DMEM":
            if not operands:
                raise AsmError(".dmem takes at least one value")
            values = [resolve_value(item, labels) for item in operands]
            for value in values:
                enc.check_word(".dmem", value, 32)
            dmem.extend(value & 0xFFFF_FFFF for value in values)
            return None

        if mnemonic == "NOP":
            if operands:
                raise AsmError("NOP takes no operands")
            return 0

        if mnemonic in ("LDRH", "LDRL"):
            if len(operands) != 2:
                raise AsmError(f"{mnemonic} takes dst, imm")
            dst = parse_reg(operands[0])
            imm = resolve_value(operands[1], labels)
            return enc.ldrh(imm, dst) if mnemonic == "LDRH" else enc.ldrl(imm, dst)

        if mnemonic in ("LDRR", "MOV"):
            if mnemonic == "MOV":
                if len(operands) != 2:
                    raise AsmError("MOV takes dst, src")
                return enc.alu(enc.ADD, enc.INCR_0, enc.R0, parse_reg(operands[1]), parse_reg(operands[0]))
            if len(operands) not in (2, 4):
                raise AsmError("LDRR takes dst, src1[, src2, inc]")
            dst = parse_reg(operands[0])
            src1 = parse_reg(operands[1])
            src2 = parse_reg(operands[2]) if len(operands) == 4 else enc.R0
            inc = parse_int(operands[3]) if len(operands) == 4 else enc.INCR_0
            return enc.ldrr(inc, src2, src1, dst)

        if mnemonic in ("LDRP", "STRP"):
            if len(operands) != 2:
                raise AsmError(f"{mnemonic} takes reg, [base + offset]")
            reg = parse_reg(operands[0])
            base, offset, offset_is_reg = parse_addr(operands[1])
            return enc.ldrp(offset, base, reg, offset_is_reg) if mnemonic == "LDRP" else enc.strp(offset, base, reg, offset_is_reg)

        if mnemonic in ALU_OPS:
            if mnemonic == "CMP" and len(operands) == 2:
                src1 = parse_reg(operands[0])
                src2 = parse_reg(operands[1])
                dst = enc.R0
            elif len(operands) == 3:
                dst = parse_reg(operands[0])
                src1 = parse_reg(operands[1])
                src2 = parse_reg(operands[2])
            else:
                raise AsmError(f"{mnemonic} takes dst, src1, src2")
            return enc.alu(ALU_OPS[mnemonic], enc.INCR_0, src2, src1, dst)

        if mnemonic in LDRF_OPS:
            if len(operands) != 3:
                raise AsmError(f"{mnemonic} takes dst, src1, src2")
            return enc.ldrf(LDRF_OPS[mnemonic], enc.INCR_0, parse_reg(operands[2]), parse_reg(operands[1]), parse_reg(operands[0]))

        if mnemonic == "JMP":
            if len(operands) != 1:
                raise AsmError("JMP takes a target label/address")
            return enc.jmp_abs(resolve_value(operands[0], labels))

        if mnemonic in JMPF_OPS:
            if len(operands) != 1:
                raise AsmError(f"{mnemonic} takes a target label/offset")
            target = operands[0].strip()
            offset = labels[target] - (pc_words * 4) - 8 if target in labels else resolve_value(target, labels)
            return enc.jmp_flag(JMPF_OPS[mnemonic], offset)

        raise AsmError(f"unknown mnemonic {mnemonic}")
    except ValueError as exc:
        raise AsmError(f"{line.path}:{line.lineno}: {exc}") from exc


def assemble_lines(lines: list[SourceLine]) -> Program:
    labels, items = extract_labels(lines)
    imem: list[int] = []
    dmem: list[int] = []
    for item in items:
        word = encode_item(item, labels, len(imem), dmem)
        if word is not None:
            imem.append(word & 0xFFFF_FFFF)
    return Program(imem=imem, dmem=dmem)


def assemble_text(text: str, path: Path | None = None) -> Program:
    src = path or Path("<string>")
    lines = [SourceLine(src, idx, stripped) for idx, raw in enumerate(text.splitlines(), 1) if (stripped := strip_comment(raw))]
    return assemble_lines(lines)


def assemble_file(path: Path) -> Program:
    return assemble_lines(load_lines(path))


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Assemble qrisc32 repo-native assembly to hex.")
    parser.add_argument("input", type=Path)
    parser.add_argument("-o", "--output", type=Path, required=True, help="Instruction hex output path.")
    parser.add_argument("--dmem", type=Path, help="Optional data-memory hex output path.")
    args = parser.parse_args(argv)

    program = assemble_file(args.input)
    enc.write_hex(args.output, program.imem)
    if args.dmem is not None:
        enc.write_hex(args.dmem, program.dmem)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
