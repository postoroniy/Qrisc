from __future__ import annotations

from dataclasses import dataclass, field
import json
from pathlib import Path

MASK32 = 0xFFFF_FFFF


def u32(value: int) -> int:
    return value & MASK32


def sext(value: int, bits: int) -> int:
    sign = 1 << (bits - 1)
    return u32((value & (sign - 1)) - (value & sign))


def incr_value(code: int) -> tuple[int, bool]:
    return {
        0: (0, False),
        1: (1, True),
        2: (2, True),
        3: (4, True),
        4: (0, False),
        5: (-1, True),
        6: (-2, True),
        7: (-4, True),
    }[code]


def load_words(path: Path) -> list[int]:
    words: list[int] = []
    for line in path.read_text().splitlines():
        text = line.strip()
        if text:
            words.append(int(text, 16))
    return words


def load_manifest(repo_root: Path) -> dict[str, dict]:
    data = json.loads((repo_root / "tests" / "vectors.json").read_text())
    return {entry["name"]: entry for entry in data["vectors"]}


@dataclass
class Qrisc32Model:
    imem: list[int]
    dmem: dict[int, int] = field(default_factory=dict)
    regs: list[int] = field(default_factory=lambda: [0] * 32)
    pc: int = 0
    flag_z: bool = False
    flag_c: bool = False

    def step(self) -> None:
        idx = self.pc >> 2
        inst = self.imem[idx] if 0 <= idx < len(self.imem) else 0
        current_pc = self.pc
        self.pc = u32(self.pc + 4)

        op = (inst >> 28) & 0xF
        mode = (inst >> 26) & 0x3
        dst = inst & 0x1F
        src1 = (inst >> 5) & 0x1F
        src2 = (inst >> 10) & 0x1F
        imm16 = (inst >> 5) & 0xFFFF
        off15 = (inst >> 10) & 0x7FFF
        incr, incr_en = incr_value((inst >> 22) & 0x7)

        r1 = self.regs[src1]
        r2 = self.regs[src2]
        dst_val = self.regs[dst]
        offset = r2 if ((inst >> 25) & 1) else sext(off15, 15)

        write_reg = False
        write_value = dst_val
        inc_target: int | None = src2 if incr_en else None
        inc_source = r2

        if op == 0x0:
            if mode == 0:
                write_reg = dst != src1
                write_value = r1
            elif mode == 1:
                write_reg = True
                write_value = u32((dst_val & 0x0000_FFFF) | (imm16 << 16))
                inc_target = None
            elif mode == 2:
                write_reg = True
                write_value = u32((dst_val & 0xFFFF_0000) | imm16)
                inc_target = None
            else:
                addr = u32(r1 + offset)
                write_reg = True
                write_value = self.dmem.get(addr >> 2, 0)
                inc_target = src2 if ((inst >> 25) & 1) else None
                inc_source = r2
        elif op == 0x1:
            addr = u32(r1 + offset)
            self.dmem[addr >> 2] = dst_val
            inc_target = src2 if ((inst >> 25) & 1) else None
            inc_source = r2
        elif op == 0x2:
            if mode == 0:
                self.pc = inst & 0x03FF_FFFF
                inc_target = None
            elif mode in (1, 2):
                self.pc = u32(current_pc + offset)
                if mode == 2:
                    write_reg = True
                    write_value = current_pc
                inc_target = src2 if ((inst >> 25) & 1) else None
            else:
                self.pc = dst_val
                inc_target = src2 if ((inst >> 25) & 1) else None
        elif op == 0x3:
            cond = {
                0: self.flag_z,
                1: not self.flag_z,
                2: self.flag_c,
                3: not self.flag_c,
            }[mode]
            if cond:
                self.pc = u32(current_pc + offset)
            inc_target = src2 if ((inst >> 25) & 1) else None
        elif op == 0x4:
            alu = (inst >> 25) & 0x7
            if alu == 0:
                write_reg = True
                write_value = r1 & r2
                self.flag_c = False
                self.flag_z = write_value == 0
            elif alu == 1:
                write_reg = True
                write_value = r1 | r2
                self.flag_c = False
                self.flag_z = write_value == 0
            elif alu == 2:
                write_reg = True
                write_value = r1 ^ r2
                self.flag_c = False
                self.flag_z = write_value == 0
            elif alu == 3:
                result = r1 + r2
                write_reg = True
                write_value = u32(result)
                self.flag_c = result > MASK32
                self.flag_z = write_value == 0
            elif alu == 4:
                prod = r1 * r2
                write_reg = True
                write_value = u32(prod)
                self.flag_c = False
                self.flag_z = write_value == 0
            elif alu == 5:
                shl = (r1 << (r2 & 0x1F)) & ((1 << 33) - 1)
                write_reg = True
                write_value = u32(shl)
                self.flag_c = False
                self.flag_z = write_value == 0
            elif alu == 6:
                shift = r2 & 0x1F
                combined = (r1 << 1) & ((1 << 33) - 1)
                shr = combined >> shift
                write_reg = True
                write_value = (shr >> 1) & MASK32
                self.flag_c = (shr & 1) != 0
                self.flag_z = write_value == 0
            else:
                self.flag_z = r1 == r2
                self.flag_c = r1 < r2
        elif op == 0x5:
            cond = {
                0: self.flag_z,
                1: not self.flag_z,
                2: self.flag_c,
                3: not self.flag_c,
            }[mode]
            write_reg = True
            write_value = r1 if cond else r2

        if write_reg:
            self.regs[dst] = u32(write_value)
        if inc_target is not None:
            self.regs[inc_target] = u32(inc_source + incr)

    def run(self, max_cycles: int = 4096) -> dict[int, int]:
        seen_terminal = 0
        last_pc = None
        for _ in range(max_cycles):
            last_pc = self.pc
            self.step()
            if self.pc == last_pc:
                seen_terminal += 1
                if seen_terminal >= 4:
                    break
            else:
                seen_terminal = 0
        return self.dmem


def run_vector(repo_root: Path, name: str) -> tuple[dict, dict[int, int]]:
    manifest = load_manifest(repo_root)[name]
    tests_dir = repo_root / "tests"
    imem = load_words(tests_dir / manifest["hex"])
    dmem: dict[int, int] = {}
    if "dmem_init" in manifest:
        dmem.update(enumerate(load_words(tests_dir / manifest["dmem_init"])))
    model = Qrisc32Model(imem=imem, dmem=dmem)
    return manifest, model.run()
