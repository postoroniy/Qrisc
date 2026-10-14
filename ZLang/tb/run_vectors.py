"""Exercise ZLang qrisc32 through its native persistent JIT simulator."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

import zlang


ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / "ZLang" / "src" / "Qrisc32.zhl"
VCD_DIR = ROOT / "ZLang" / "build" / "vcd"
SIGNALS = {
    "pc": ("!", 32),
    "instruction": ('"', 32),
    "data_addr": ("#", 32),
    "data_wdata": ("$", 32),
    "data_read": ("%", 1),
    "data_write": ("&", 1),
    "zero_flag": ("'", 1),
    "carry_flag": ("(", 1),
    "id_valid_out": ("*", 1),
    "ex_valid_out": ("+", 1),
    "mem_valid_out": (",", 1),
    "wb_valid_out": ("-", 1),
    "hazard_stall": (".", 1),
    "terminal_jump": ("0", 1),
}


def words(path: Path) -> list[int]:
    return [int(line, 16) for line in path.read_text().splitlines() if line.strip()]


def run_vector(program: zlang.sim.Program, vector: dict, max_cycles: int, verbose: bool, manifest_dir: Path | None = None) -> None:
    base = manifest_dir or ROOT / "tests"
    imem = words(base / vector["hex"])
    dmem = words(base / vector["dmem_init"]) if "dmem_init" in vector else []
    VCD_DIR.mkdir(parents=True, exist_ok=True)
    vcd_path = VCD_DIR / f"{vector['name']}.vcd"

    with program.create() as sim, vcd_path.open("w", encoding="ascii") as vcd:
        vcd.write("$date qrisc32 native JIT $end\n$version ZLang $end\n")
        vcd.write("$timescale 1ns $end\n$scope module qrisc32 $end\n")
        for name, (identifier, width) in SIGNALS.items():
            vcd.write(f"$var wire {width} {identifier} {name} $end\n")
        vcd.write("$upscope $end\n$enddefinitions $end\n")
        sim.set("enable", 1)
        terminal_count = 0
        for cycle in range(max_cycles):
            pc = int(sim.get("pc"))
            index = pc >> 2
            instruction = imem[index] if 0 <= index < len(imem) else 0
            sim.set("instruction", instruction)
            sim.set("data_rdata", 0)
            current = sim.eval()
            address = int(current["data_addr"]) >> 2
            read_data = dmem[address] if 0 <= address < len(dmem) else 0
            sim.set("data_rdata", read_data)
            current = sim.eval()

            if verbose and instruction >> 28 == 5:
                regs_lo = int(sim.get_packed("regs_lo"))
                print(
                    f"pc=0x{pc:08x} LDRF=0x{instruction:08x} "
                    f"r2={(regs_lo >> 64) & 0xffffffff} "
                    f"Z={current['zero_flag']} C={current['carry_flag']}"
                )

            vcd.write(f"#{cycle * 10}\n")
            values = {"instruction": instruction, **current}
            for name, (identifier, width) in SIGNALS.items():
                vcd.write(f"b{int(values[name]):0{width}b} {identifier}\n")

            if int(current["data_write"]):
                if verbose:
                    print(
                        f"pc=0x{pc:08x} inst=0x{instruction:08x} "
                        f"dmem[{address}]={int(current['data_wdata'])} "
                        f"Z={current['zero_flag']} C={current['carry_flag']}"
                    )
                if address >= 4096:
                    raise AssertionError(f"data address 0x{address * 4:08x} out of demo range")
                if address >= len(dmem):
                    dmem.extend([0] * (address + 1 - len(dmem)))
                dmem[address] = int(current["data_wdata"])

            sim.edge("clk")
            terminal_count = terminal_count + 1 if int(current["terminal_jump"]) else terminal_count
            if terminal_count >= 3:
                break
        else:
            raise AssertionError(f"{vector['name']}: timed out after {max_cycles} cycles")

    for key, expected in vector["expected_dmem"].items():
        index = int(key)
        actual = dmem[index] if index < len(dmem) else 0
        if actual != expected:
            raise AssertionError(
                f"{vector['name']}: dmem[{index}] = {actual}, expected {expected}"
            )
    print(vector["pass"])


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("vector", help="Vector name or all")
    parser.add_argument("--manifest", type=Path, default=ROOT / "tests/vectors.json")
    parser.add_argument("--max-cycles", type=int, default=100000)
    parser.add_argument("--verbose", action="store_true")
    args = parser.parse_args()
    manifest = json.loads(args.manifest.read_text())
    vectors = [v for v in manifest["vectors"] if args.vector in ("all", v["name"])]
    if not vectors:
        parser.error(f"unknown vector {args.vector!r}")
    program = zlang.sim.compile(SOURCE, top="Qrisc32", engine="native")
    for vector in vectors:
        run_vector(program, vector, args.max_cycles, args.verbose, args.manifest.parent)


if __name__ == "__main__":
    main()
