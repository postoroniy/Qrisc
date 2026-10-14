"""Build the shared toolchain binary-compatibility manifest."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from .asm import assemble_file, assemble_text
from .cc import compile_file
from .encoder import jmp_abs, write_hex


ROOT = Path(__file__).resolve().parents[2]


def write_if_changed(path: Path, content: str) -> None:
    if not path.exists() or path.read_text() != content:
        path.write_text(content)


def expected_words(values: list[int]) -> dict[str, int]:
    return {str(index): value & 0xFFFF_FFFF for index, value in enumerate(sorted(values))}


def check_smoke_golden(output_dir: Path) -> None:
    generated = output_dir / "c_bubble_sort.hex"
    golden = ROOT / "tests/smoke.hex"
    if not generated.is_file() or generated.read_bytes() != golden.read_bytes():
        raise ValueError(
            "tests/smoke.hex differs from compact bubble_sort.c output; "
            "run `make update-smoke-golden` after reviewing the compiler change"
        )


def generate(spec_path: Path, output_dir: Path, *, check_golden: bool = True) -> Path:
    specs = json.loads(spec_path.read_text())["programs"]
    output_dir.mkdir(parents=True, exist_ok=True)
    vectors = []
    for spec in specs:
        source = ROOT / spec["source"]
        name = spec["name"]
        if source.suffix == ".c":
            assembly = compile_file(source, schedule="compact")
            write_if_changed(output_dir / f"{name}.s", assembly)
            program = assemble_text(assembly, source.with_suffix(".s"))
        elif source.suffix == ".s":
            program = assemble_file(source)
        else:
            raise ValueError(f"unsupported toolchain source: {source}")
        hex_path = (output_dir / f"{name}.hex").resolve()
        write_hex(hex_path, program.imem)
        terminal_pc = (len(program.imem) - 2) * 4
        if len(program.imem) < 2 or program.imem[-1] != 0 or program.imem[-2] != jmp_abs(terminal_pc):
            raise ValueError(f"{name} must end with a terminal JMP and NOP")
        if "sort_cases" in spec:
            if len(program.dmem) != 10:
                raise ValueError(f"{name} must declare exactly ten data words")
            for case_name, values in spec["sort_cases"].items():
                if len(values) != len(program.dmem):
                    raise ValueError(f"{name} {case_name} must contain ten data words")
                vector_name = f"{name}_{case_name}"
                dmem_path = (output_dir / f"{vector_name}.dmem.hex").resolve()
                write_hex(dmem_path, values)
                vectors.append({
                    "name": vector_name,
                    "group": "sort",
                    "hex": str(hex_path),
                    "dmem_init": str(dmem_path),
                    "expected_dmem": expected_words(values),
                    "pass": f"PASS: {name} {case_name}",
                    "max_cycles": spec.get("max_cycles", 250000),
                    "terminal_pc": terminal_pc,
                })
        else:
            dmem_path = (output_dir / f"{name}.dmem.hex").resolve()
            write_hex(dmem_path, program.dmem)
            vectors.append({
                "name": name,
                "hex": str(hex_path),
                "dmem_init": str(dmem_path),
                "expected_dmem": spec["expected_dmem"],
                "pass": spec["pass"],
                "axi": spec.get("axi", False),
                "max_cycles": spec.get("max_cycles", 50000),
                "terminal_pc": terminal_pc,
            })
    manifest = output_dir / "vectors.json"
    write_if_changed(manifest, json.dumps({"vectors": vectors}, indent=2) + "\n")
    if check_golden:
        check_smoke_golden(output_dir)
    return manifest


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--spec", type=Path, default=ROOT / "tests/toolchain_programs.json")
    parser.add_argument("--out-dir", type=Path, default=ROOT / "build/toolchain")
    parser.add_argument(
        "--update-smoke-golden",
        action="store_true",
        help="Replace tests/smoke.hex with reviewed compact bubble-sort output.",
    )
    args = parser.parse_args()
    manifest = generate(args.spec, args.out_dir, check_golden=not args.update_smoke_golden)
    if args.update_smoke_golden:
        generated = args.out_dir / "c_bubble_sort.hex"
        write_if_changed(ROOT / "tests/smoke.hex", generated.read_text())
        check_smoke_golden(args.out_dir)
    print(manifest)


if __name__ == "__main__":
    main()
