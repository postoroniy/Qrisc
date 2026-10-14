"""Run one generated instruction image through an RTL port's checked AXI bench."""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
from pathlib import Path

from .generate_programs import ROOT, generate


BENCH_GENERATOR = ROOT / "Clash/tb/gen_vector_bench.py"
CHECKED_MEMORY = ROOT / "SystemVerilog/tb/axi_checked_mem.sv"


def run(command: list[str], cwd: Path, expected_pass: str | None = None) -> None:
    result = subprocess.run(
        command, cwd=cwd, env={**os.environ, "CCACHE_DISABLE": "1"},
        text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
    )
    if result.returncode:
        print(result.stdout, file=sys.stderr)
        raise subprocess.CalledProcessError(result.returncode, command)
    if expected_pass is not None and expected_pass not in result.stdout.splitlines():
        print(result.stdout, file=sys.stderr)
        raise RuntimeError(f"missing expected PASS line: {expected_pass}")
    for line in result.stdout.splitlines():
        if line.startswith("PASS:"):
            print(line, flush=True)


def hdl_sources(port: str) -> list[str]:
    if port == "SystemVerilog":
        rtl = ROOT / port / "rtl"
        return [str(rtl / "Qrisc_pack.sv"), *(str(path) for path in sorted(rtl.glob("Qrisc32*.sv")))]
    if port == "Amaranth":
        run(["make", "verilog"], ROOT / port)
        return [str(ROOT / port / "build/verilog/qrisc32.v")]
    if port == "Clash":
        run(["make", "verilog"], ROOT / port)
        hdl = ROOT / port / "build/hdl"
        return [str(path) for path in sorted((*hdl.rglob("*.v"), *hdl.rglob("*.sv")))]
    if port in ("BSV", "BSC"):
        run(["make", "verilog"], ROOT / port)
        distro = os.environ.get("DISTRO") or str(Path(os.environ.get("BSC_HOME", "")) / "lib")
        if not (Path(distro) / "Verilog/FIFO1.v").is_file():
            raise RuntimeError("set BSC_HOME or DISTRO to the Bluespec installation")
        sources = [
            str(Path(distro) / "Verilog/FIFO1.v"),
            str(Path(distro) / "Verilog/FIFO2.v"),
        ]
        if port == "BSC":
            sources.append(str(ROOT / port / "build/verilog/mkQrisc32.v"))
        sources.append(str(ROOT / port / "build/verilog/qrisc32.v"))
        return sources
    raise ValueError(f"unsupported Verilator port: {port}")


def run_verilator(port: str, spec: dict, manifest: Path, hdl: list[str]) -> None:
    name = spec["name"]
    port_dir = ROOT / port
    build = port_dir / "build/compat" / name
    bench = build / "tb"
    vcd_dir = port_dir / "build/vcd/compat"
    vcd_dir.mkdir(parents=True, exist_ok=True)
    command = [
        sys.executable, str(BENCH_GENERATOR), "--vector", name,
        "--manifest", str(manifest), "--out-dir", str(bench),
        "--vcd-dir", str(vcd_dir),
    ]
    if port in ("BSV", "BSC"):
        command.append("--bluespec")
    run(command, ROOT)
    top = f"qrisc32_{name}_tb"
    executable = build / f"V{top}"
    inputs = [*hdl, str(CHECKED_MEMORY), str(bench / f"{top}.sv"), str(bench / f"{top}.cpp")]
    if not executable.exists() or any(Path(path).stat().st_mtime > executable.stat().st_mtime for path in inputs):
        run([
            "verilator", "--cc", "--exe", "-O0", "-x-assign", "0", "--trace",
            "-Wno-fatal", "-Wno-lint", "-Wno-UNOPTFLAT", "--threads", "2",
            "--build", *hdl, str(CHECKED_MEMORY), str(bench / f"{top}.sv"),
            str(bench / f"{top}.cpp"), "--Mdir", str(build), "--top", top,
        ], port_dir)
    vcd = vcd_dir / f"{name}.vcd"
    vcd.unlink(missing_ok=True)
    run([str(executable)], port_dir, spec["pass"])
    if not vcd.is_file() or vcd.stat().st_size == 0:
        raise RuntimeError(f"missing VCD: {vcd}")


def run_zlang(spec: dict, manifest: Path) -> None:
    env = os.environ
    root = env.get("ZLANG_ROOT")
    python = env.get("ZLANG_PYTHON") or (str(Path(root) / ".venv/bin/python") if root else sys.executable)
    vcd = ROOT / "ZLang/build/vcd" / f"{spec['name']}.vcd"
    vcd.unlink(missing_ok=True)
    run([
        python, str(ROOT / "ZLang/tb/run_vectors.py"), spec["name"],
        "--manifest", str(manifest), "--max-cycles", str(spec.get("max_cycles", 50000)),
    ], ROOT, spec["pass"])
    if not vcd.is_file() or vcd.stat().st_size == 0:
        raise RuntimeError(f"missing VCD: {vcd}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("port", choices=("SystemVerilog", "BSV", "Amaranth", "Clash", "BSC", "ZLang"))
    parser.add_argument("--vector", default="all")
    parser.add_argument("--group", help="Run only generated vectors in this group.")
    args = parser.parse_args()
    manifest = generate(ROOT / "tests/toolchain_programs.json", ROOT / "build/toolchain")
    specs = json.loads(manifest.read_text())["vectors"]
    selected = [
        spec for spec in specs
        if args.vector in ("all", spec["name"])
        and (args.group is None or spec.get("group") == args.group)
    ]
    if not selected:
        parser.error(f"no vectors match vector={args.vector!r} group={args.group!r}")
    hdl = hdl_sources(args.port) if args.port != "ZLang" else []
    for spec in selected:
        if args.port == "ZLang":
            run_zlang(spec, manifest)
        else:
            run_verilator(args.port, spec, manifest, hdl)


if __name__ == "__main__":
    main()
