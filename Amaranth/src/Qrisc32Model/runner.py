from __future__ import annotations

from pathlib import Path
import sys

from .model import run_vector


def repo_root() -> Path:
    return Path(__file__).resolve().parents[3]


def main(argv: list[str] | None = None) -> int:
    args = list(sys.argv[1:] if argv is None else argv)
    if len(args) != 1 or args[0] not in {"smoke", "flags", "isa", "axi"}:
        print("usage: python -m Qrisc32Model.runner smoke|flags|isa|axi", file=sys.stderr)
        return 2

    manifest, dmem = run_vector(repo_root(), args[0])
    for key, expected in manifest["expected_dmem"].items():
        idx = int(key)
        got = dmem.get(idx, 0)
        if got != expected:
            print(f"FAIL: dmem[{idx}]={got} expected {expected}")
            return 1

    print(manifest["pass"])
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
