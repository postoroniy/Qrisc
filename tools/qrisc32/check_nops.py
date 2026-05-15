from __future__ import annotations

import argparse
from pathlib import Path


def longest_nop_run(path: Path) -> int:
    best = 0
    current = 0
    for line in path.read_text().splitlines():
        if line.strip() == "NOP":
            current += 1
            best = max(best, current)
        else:
            current = 0
    return best


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Check generated qrisc32 assembly NOP runs.")
    parser.add_argument("--max-run", type=int, default=5)
    parser.add_argument("paths", type=Path, nargs="+")
    args = parser.parse_args(argv)

    failed = False
    for path in args.paths:
        run = longest_nop_run(path)
        if run > args.max_run:
            print(f"FAIL: {path}: longest NOP run {run} exceeds {args.max_run}")
            failed = True
    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
