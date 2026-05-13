from __future__ import annotations

from pathlib import Path
import sys

from amaranth.back import verilog

from .rtl import Qrisc32


def emit(path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    top = Qrisc32()
    path.write_text(verilog.convert(top, name="qrisc32", ports=top.ports()))


def main(argv: list[str] | None = None) -> int:
    args = list(sys.argv[1:] if argv is None else argv)
    if len(args) != 1:
        print("usage: python -m Qrisc32Model.verilog <output.v>", file=sys.stderr)
        return 2
    emit(Path(args[0]))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
