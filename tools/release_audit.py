#!/usr/bin/env python3
"""Reject machine-local paths or credentials in candidate release sources."""

from __future__ import annotations

import re
import subprocess
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SELF = Path(__file__).resolve()
PATTERNS = {
    "Unix home path": re.compile(r"/(?:home|Users)/[A-Za-z0-9._-]+/"),
    "local sibling checkout": re.compile(r"projects/(?:piecedata|bluespec)/"),
    "AWS access key": re.compile(r"AKIA[0-9A-Z]{16}"),
    "GitHub token": re.compile(r"gh[pousr]_[A-Za-z0-9]{20,}"),
    "private key": re.compile(r"BEGIN (?:RSA |OPENSSH |EC )?PRIVATE KEY"),
}


def candidate_paths() -> list[Path]:
    result = subprocess.run(
        ["git", "ls-files", "--cached", "--others", "--exclude-standard", "-z"],
        cwd=ROOT,
        check=True,
        stdout=subprocess.PIPE,
    )
    return [ROOT / item.decode() for item in result.stdout.split(b"\0") if item]


def main() -> int:
    findings: list[str] = []
    for path in candidate_paths():
        if path.resolve() == SELF or not path.is_file():
            continue
        try:
            text = path.read_text()
        except (UnicodeDecodeError, OSError):
            continue
        relative = path.relative_to(ROOT)
        for line_number, line in enumerate(text.splitlines(), 1):
            for label, pattern in PATTERNS.items():
                if pattern.search(line):
                    findings.append(f"{relative}:{line_number}: {label}: {line.strip()}")
    if findings:
        print("Release audit failed:")
        print("\n".join(findings))
        return 1
    print("PASS: release source audit")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
