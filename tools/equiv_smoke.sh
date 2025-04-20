#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

need_pass() {
  local name="$1"
  local expect_re="$2"
  shift 2
  echo "==> ${name}"
  local out
  out="$("$@" 2>&1)"
  echo "${out}"
  if ! echo "${out}" | rg -q "${expect_re}"; then
    echo "ERROR: ${name} did not match expected PASS pattern: ${expect_re}" >&2
    exit 1
  fi
}

need_pass "BSV smoke" "PASS: qrisc32 sort smoke" make -C "${root_dir}/BSV" smoke
need_pass "SystemVerilog smoke" "PASS: qrisc32 sort smoke" bash -lc "CCACHE_DISABLE=1 make -C '${root_dir}/SystemVerilog' smoke"
need_pass "Amaranth smoke" "PASS: qrisc32 sort smoke" make -C "${root_dir}/Amaranth" smoke
need_pass "Clash smoke" "PASS: qrisc32 sort smoke" make -C "${root_dir}/Clash" smoke
need_pass "BSC smoke" "PASS: qrisc32 sort smoke" make -C "${root_dir}/BSC" smoke

echo "OK: all implemented ports PASS smoke"
