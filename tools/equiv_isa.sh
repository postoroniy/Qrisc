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

need_pass "BSV ISA" "PASS: ISA parity vector" make -C "${root_dir}/BSV" isa
need_pass "SystemVerilog ISA" "PASS: ISA parity vector" bash -lc "CCACHE_DISABLE=1 make -C '${root_dir}/SystemVerilog' isa"
need_pass "Amaranth ISA" "PASS: ISA parity vector" make -C "${root_dir}/Amaranth" isa
need_pass "Clash ISA" "PASS: ISA parity vector" make -C "${root_dir}/Clash" isa
need_pass "BSC ISA" "PASS: ISA parity vector" make -C "${root_dir}/BSC" isa

echo "OK: all implemented ports PASS ISA parity vector"
