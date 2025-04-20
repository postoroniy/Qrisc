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

need_pass "BSV AXI" "PASS: AXI smoke dmem\\[0\\] = 12" make -C "${root_dir}/BSV" axi
need_pass "SystemVerilog AXI" "PASS: AXI smoke dmem\\[0\\] = 12" bash -lc "CCACHE_DISABLE=1 make -C '${root_dir}/SystemVerilog' axi"
need_pass "Amaranth AXI" "PASS: AXI smoke dmem\\[0\\] = 12" make -C "${root_dir}/Amaranth" axi
need_pass "Clash AXI" "PASS: AXI smoke dmem\\[0\\] = 12" make -C "${root_dir}/Clash" axi
need_pass "BSC AXI" "PASS: AXI smoke dmem\\[0\\] = 12" make -C "${root_dir}/BSC" axi

echo "OK: all implemented ports PASS AXI smoke"
