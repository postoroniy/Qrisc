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

need_pass "BSV flags" "PASS: dmem\\[0\\]=5 dmem\\[1\\]=7" make -C "${root_dir}/BSV" flags
need_pass "SystemVerilog flags" "PASS: dmem\\[0\\]=5 dmem\\[1\\]=7" bash -lc "CCACHE_DISABLE=1 make -C '${root_dir}/SystemVerilog' flags"
need_pass "Amaranth flags" "PASS: dmem\\[0\\]=5 dmem\\[1\\]=7" make -C "${root_dir}/Amaranth" flags
need_pass "Clash flags" "PASS: dmem\\[0\\]=5 dmem\\[1\\]=7" make -C "${root_dir}/Clash" flags
need_pass "BSC flags" "PASS: dmem\\[0\\]=5 dmem\\[1\\]=7" make -C "${root_dir}/BSC" flags

echo "OK: all implemented ports PASS flags"
