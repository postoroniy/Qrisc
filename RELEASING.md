# Preparing a GitHub release

This repository uses the SystemVerilog core as the ISA reference. The BSV,
Amaranth, Clash, BSC, and ZLang are part of the default simulator tests.
The ZLang port is an overlapping five-stage pipeline with a typed native AXI4
adapter, native-JIT benches, generated-Verilog lint, and formal entry points.

## Local setup

Install the external simulators and compilers, then configure local paths:

```sh
cp .env.example .env
# Edit .env for this machine, then load it into the shell.
. ./.env
```

`BSC_HOME` names an installation prefix containing `bin/bsc` and `lib/`.
`PICCOLO_ROOT` names the Piccolo checkout required by the Bluespec AXI tests.
Set `ZLANG_ROOT` for a local checkout with a native-JIT-enabled virtualenv, or
install the released `zlang-hdl==0.1.0a23` distribution and its native runtime
in the active Python environment. The distribution provides the `zlang` CLI;
the unrelated PyPI distribution named `zlang` is not this HDL compiler. Set
`CLASH_COMPILER_ROOT` only when building Clash from source. Normal Clash tests
need a working `clash` command and, for a local Cabal build, may also need
`CLASH_PACKAGE_DB`.

The private `.env` file is ignored. Never commit local build outputs or a
machine-specific environment file.

## Checks

```sh
make release-check
git status --short
```

The release check verifies the root `VERSION` against `ZLang/zlang.toml` and
requires ZLang `0.1.0a23` by default. Override `EXPECTED_ZLANG_VERSION` only for
an intentional test run; update the release notes before publishing
with a different supported version.

Review the source diff and newly added files before committing. Search the
release contents for personal home directories, sibling checkout assumptions,
and credentials. The tracked presentation PDF is intentional; `make clean`
does not remove it.

For `v0.1.0`, use `RELEASE_NOTES.md` as the GitHub release body and verify that
`CHANGELOG.md`, `VERSION`, and `ZLang/zlang.toml` agree. After the checks pass,
commit the intended files, push the branch, create an annotated `v0.1.0` tag,
push the tag, and create the GitHub release. Preparation alone does not commit,
tag, publish, or move the remote branch.
