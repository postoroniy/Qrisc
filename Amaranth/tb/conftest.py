from __future__ import annotations

from pathlib import Path

from Qrisc32Model.model import load_manifest


REPO_ROOT = Path(__file__).resolve().parents[2]
VECTOR_ORDER = ("smoke", "flags", "isa", "axi")
VECTOR_CHOICES = ("all",) + VECTOR_ORDER


def pytest_addoption(parser):
    parser.addoption(
        "--vector",
        action="store",
        default="all",
        choices=VECTOR_CHOICES,
        help="Run one shared qrisc32 vector or all vectors.",
    )


def pytest_generate_tests(metafunc):
    if "vector_name" not in metafunc.fixturenames:
        return

    selected = metafunc.config.getoption("--vector")
    manifests = load_manifest(REPO_ROOT)
    if selected == "all":
        names = [name for name in VECTOR_ORDER if name in manifests]
    else:
        names = [selected]
    metafunc.parametrize("vector_name", names, ids=names)


def pytest_terminal_summary(terminalreporter, exitstatus, config):
    if exitstatus != 0:
        return

    passed = {}
    for report in terminalreporter.stats.get("passed", []):
        if report.when != "call":
            continue
        props = dict(report.user_properties)
        vector_name = props.get("vector_name")
        pass_line = props.get("pass_line")
        if vector_name and pass_line:
            passed[vector_name] = pass_line

    for vector_name in VECTOR_ORDER:
        if vector_name in passed:
            terminalreporter.write_line(passed[vector_name])
