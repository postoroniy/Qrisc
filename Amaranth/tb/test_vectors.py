from pathlib import Path

from Qrisc32Model.model import load_manifest, run_vector


REPO_ROOT = Path(__file__).resolve().parents[2]


def test_shared_vector(vector_name, record_property):
    manifest = load_manifest(REPO_ROOT)[vector_name]
    _, dmem = run_vector(REPO_ROOT, vector_name)
    for key, expected in manifest["expected_dmem"].items():
        assert dmem.get(int(key), 0) == expected

    record_property("vector_name", vector_name)
    record_property("pass_line", manifest["pass"])
