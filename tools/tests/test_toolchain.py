import json
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory

from qrisc32 import encoder as enc
from qrisc32.asm import AsmError, assemble_file, assemble_text
from qrisc32.cc import CompileError, compile_file, compile_text
from qrisc32.generate_programs import generate

REPO_ROOT = Path(__file__).resolve().parents[2]


class EncoderTests(unittest.TestCase):
    def test_known_words_match_existing_vectors(self):
        self.assertEqual(enc.ldrh(0, enc.R1), 0x04000001)
        self.assertEqual(enc.ldrl(5, enc.R1), 0x080000A1)
        self.assertEqual(enc.alu(enc.ADD, enc.INCR_0, enc.R2, enc.R1, enc.R3), 0x46000823)
        self.assertEqual(enc.strp(0, enc.R0, enc.R3), 0x1C000003)
        self.assertEqual(enc.jmp_abs(0x48), 0x20000048)
        self.assertEqual(enc.alu(enc.CMP, enc.INCR_0, enc.R2, enc.R1, enc.R0), 0x4E000820)
        self.assertEqual(enc.ldrf(enc.LDRC, enc.INCR_0, enc.R2, enc.R1, enc.R3), 0x58000823)
        self.assertEqual(enc.jmp_flag(enc.JMPNZ, 0), 0x34000000)

    def test_encoder_rejects_truncation(self):
        for encode in (
            lambda: enc.ldrl(0x10000, 1),
            lambda: enc.ldrp(16384, 0, 1),
            lambda: enc.strp(-16385, 0, 1),
            lambda: enc.alu(enc.ADD, 8, 0, 0, 1),
            lambda: enc.jmp_abs(3),
            lambda: enc.jmp_flag(enc.JMPZ, 2),
        ):
            with self.subTest(encode=encode), self.assertRaises(ValueError):
                encode()


class AssemblerTests(unittest.TestCase):
    def test_labels_and_flag_branch_offsets(self):
        program = assemble_text(
            """
            start:
              CMP r0, r1, r2
              JMPNZ done
              NOP
            done:
              JMP done
            """
        )
        self.assertEqual(program.imem[1], enc.jmp_flag(enc.JMPNZ, 0))
        self.assertEqual(program.imem[3], enc.jmp_abs(12))

    def test_dmem_directive(self):
        program = assemble_text(".dmem 10, 9, 8\nNOP\n")
        self.assertEqual(program.dmem, [10, 9, 8])
        self.assertEqual(program.imem, [0])

    def test_register_bounds_are_diagnosed(self):
        with self.assertRaises(AsmError):
            assemble_text("ADD r32, r0, r1\n")

    def test_immediate_and_word_bounds_are_diagnosed(self):
        for source in ("LDRL r1, 0x10000", ".word 0x100000000", ".dmem -2147483649", "JMP 3"):
            with self.subTest(source=source), self.assertRaises(AsmError):
                assemble_text(source)

    def test_missing_include_is_diagnosed(self):
        with self.assertRaisesRegex(AsmError, "cannot read"):
            assemble_file(REPO_ROOT / "tests" / "missing.s")

    def test_undefined_label_and_recursive_include_are_diagnosed(self):
        with self.assertRaisesRegex(AsmError, "undefined label missing"):
            assemble_text("JMP missing")
        with TemporaryDirectory() as directory:
            path = Path(directory) / "program.s"
            path.write_text('.include "program.s"\n')
            with self.assertRaisesRegex(AsmError, "recursive include"):
                assemble_file(path)

    def test_malformed_operands_and_include_location(self):
        for source in ("ADD r1, r2,", "LDRP r1, [r0 + 4", "ADD r1,, r2"):
            with self.subTest(source=source), self.assertRaises(AsmError):
                assemble_text(source)
        with TemporaryDirectory() as directory:
            path = Path(directory) / "program.s"
            path.write_text('.include "missing.s"\n')
            with self.assertRaisesRegex(AsmError, "program.s:1: cannot read"):
                assemble_file(path)


class CompilerTests(unittest.TestCase):
    def longest_nop_run(self, asm: str) -> int:
        best = 0
        current = 0
        for line in asm.splitlines():
            if line.strip() == "NOP":
                current += 1
                best = max(best, current)
            else:
                current = 0
        return best

    def test_compiles_flags_example_shape(self):
        src = (REPO_ROOT / "examples/c/flags.c").read_text()
        asm = compile_text(src)
        program = assemble_text(asm)
        self.assertGreater(len(program.imem), 8)
        self.assertEqual(program.dmem[:2], [0, 0])
        self.assertLess(self.longest_nop_run(asm), 6)

    def test_legacy_schedule_keeps_old_padding(self):
        src = "int main(void) { int x; x = 1; return x; }"
        asm = compile_text(src, schedule="legacy")
        self.assertGreaterEqual(self.longest_nop_run(asm), 6)

    def test_compact_schedule_removes_blanket_padding(self):
        src = "int main(void) { int x; x = 1; return x; }"
        asm = compile_text(src, schedule="compact")
        self.assertLess(self.longest_nop_run(asm), 6)
        self.assertEqual(compile_text(src, schedule="safe"), asm)
        self.assertEqual(compile_text(src, schedule="none"), asm)

    def test_rejects_unknown_variable(self):
        with self.assertRaises(CompileError):
            compile_text("int main(void) { x = 1; return 0; }")

    def test_signed_lowering_and_loop_state(self):
        src = (REPO_ROOT / "examples/c/signed.c").read_text()
        asm = compile_text(src)
        self.assertIn("shr_positive", asm)
        self.assertIn("LDRH r19, 0x8000", asm)
        self.assertIn("__forpost_", asm)
        self.assertLess(self.longest_nop_run(asm), 6)
        self.assertGreater(len(assemble_text(asm).imem), 20)

    def test_sort_examples_use_supported_loop_and_array_lowering(self):
        for source in ("bubble_sort.c", "shell_modified.c", "shell_first.c"):
            with self.subTest(source=source):
                asm = compile_file(REPO_ROOT / "examples/c" / source)
                program = assemble_text(asm)
                self.assertEqual(program.dmem, list(range(10, 0, -1)))
                self.assertIn("LDRP", asm)
                self.assertIn("STRP", asm)
                self.assertLess(self.longest_nop_run(asm), 6)

        modified = compile_file(REPO_ROOT / "examples/c/shell_modified.c")
        first = compile_file(REPO_ROOT / "examples/c/shell_first.c")
        self.assertIn("__while_", modified)
        self.assertIn("__while_", first)
        self.assertIn("JMP __endwhile_", modified)
        self.assertIn("SHR", modified)
        self.assertIn("JMP __endwhile_", first)

    def test_rejects_trailing_source_and_unterminated_block(self):
        for source in ("int main(void) {} garbage", "int main(void) { int x = 1;"):
            with self.subTest(source=source), self.assertRaises(CompileError):
                compile_text(source)

    def test_rejects_invalid_array_and_shift(self):
        for source in (
            "int main(void) { int a[0]; return 0; }",
            "int main(void) { int a[2] = 1; return 0; }",
            "int main(void) { int x = 1 << 32; return x; }",
            "int main(void) { int x = 2147483648; return x; }",
            "int main(void) { int x = -2147483649; return x; }",
            "int main(void) { int a[1] = {0xffffffff}; return 0; }",
        ):
            with self.subTest(source=source), self.assertRaises(CompileError):
                compile_text(source)


class GeneratedProgramTests(unittest.TestCase):
    def test_sort_cases_are_deterministic_and_share_instruction_images(self):
        with TemporaryDirectory() as directory:
            output = Path(directory)
            manifest_path = generate(
                REPO_ROOT / "tests/toolchain_programs.json", output, check_golden=False
            )
            first = {path.name: path.read_bytes() for path in output.iterdir()}
            generate(REPO_ROOT / "tests/toolchain_programs.json", output, check_golden=False)
            second = {path.name: path.read_bytes() for path in output.iterdir()}
            self.assertEqual(first, second)

            vectors = json.loads(manifest_path.read_text())["vectors"]
            sorts = [vector for vector in vectors if vector.get("group") == "sort"]
            self.assertEqual(len(sorts), 9)
            for prefix in ("c_bubble_sort", "c_shell_modified", "c_shell_first"):
                cases = [vector for vector in sorts if vector["name"].startswith(prefix)]
                self.assertEqual(len(cases), 3)
                self.assertEqual(len({vector["hex"] for vector in cases}), 1)
                for vector in cases:
                    initial = [
                        int(line, 16)
                        for line in Path(vector["dmem_init"]).read_text().splitlines()
                    ]
                    expected = [vector["expected_dmem"][str(i)] for i in range(10)]
                    self.assertEqual(expected, sorted(initial))


if __name__ == "__main__":
    unittest.main()
