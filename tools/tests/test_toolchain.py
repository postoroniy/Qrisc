import unittest
from pathlib import Path

from qrisc32 import encoder as enc
from qrisc32.asm import AsmError, assemble_text
from qrisc32.cc import CompileError, compile_text

REPO_ROOT = Path(__file__).resolve().parents[2]


class EncoderTests(unittest.TestCase):
    def test_known_words_match_existing_vectors(self):
        self.assertEqual(enc.ldrh(0, enc.R1), 0x04000001)
        self.assertEqual(enc.ldrl(5, enc.R1), 0x080000A1)
        self.assertEqual(enc.alu(enc.ADD, enc.INCR_0, enc.R2, enc.R1, enc.R3), 0x46000823)
        self.assertEqual(enc.strp(0, enc.R0, enc.R3), 0x1C000003)
        self.assertEqual(enc.jmp_abs(0x48), 0x20000048)


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

    def test_rejects_unknown_variable(self):
        with self.assertRaises(CompileError):
            compile_text("int main(void) { x = 1; return 0; }")


if __name__ == "__main__":
    unittest.main()
