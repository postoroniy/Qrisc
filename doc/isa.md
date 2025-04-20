# qrisc32 ISA Notes

The SystemVerilog implementation is the canonical ISA source for this repo.
These notes document the subset currently exercised by the shared smoke and
flags programs in `../tests/`.

## Instruction Word

Instructions are 32-bit words.  The major opcode is `inst[31:28]`.

| Opcode | Name | Tested behavior |
| --- | --- | --- |
| `0x0` | `LDR` | Load immediate high/low fields or load a word from data memory. |
| `0x1` | `STR` | Store a word to data memory. |
| `0x2` | `JMPUNC` | Unconditional jump. |
| `0x4` | `ALU` | Arithmetic/logic operation, including `ADD` and `CMP`. |
| `0x5` | `LDRF` | Conditional register select from flags. |

## Common Fields

- Register indexes are 5-bit values in the range `r0` through `r31`.
- `dst` is `inst[4:0]`.
- `src1` is `inst[9:5]`.
- `src2` is `inst[14:10]` when the format uses a register operand.
- Increment control is `inst[24:22]`: `000` no change, `001` plus 1,
  `010` plus 2, `011` plus 4, `101` minus 1, `110` minus 2, `111` minus 4.
- Offset source is usually `inst[25]`: `0` means signed immediate offset from
  `inst[24:10]`, `1` means signed register offset from `src2`.

## Tested Encodings

`LDRH dst, imm16`

- `inst[31:28] = 0x0`
- `inst[27:26] = 01`
- `inst[20:5] = imm16`
- Effect: writes `imm16` into `dst[31:16]`.

`LDRL dst, imm16`

- `inst[31:28] = 0x0`
- `inst[27:26] = 10`
- `inst[20:5] = imm16`
- Effect: writes `imm16` into `dst[15:0]`.

`LDRP dst, [base + off15]`

- `inst[31:28] = 0x0`
- `inst[27:26] = 11`
- `inst[25] = 0`
- `inst[24:10] = signed off15`
- `inst[9:5] = base`
- `inst[4:0] = dst`
- Effect: loads one 32-bit word from data memory.

`STRP src, [base + off15]`

- `inst[31:28] = 0x1`
- `inst[27:26] = 11`
- `inst[25] = 0`
- `inst[24:10] = signed off15`
- `inst[9:5] = base`
- `inst[4:0] = src`
- Effect: stores one 32-bit word to data memory.

`ALU dst, src1, src2`

- `inst[31:28] = 0x4`
- `inst[27:25]` selects the operation: `3` is `ADD`, `7` is `CMP`.
- `inst[24:22]` applies the increment control to `src2`.
- Effect: `ADD` writes a register result. `CMP` updates flags used by `LDRF`.

`LDRF dst, src1, src2`

- `inst[31:28] = 0x5`
- `inst[27:26]` selects the flag condition: `2` is carry set, `3` is carry
  clear.
- Effect: selects `src1` or `src2` according to the flags.

`JMP absolute`

- `inst[31:28] = 0x2`
- `inst[27:26] = 00`
- `inst[25:0]` is the target byte address.
- Effect: sets the program counter to the target.

## Shared Expected Results

- `tests/smoke.hex`: sorts `smoke_reversed.hex`, producing
  `dmem[0..9] = 1..10`.
- `tests/flags.hex`: `dmem[0] = 5`, `dmem[1] = 7`.
