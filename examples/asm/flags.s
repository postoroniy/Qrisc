# Select values through CMP plus carry flag and store them to dmem[0..1].
  LDRH r0, 0
  LDRL r0, 0
  LDRH r1, 0
  LDRL r1, 5
  LDRH r2, 0
  LDRL r2, 7
  NOP
  NOP
  NOP
  CMP r0, r1, r2
  NOP
  NOP
  NOP
  LDRC r3, r1, r2
  NOP
  NOP
  NOP
  STRP r3, [r0 + 0]
  LDRNC r4, r1, r2
  NOP
  NOP
  NOP
  STRP r4, [r0 + 4]
halt:
  JMP halt
  NOP
