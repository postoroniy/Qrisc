# Small AXI smoke program: 5 + 7, store, read back, terminal loop.
  LDRH r0, 0
  LDRL r0, 0
  LDRH r1, 0
  LDRL r1, 5
  LDRH r2, 0
  LDRL r2, 7
  ADD r3, r1, r2
  STRP r3, [r0 + 0]
  LDRP r4, [r0 + 0]
  STRP r4, [r0 + 4]
halt:
  JMP halt
  NOP
