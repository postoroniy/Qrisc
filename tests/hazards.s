# No-NOP hazard coverage for qrisc32 interlocks.
  LDRH r0, 0
  LDRL r0, 0
  LDRH r1, 0
  LDRL r1, 5
  LDRH r2, 0
  LDRL r2, 7
  ADD r3, r1, r2
  ADD r4, r3, r2
  STRP r4, [r0 + 0]
  LDRP r5, [r0 + 0]
  ADD r6, r5, r1
  STRP r6, [r0 + 4]
  LDRL r8, 8
  STRP r6, [r8 + 0]
  LDRL r10, 3
  LDRL r11, 9
  CMP r0, r10, r11
  LDRC r12, r10, r11
  STRP r12, [r0 + 12]
  CMP r0, r10, r11
  JMPNC branch_fail
  LDRL r13, 31
  JMP branch_after
branch_fail:
  LDRL r13, 99
branch_after:
  STRP r13, [r0 + 16]
  CMP r0, r10, r11
  JMPC carry_taken
  STRP r11, [r0 + 44]
carry_taken:
  LDRL r15, 77
  STRP r15, [r0 + 48]
  JMP zero_taken
  STRP r11, [r0 + 20]
zero_taken:
  LDRL r14, 42
  STRP r14, [r0 + 24]
  LDRL r20, 100
  LDRL r21, 5
  LDRR r22, r20, r21, 1
  ADD r23, r21, r1
  STRP r23, [r0 + 28]
  STRP r22, [r0 + 32]
  LDRP r24, [r0 + 0]
  STRP r24, [r0 + 36]
  LDRP r25, [r0 + 4]
  STRP r1, [r25 + 16]
halt:
  JMP halt
  NOP
