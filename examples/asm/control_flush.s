# Branches must squash every younger side effect without delay slots.
  LDRL r0, 0
  LDRL r1, 1
  LDRL r2, 2
  CMP r0, r1, r2
  JMPC taken
  STRP r1, [r0 + 0]
  STRP r1, [r0 + 4]
  LDRL r3, 99
taken:
  LDRL r3, 7
  STRP r3, [r0 + 8]
  CMP r0, r2, r1
  JMPC not_taken
  LDRL r4, 9
  STRP r4, [r0 + 12]
  JMP halt
not_taken:
  STRP r1, [r0 + 20]
halt:
  JMP halt
  NOP
