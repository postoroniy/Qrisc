int main(void) {
  int out[15];
  int a = -7;
  int b = 3;
  int i = 0;

  out[0] = a < b;
  out[1] = a >= b;
  out[2] = a >> 1;
  out[3] = b >> 1;
  for (i = 0; i < 3; i = i + 1) {
    out[4] = i;
  }
  out[5] = i;
  out[6] = -2147483648 < 0;
  out[7] = 2147483647 > -2147483648;
  out[8] = -2147483648 <= 2147483647;
  out[9] = -1 >> 31;
  out[10] = -2147483648 >> 31;
  out[11] = -3 >> 1;
  out[12] = 3 >= -7;
  int j = 0;
  while (j < 6) {
    j = j + 1;
    if (j == 2) continue;
    if (j == 4) break;
  }
  out[13] = j;
  out[14] = out[13] + out[5];
  return 0;
}
