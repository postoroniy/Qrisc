int main(void) {
  int out[2];
  int a = 5;
  int b = 7;

  if (a < b) {
    out[0] = a;
  } else {
    out[0] = b;
  }

  if (a >= b) {
    out[1] = a;
  } else {
    out[1] = b;
  }

  return 0;
}
