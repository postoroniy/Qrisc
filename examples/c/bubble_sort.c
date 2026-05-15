int main(void) {
  int data[10] = {10, 9, 8, 7, 6, 5, 4, 3, 2, 1};
  int i = 0;
  int j = 0;
  int tmp = 0;

  for (i = 0; i < 10; i = i + 1) {
    for (j = 0; j < 9; j = j + 1) {
      if (data[j] > data[j + 1]) {
        tmp = data[j];
        data[j] = data[j + 1];
        data[j + 1] = tmp;
      }
    }
  }

  return 0;
}
