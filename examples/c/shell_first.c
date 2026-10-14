int main(void) {
  int data[10] = {10, 9, 8, 7, 6, 5, 4, 3, 2, 1};
  int i = 1;
  int j = 0;
  int value = 0;

  while (i < 10) {
    value = data[i];
    j = i;
    while (j >= 1) {
      if (data[j - 1] > value) {
        data[j] = data[j - 1];
        j = j - 1;
      } else {
        break;
      }
    }
    data[j] = value;
    i = i + 1;
  }

  return 0;
}
