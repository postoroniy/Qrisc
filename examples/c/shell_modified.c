int main(void) {
  int data[10] = {10, 9, 8, 7, 6, 5, 4, 3, 2, 1};
  int gap = 5;
  int i = 0;
  int j = 0;
  int value = 0;

  while (gap > 0) {
    i = gap;
    while (i < 10) {
      value = data[i];
      j = i;
      while (j >= gap) {
        if (data[j - gap] > value) {
          data[j] = data[j - gap];
          j = j - gap;
        } else {
          break;
        }
      }
      data[j] = value;
      i = i + 1;
    }

    if (gap == 5) {
      gap = 2;
    } else {
      gap = gap >> 1;
    }
  }

  return 0;
}
