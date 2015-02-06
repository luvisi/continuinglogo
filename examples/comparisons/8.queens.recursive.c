
#include <stdio.h>


int test_position(int new, int count, int rest[]) {
    int i;

    for(i = 0; i < count; i++) {
        if(new == rest[i]) return 0;
        if(new == rest[i] + (count-i)) return 0;
        if(new == rest[i] - (count-i)) return 0;
    }
    return 1;
}

void f_8_queens_helper(int n, int total, int sofar[]) {
  int i;

  if(n == total) {
      for(i = 0; i < total; i++)
          printf("%d ", sofar[i]);
      printf("\n");
      return;
  }

  for(i = 0; i < 8; i++)
    if(test_position(i, n, sofar)) {
      sofar[n] = i;
      f_8_queens_helper(n+1, total, sofar);
    }
}

void f_8_queens() {
  int places[8];

  f_8_queens_helper(0, 8, places);
}

int main() {
    f_8_queens();
}
