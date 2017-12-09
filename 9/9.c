#include <stdio.h>

// Usage:
//   ./9 < INPUT
// Prints solutions to STDOUT

int main(){
  char c;
  int score = 0;
  int garbage = 0;
  int gmode = 0;
  int layer = 1;
  while ((c = getchar()) != EOF) {
    if (gmode) {
      if (c == '!') {
        getchar();
      } else if (c == '>') {
        gmode = 0;
      } else {
        garbage++;
      }
    } else {
      if (c == '<') {
        gmode = 1;
      } else if (c == '{') {
        score += layer++;
      } else if (c == '}') {
        layer--;
      }
    }
  }
  printf("day 09 solutions:\n- score : %d\n- garbage : %d\n", score, garbage);
  return 0;
}
