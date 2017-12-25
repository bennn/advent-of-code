#include <stdio.h>

int main() {
  int a=0,b=0,c=0,d=0,e=0,f=0,g=0,h=0;
  a=1;
  b=57;
  if (a != 0) {
    //b *= 100;
    b -= -100000;
    c = b;
    c -= -17000;
  }
  A: f = 1;
  d = 2;
  B: e = 2;
  C: g = d;
  g *= e;
  g -= b;
  if (0 == g) {
    f = 0;
  } else {
  }
  e ++;
  g = e;
  g -= b;
  if (0 != g) { goto C; }
  d ++;
  g = d;
  g -= b;
  if (0 != g) { goto B; }
  if (0 != f) {
  } else {
    h ++;
  }
  g = b;
  g -= c;
  if (0 != g) {
    b -= -17;
    goto A;
  }
  return h;
}
