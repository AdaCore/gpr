#include <stdio.h>

extern void adainit();
extern void adafinal();
extern void in_ada();

int main () {
  adainit();
  printf("hello from C\n");
  in_ada();
  adafinal();
  return 0;
}
