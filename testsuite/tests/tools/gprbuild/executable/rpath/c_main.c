#include <stdlib.h>
#include <stdio.h>

extern void mylibinit (void);
extern void mylibfinal (void);
extern int mult(int c, int t);

int main (void) {
  mylibinit();
  printf("%d", mult (2, 5));
  mylibfinal();
  exit (0);
}
