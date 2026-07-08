extern void pkg2__print(int toto);
extern void pkg__print(int toto);
#include <stdio.h>
#include <stdlib.h>

int main (void) {
  pkg2__print (2);
  pkg__print (1);
  exit (0);
}
