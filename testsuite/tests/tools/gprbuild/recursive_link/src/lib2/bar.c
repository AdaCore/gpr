#include <stdio.h>
#include "foo.h"

void bar() {
  foo2();
}

void bar2() {
  printf("in bar.c");
}
