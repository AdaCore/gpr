#include <stdio.h>

#include "repro.h"

int
main (void)
{
  printf ("answer = %d\n", repro_answer ());
  printf ("double = %d\n", repro_double (21));

  return 0;
}
