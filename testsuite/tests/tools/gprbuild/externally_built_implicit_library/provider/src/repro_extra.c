#include "repro.h"

/* A second compilation unit, so the archive holds more than one member. */

int
repro_double (int x)
{
  return 2 * x;
}
