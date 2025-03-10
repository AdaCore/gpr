
/* Initialize a matrix with random values */

#include <stdlib.h>
#include <stdlib.h>

void
initmat (float *mat, int nb_line, int nb_column)
{
  int column, line;

  for (line=0; line<nb_line; line++)
    for (column=0; column<nb_column; column++)
      *(mat + column + line*nb_column) = (float) rand() / RAND_MAX;
}

