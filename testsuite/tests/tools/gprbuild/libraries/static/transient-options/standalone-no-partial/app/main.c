#include <stdio.h>
extern int library_value(void);
int main(void) { printf("%d\n", library_value()); return 0; }
