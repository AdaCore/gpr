// main.c
extern void __gnat_initialize (void *);
extern void mylib_p (void);

int
main (void)
{
  int SEH[2];
  __gnat_initialize (&SEH);
  mylib_p ();
  return 0;
}
