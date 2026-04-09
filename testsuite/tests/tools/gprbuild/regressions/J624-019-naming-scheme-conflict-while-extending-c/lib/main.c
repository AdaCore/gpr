// main.c
extern void adainit(void);
extern void adafinal(void);
extern void pkg_execute(void);
extern void pkg2_execute(void);

int main(void) {
    adainit();
    pkg_execute();
    pkg2_execute();
    adafinal();
    return 0;
}
