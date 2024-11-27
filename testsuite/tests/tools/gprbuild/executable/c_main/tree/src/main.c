extern void adainit();
extern void adafinal();
extern void exported_to_c();

int main() {
  adainit();
  exported_to_c();
  adafinal();
}
