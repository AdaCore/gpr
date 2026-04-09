extern "C" void adainit();
extern "C" void adafinal();
extern "C" void exported_to_c();

int main() {
    adainit();
    exported_to_c();
    adafinal();
    return 0;
}
