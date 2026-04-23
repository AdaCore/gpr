// main.cpp
extern "C" {    
    void pkg_execute();
    void pkg2_execute();
    void adainit();
    void adafinal();
}

int main() {
    adainit();
    pkg_execute();
    pkg2_execute();
    adafinal();
    return 0;
}
