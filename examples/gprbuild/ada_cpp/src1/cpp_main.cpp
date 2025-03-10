#include <iostream>
extern "C" void call_cpp ();
extern "C" void adainit ();
extern "C" void adafinal ();

using namespace std;

int main () 
{
  adainit ();
  call_cpp ();
  adafinal ();
}
