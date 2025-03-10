#include <iostream>
#include "cpp_routine.h"

using namespace std;

void recurse_then_raise (int n);

void cpp_routine () 
{
  cout << " In cpp_routine" << endl;

  cout << " Calling recurse_then_raise" << endl;

  try 
    {
      recurse_then_raise (10);
    }
  catch (int except)
    {
      cout << "   caught an exception: " << except << endl;
    }

  cout << " returning from cpp_routine." << endl;
}

void recurse_then_raise (int n) 
{
  if (n > 0) 
    {
      recurse_then_raise (n - 1);
    }
  else
    {
      throw 1;
    }
}
