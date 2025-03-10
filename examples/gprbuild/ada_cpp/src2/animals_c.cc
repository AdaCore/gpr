//animals.cpp

#include "animals.h"
#include <iostream>

using namespace std;

//  -------------------------------------------------------------
void Animal::Set_Age (int New_Age)
{
  cout << " In C++ Animal::Set_Age" << endl;
  Age_Count = New_Age;
}

int Animal:: Age (void)
{
  cout << " In C++ Animal::Age" << endl;
  return Age_Count;
}

//  -------------------------------------------------------------
int Dog::Number_Of_Teeth (void)
{
   cout << " In C++ Dog::Number_Of_Teeth" << endl;
   return Tooth_Count;
}

void Dog::Set_Owner (char* Name)
{
   cout << " In C++ Dog::Set_Owner" << endl;
}

Dog::Dog(void)
{
  cout << "C++: Constructor of Dog called" << endl;
}
