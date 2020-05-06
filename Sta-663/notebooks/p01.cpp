
#include <iostream>

using std::cout;

int main() {
    int x = 23;
    int *xp;
    xp = &x;
    
    cout << "x                    " << x << "\n";
    cout << "Address of x         " << &x << "\n";
    cout << "Pointer to x         " << xp << "\n";
    cout << "Value at pointer to x " << *xp << "\n";
}