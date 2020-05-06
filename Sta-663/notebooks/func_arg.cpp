
#include <iostream>
using std::cout;
using std::endl;
    
// Value parameter
void f1(int x) {
    x *= 2;
    cout << "In f1    : x=" << x << endl;
}

// Reference parameter
void f2(int &x) {
    x *= 2;
    cout << "In f2    : x=" << x << endl;
}

/* Note
If you want to avoid side effects 
but still use references to avoid a copy operation
use a const refernece like this to indicate that x cannot be changed

void f2(const int &x) 
*/

/* Note 
Raw pointers are prone to error and 
generally avoided in modern C++
See unique_ptr and shared_ptr
*/

// Raw pointer parameter
void f3(int *x) {
    *x *= 2;
    cout << "In f3    : x=" << *x << endl;
}
    
int main() {
    int x = 1;
    
    cout << "Before f1: x=" << x << "\n";
    f1(x);
    cout << "After f1 : x=" << x << "\n";
    
    cout << "Before f2: x=" << x << "\n";
    f2(x);
    cout << "After f2 : x=" << x << "\n";
    
    cout << "Before f3: x=" << x << "\n";
    f3(&x);
    cout << "After f3 : x=" << x << "\n";
}