
#include <iostream>

using std::cout;
using std::begin;
using std::end;

int main() {
    
    // declare memory 
    int *z = new int; // single integer
    *z = 23;
    
    // Allocate on heap
    int *zs = new int[3]; // array of 3 integers
    for (int i=0; i<3; i++) {
        zs[i] = 10*i;
    }

    cout << *z << "\n";
    
    for (int i=0; i < 3; i++) {
        cout << zs[i] << " ";
    }
    cout << "\n";
    
    // need for manual management of dynamically assigned memory
    delete z;
    delete[] zs; 
}