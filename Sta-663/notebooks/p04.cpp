
#include <iostream>

using std::cout;
using std::begin;
using std::end;

int main() {
    int xs[] = {100,200,300,400,500,600,700,800,900,1000};
    
    cout << xs << ": " << *xs  << "\n";
    cout << &xs << ": " << *xs  << "\n";
    cout << &xs[3] << ": " << xs[3] << "\n";
    cout << xs+3 << ": " << *(xs+3)  << "\n"; 
}