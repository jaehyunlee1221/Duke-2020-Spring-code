
#include <iostream>
using std::cout;

// Using value
void foo1(int x) {
    x = x + 1;
}


// Using pointer
void foo2(int *x) {
    *x = *x + 1;
}

// Using ref
void foo3(int &x) {
    x = x + 1;
}

int main() {
    int x = 0;
    
    cout << x << "\n";
    foo1(x);
    cout << x << "\n";
    foo2(&x);
    cout << x << "\n";
    foo3(x);
    cout << x << "\n";
}