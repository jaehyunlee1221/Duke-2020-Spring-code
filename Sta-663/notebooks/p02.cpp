
#include <iostream>

using std::cout;
using std::begin;
using std::end;

int main() {
    int xs[] = {1,2,3,4,5};
    
    int ys[3];
    for (int i=0; i<5; i++) {
        ys[i] = i*i;
    }
    
    for (auto x=begin(xs); x!=end(xs); x++) {
        cout << *x << " ";
    }
    cout << "\n";
    
    for (auto x=begin(ys); x!=end(ys); x++) {
        cout << *x << " ";
    }
    cout << "\n";
}