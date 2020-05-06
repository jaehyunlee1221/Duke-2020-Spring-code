
#include <iostream>
using std::cout;
using std::endl;
using std::begin;
using std::end;

int main() 
{
    int x[] = {1, 2, 3, 4, 5};

    cout << "\nTraditional for loop\n";
    for (int i=0; i < sizeof(x)/sizeof(x[0]); i++) {
        cout << i << endl;
    }

    cout << "\nUsing iterators\n";
    for (auto it=begin(x); it != end(x); it++) {
        cout << *it << endl;
    }
    
    cout << "\nRanged for loop\n\n";
    for (auto const &i : x) {
        cout << i << endl;
    }
}