
#include <iostream>

using std::cout;
using std::begin;
using std::end;

int main() {
    int m = 3;
    int n = 4;
    int **xss = new int*[m]; // assign memory for m pointers to int
    for (int i=0; i<m; i++) {
        xss[i] = new int[n]; // assign memory for array of n ints
        for (int j=0; j<n; j++) {
            xss[i][j] = i*10 + j;
        }
    }    
    
    for (int i=0; i<m; i++) {
        for (int j=0; j<n; j++) {
            cout << xss[i][j] << "\t";
        }
        cout << "\n";
    }
    
    // Free memory
    for (int i=0; i<m; i++) {
        delete[] xss[i];
    }
    delete[] xss;
}