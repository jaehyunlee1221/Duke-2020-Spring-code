
#include <iostream>

double* add(double *x, double *y, int n) {
    double *res = new double[n];
    
    for (int i=0; i<n; i++) {
        res[i] = x[i] + y[i];
    }
    return res;
}

int main() {
    double a[] = {1,2,3};
    double b[] = {4,5,6};
    
    int n = 3;
    double *c = add(a, b, n);
    
    for (int i=0; i<n; i++) {
        std::cout << c[i] << " ";    
    }
    std::cout << "\n";
    
    delete[] c; // Note difficulty of book-keeping when using raw pointers!
}