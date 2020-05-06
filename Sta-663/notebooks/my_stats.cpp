
#include "my_math.hpp"

int mean(int xs[], int n) {
    double s = 0;
    for (int i=0; i<n; i++) {
        s += xs[i];
    }
    return s/n;
}