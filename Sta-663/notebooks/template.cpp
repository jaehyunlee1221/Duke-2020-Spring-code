
#include <iostream>

template<typename T>
T add(T a, T b) {
    return a + b;
}

int main() {
    int m =2, n =3;
    double u = 2.5, v = 4.5;
    
    std::cout << add(m, n) << std::endl;
    std::cout << add(u, v) << std::endl;
}