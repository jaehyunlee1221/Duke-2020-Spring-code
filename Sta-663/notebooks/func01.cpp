
#include <iostream>

double add(double x, double y) {
    return x + y;
}

double mult(double x, double y) {
    return x * y;
}

int main() {
    double a = 3;
    double b = 4;
    
    std::cout << add(a, b) << std::endl;
    std::cout << mult(a, b) << std::endl;
    
}