
#include <iostream>
#include "my_math.hpp"
#include "my_stats.hpp"

int main() {
    int xs[] = {1,2,3,4,5};
    int n = 5;
    int a = 3, b= 4;
    
    std::cout << "sum = " << add(a, b) << "\n";
    std::cout << "prod = " << multiply(a, b) << "\n";
    std::cout << "mean = " << mean(xs, n) << "\n";
}