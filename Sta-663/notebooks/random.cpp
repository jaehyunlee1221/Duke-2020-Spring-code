
#include <iostream>
#include <random>
#include <functional>

using std::cout;
using std::random_device;
using std::mt19937;
using std::default_random_engine;
using std::uniform_int_distribution;
using std::poisson_distribution;
using std::student_t_distribution;
using std::bind;
        
// start random number engine with fixed seed
// Note default_random_engine may give differnet values on different platforms
// default_random_engine re(1234);

// or
// Using a named engine will work the same on differnt platforms
// mt19937 re(1234);

// start random number generator with random seed
random_device rd;
mt19937 re(rd());

uniform_int_distribution<int> uniform(1,6); // lower and upper bounds
poisson_distribution<int> poisson(30); // rate
student_t_distribution<double> t(10); // degrees of freedom      

int main() 
{
    cout << "\nGenerating random numbers\n";

    auto runif = bind (uniform, re);
    auto rpois = bind(poisson, re);
    auto rt = bind(t, re);

    for (int i=0; i<10; i++) {
        cout << runif() << ", " << rpois() <<  ", " << rt() << "\n";

    }
}