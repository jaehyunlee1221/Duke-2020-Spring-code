
#include <iostream>
using std::cout;
using std::endl;

int main() {

    int a = 3, b = 4;
    int c = 0;

    // Lambda function with no capture
    auto add1 = [] (int a, int b) { return a + b; };
    // Lambda function with value capture
    auto add2 = [c] (int a, int b) { return c * (a + b); };
    // Lambda funciton with reference capture   
    auto add3 = [&c] (int a, int b) { return c * (a + b); };

    // Change value of c after function definition
    c += 5;

    cout << "Lambda function\n";
    cout << add1(a, b) <<  endl;
    cout << "Lambda function with value capture\n";
    cout << add2(a, b) <<  endl;
    cout << "Lambda function with reference capture\n";
    cout << add3(a, b) <<  endl;

}