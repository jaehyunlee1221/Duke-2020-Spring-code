
#include <iostream>
#include <vector>
#include <functional>

using std::cout;
using std::endl;
using std::function;
using std::vector;

int main() 
{
    cout << "\nUsing generalized function pointers\n";
    using func = function<double(double, double)>;

    auto f1 = [](double x, double y) { return x + y; };
    auto f2 = [](double x, double y) { return x * y; };
    auto f3 = [](double x, double y) { return x + y*y; };

    double x = 3, y = 4;

    vector<func> funcs = {f1, f2, f3,};

    for (auto& f : funcs) {
        cout << f(x, y) << "\n";
    }
}