
#include <vector>
#include <iostream>
#include <numeric>

using std::cout;
using std::endl;
using std::vector;
using std::begin;
using std::end;
        
int main() {
    vector<int> v(10);

    // iota is somewhat like range
    std::iota(v.begin(), v.end(), 1);
    
    for (auto i: v) {
        cout << i << " ";
    }
    cout << endl;
    
    // C++ version of reduce    
    cout << std::accumulate(begin(v), end(v), 0) << endl;
    
    // Accumulate with lambda
    cout << std::accumulate(begin(v), end(v), 1, [](int a, int b){return a * b; }) << endl;
}