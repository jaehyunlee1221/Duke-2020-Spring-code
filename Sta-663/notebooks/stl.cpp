
#include <iostream>
#include <vector>
#include <map>
#include <unordered_map>

using std::vector;
using std::map;
using std::unordered_map;
using std::string;
using std::cout;
using std::endl;
        
struct Point{
    int x;
    int y;
    
    Point(int x_, int y_) : 
      x(x_), y(y_) {};
};

int main() {
    vector<int> v1 = {1,2,3};
    v1.push_back(4);
    v1.push_back(5);
    
    cout << "Vecotr<int>" << endl;
    for (auto n: v1) {
        cout << n << endl;
    }
    cout << endl;
    
    vector<Point> v2;
    v2.push_back(Point(1, 2));
    v2.emplace_back(3,4);
    
    cout <<  "Vector<Point>" << endl;
    for (auto p: v2) {
        cout << "(" << p.x << ", " << p.y << ")" << endl;
    }
    cout << endl;
    
    map<string, int> v3 = {{"foo", 1}, {"bar", 2}};
    v3["hello"] = 3;
    v3.insert({"goodbye", 4});    
    
    // Note the a C++ map is ordered
    // Note using (traditional) iterators instead of ranged for loop
    cout << "Map<string, int>" << endl;
    for (auto iter=v3.begin(); iter != v3.end(); iter++) {
        cout << iter->first << ": " << iter->second << endl;
    }
    cout << endl;
    
    unordered_map<string, int> v4 = {{"foo", 1}, {"bar", 2}};
    v4["hello"] = 3;
    v4.insert({"goodbye", 4});    
    
    // Note the unordered_map is similar to Python' dict.'
    // Note using ranged for loop with const ref to avoid copying or mutation
    cout << "Unordered_map<string, int>" << endl;
    for (const auto& i: v4) {
        cout << i.first << ": " << i.second << endl;
    }
    cout << endl;
}