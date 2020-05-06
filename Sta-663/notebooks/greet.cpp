
#include <iostream>
#include <string>
using std::string;
using std::cout;

int main(int argc, char* argv[]) {
    string name = argv[1];
    int n = std::stoi(argv[2]);
    
    for (int i=0; i<n; i++) {
        cout << "Hello " << name << "!" << "\n";
    }
}