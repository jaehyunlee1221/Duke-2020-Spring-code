
#include <iostream>
using std::cout;
        
int main(int argc, char* argv[]) {
    for (int i=0; i<argc; i++) {
        cout << i << ": " << argv[i] << "\n";
    }
}