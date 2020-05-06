
#include <iostream>
using std::cout;
using std::endl;
        
int main() {
    
    int N = 3;
    double counts[N];
    
    counts[0] = 1;
    counts[1] = 3;
    counts[2] = 3;

    double avg = (counts[0] + counts[1] + counts[2])/3;
    
    cout << avg << endl;    
}