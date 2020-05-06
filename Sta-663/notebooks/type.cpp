#include <iostream>
using std::cout;
using std::string;
using std::stoi;
        
int main() {
    char c = '3'; // A char is an integer type
    string s = "3"; // A string is not an integer type
    int i = 3;
    float f = 3.1;
    double d = 3.2;
    
    cout << c << "\n";
    cout << i << "\n";
    cout << f << "\n";
    cout << d << "\n";
    
    cout << "c + i is " << c + i << "\n";
    cout << "c + i is " << c - '0' + i << "\n";
    
    // Casting string to number
    cout << "s + i is " << stoi(s) + i << "\n"; // Use std::stod to convert to double
    
    // Two ways to cast float to int
    cout << "f + i is " << f + i << "\n"; 
    cout << "f + i is " << int(f) + i << "\n"; 
    cout << "f + i is " << static_cast<int>(f) + i << "\n"; 

}