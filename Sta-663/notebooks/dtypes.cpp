
#include <iostream>
#include <complex>

using std::cout;

int main() {
    // Boolean
    bool a = true, b = false;  
    
    cout << "and            " << (a and b) << "\n";
    cout << "&&             " << (a && b) << "\n";
    cout << "or             " << (a or b) << "\n";
    cout << "||             " << (a || b) << "\n";
    cout << "not            " << not (a or b) << "\n";
    cout << "!              " << !(a or b) << "\n";

    // Integral numbers
    cout << "char           " << sizeof(char) << "\n";
    cout << "short int      " << sizeof(short int) << "\n";
    cout << "int            " << sizeof(int) << "\n";
    cout << "long           " << sizeof(long) << "\n";

    // Floating point numbers
    cout << "float          " << sizeof(float) << "\n";
    cout << "double         " << sizeof(double) << "\n";
    cout << "long double    " << sizeof(long double) << "\n";
    cout << "complex double " << sizeof(std::complex<double>) << "\n";
    
    // Characters and strings
    char c = 'a'; // Note single quotes
    char word[] = "hello"; // C char arrays
    std::string s = "hello"; // C++ string
    
    cout << c << "\n";
    cout << word << "\n";
    cout << s << "\n";   
}