
#include <fstream>
#include "my_math.hpp"

int main() {
    std::ifstream fin("data.txt");
    std::ofstream fout("result.txt");
    
    double a, b;
    
    fin >> a >> b;
    fin.close();
    
    fout << add(a, b) << std::endl;
    fout << multiply(a, b) << std::endl;
    fout.close();
}