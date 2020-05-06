
#include <iostream>
#include <armadillo>

using std::cout;
using std::endl;
        
int main() 
{
    using namespace arma;

    vec u = linspace<vec>(0,1,5);
    vec v = ones<vec>(5);
    mat A = randu<mat>(4,5); // uniform random deviates
    mat B = randn<mat>(4,5); // normal random deviates

    cout << "\nVecotrs in Armadillo\n";
    cout << u << endl;
    cout << v << endl;
    cout << u.t() * v << endl;

    cout << "\nRandom matrices in Armadillo\n";
    cout << A << endl;
    cout << B << endl;
    cout << A * B.t() << endl;
    cout << A * v << endl;

    cout << "\nQR in Armadillo\n";
    mat Q, R;
    qr(Q, R, A.t() * A);
    cout << Q << endl;
    cout << R << endl;
}