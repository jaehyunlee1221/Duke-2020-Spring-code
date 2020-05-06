
#include <iostream>
#include <armadillo>

using std::cout;
using std::endl;
        
int main() 
{
    using namespace arma;

    vec x = linspace<vec>(10,15,10);
    vec e = 10*randn<vec>(10);
    vec y;
    y = 3*pow(x,2)-7*x+2+e;
    vec xl = sqrt(x.t()*x);
    vec yl = sqrt(y.t()*y);
    vec d = x-y;
    vec dist = sqrt(d.t()*d);
    vec cov = (x-mean(x)).t()*(y-mean(y))/10;
    vec cor = cov/(stddev(x)*stddev(y));
    mat X;
    X = join_rows(ones(10),join_rows(x,pow(x,2)));
    vec b = inv(X.t()*X)*X.t()*y;
    
    cout << "\nVecotrs in Armadillo\n";
    cout <<"x:" << x << endl;
    cout <<"y:" << y << endl;
    cout <<"x's len:" << xl << endl;
    cout <<"y's len:" << yl << endl;
    cout <<"dist:" << dist << endl;
    cout <<"cor:" << cor << endl;
    cout <<"X:" << X << endl;
    cout <<"Coeff:"<< b <<endl;
}