
#include<iostream>
#include <vector>
#include <functional>
using std::cout;
using std::endl;
using std::function;
using std::vector;
        
auto f(double x){
    
};
auto grad(double x){
    
};
auto root(double x){
    using func = function<double(double)>;
    auto f = [](double y) { return y*y*y - 7*y - 6; };
    auto grad = [](double z) { return 3*z*z - 7; };
    double r = x;
    double a = 0.5;
    int iter = 10000;
    for(int i=0; i<iter; i++){
        r = r - a*f(r)/grad(r);
    }
    return int(r);
}
int main(){
    double x[] = {-5,-4,-3,-2,-1,0,1,2,3,4,5};
    int s = sizeof(x)/sizeof(x[0]);
    double xs[s];
    for(int i=0; i<s; i++){
        xs[i] = root(x[i]);
            }
    for (int i=0; i<s; i++)
    {
        int j;
        for (j=0; j<i; j++)
           if (xs[i] == xs[j])
               break;
         if (i == j)
          cout << xs[i] << " ";
    }
}