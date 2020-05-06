#define STATS_ENABLE_STDVEC_WRAPPERS
#define STATS_ENABLE_ARMA_WRAPPERS
// #define STATS_ENABLE_EIGEN_WRAPPERS
#include <iostream>
#include <vector>
#include "stats.hpp"

using std::cout;
using std::endl;
using std::vector;

// set seed for randome engine to 1776
std::mt19937_64 engine(1776);
        
int main() {
    // evaluate the normal PDF at x = 1, mu = 0, sigma = 1
    double dval_1 = stats::dnorm(1.0,0.0,1.0);

    // evaluate the normal PDF at x = 1, mu = 0, sigma = 1, and return the log value
    double dval_2 = stats::dnorm(1.0,0.0,1.0,true);

    // evaluate the normal CDF at x = 1, mu = 0, sigma = 1
    double pval = stats::pnorm(1.0,0.0,1.0);

    // evaluate the Laplacian quantile at p = 0.1, mu = 0, sigma = 1
    double qval = stats::qlaplace(0.1,0.0,1.0);

    // draw from a normal distribution with mean 100 and sd 15
    double rval = stats::rnorm(100, 15);
    
    // Use with std::vectors
    vector<int> pois_rvs = stats::rpois<vector<int> >(1, 10, 3);
    cout << "Poisson draws with rate=3 inton std::vector" << endl;
    for (auto &x : pois_rvs) {
        cout << x << ", ";
    }
    cout << endl;
    
    
    // Example of Armadillo usage: only one matrix library can be used at a time
    arma::mat beta_rvs = stats::rbeta<arma::mat>(5,5,3.0,2.0);
    // matrix input
    arma::mat beta_cdf_vals = stats::pbeta(beta_rvs,3.0,2.0);
    
    /* Example of Eigen usage: only one matrix library can be used at a time
    Eigen::MatrixXd gamma_rvs = stats::rgamma<Eigen::MatrixXd>(10, 5,3.0,2.0);
    */
    
    cout << "evaluate the normal PDF at x = 1, mu = 0, sigma = 1" << endl;
    cout << dval_1 << endl;    

    cout << "evaluate the normal PDF at x = 1, mu = 0, sigma = 1, and return the log value" << endl;
    cout << dval_2 << endl;
    cout << "evaluate the normal CDF at x = 1, mu = 0, sigma = 1" << endl;
    cout << pval << endl;

    cout << "evaluate the Laplacian quantile at p = 0.1, mu = 0, sigma = 1" << endl;
    cout << qval << endl;

    cout << "draw from a normal distribution with mean 100 and sd 15" << endl;
    cout << rval << endl;
    
    
    
    cout << "draws from a beta distribuiotn to populate Armadillo matrix" << endl;
    cout << beta_rvs << endl;
    
    cout << "evaluaate CDF for beta draws from Armadillo inputs" << endl;
    cout << beta_cdf_vals << endl;
    
    /*  If using Eigen
    cout << "draws from a Gamma distribuiotn to populate Eigen matrix" << endl;
    cout << gamma_rvs << endl;
    */
}