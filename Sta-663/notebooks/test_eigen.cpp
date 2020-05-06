#include <iostream>
#include <fstream>
#include <random>
#include <Eigen/Dense>
#include <functional>

using std::cout;
using std::endl;
using std::ofstream;
        
using std::default_random_engine;
using std::normal_distribution;
using std::bind;
        
// start random number engine with fixed seed
default_random_engine re{12345};

normal_distribution<double> norm(5,2); // mean and standard deviation
auto rnorm = bind(norm, re);  
                              
int main() 
{
    using namespace Eigen;

    VectorXd x1(6);
    x1 << 1, 2, 3, 4, 5, 6;
    VectorXd x2 = VectorXd::LinSpaced(6, 1, 2);
    VectorXd x3 = VectorXd::Zero(6);
    VectorXd x4 = VectorXd::Ones(6);
    VectorXd x5 = VectorXd::Constant(6, 3);
    VectorXd x6 = VectorXd::Random(6);
    
    double data[] = {6,5,4,3,2,1};
    Map<VectorXd> x7(data, 6);

    VectorXd x8 = x6 + x7;
    
    MatrixXd A1(3,3);
    A1 << 1 ,2, 3,
          4, 5, 6,
          7, 8, 9;
    MatrixXd A2 = MatrixXd::Constant(3, 4, 1);
    MatrixXd A3 = MatrixXd::Identity(3, 3);
    
    Map<MatrixXd> A4(data, 3, 2);
    
    MatrixXd A5 = A4.transpose() * A4;
    MatrixXd A6 = x7 * x7.transpose();
    MatrixXd A7 = A4.array() * A4.array();
    MatrixXd A8 = A7.array().log();
    MatrixXd A9 = A8.unaryExpr([](double x) { return exp(x); });
    MatrixXd A10 = MatrixXd::Zero(3,4).unaryExpr([](double x) { return rnorm(); });

    VectorXd x9 = A1.colwise().norm();
    VectorXd x10 = A1.rowwise().sum();
    
    MatrixXd A11(x1.size(), 3);
    A11 << x1, x2, x3;
    
    MatrixXd A12(3, x1.size());
    A12 << x1.transpose(),
          x2.transpose(),
          x3.transpose();
    
    JacobiSVD<MatrixXd> svd(A10, ComputeThinU | ComputeThinV);
    
    
    cout << "x1: comman initializer\n" << x1.transpose() << "\n\n";
    cout << "x2: linspace\n" << x2.transpose() << "\n\n";
    cout << "x3: zeors\n" << x3.transpose() << "\n\n";
    cout << "x4: ones\n" << x4.transpose() << "\n\n";
    cout << "x5: constant\n" << x5.transpose() << "\n\n";
    cout << "x6: rand\n" << x6.transpose() << "\n\n";
    cout << "x7: mapping\n" << x7.transpose() << "\n\n";
    cout << "x8: element-wise addition\n" << x8.transpose() << "\n\n";
    
    cout << "max of A1\n";
    cout << A1.maxCoeff() << "\n\n";
    cout << "x9: norm of columns of A1\n" << x9.transpose() << "\n\n";
    cout << "x10: sum of rows of A1\n" << x10.transpose() << "\n\n"; 
    
    cout << "head\n";
    cout << x1.head(3).transpose() << "\n\n";
    cout << "tail\n";
    cout << x1.tail(3).transpose() << "\n\n";
    cout << "slice\n";
    cout << x1.segment(2, 3).transpose() << "\n\n";
    
    cout << "Reverse\n";
    cout << x1.reverse().transpose() << "\n\n";
    
    cout << "Indexing vector\n";
    cout << x1(0);
    cout << "\n\n";
    
    cout << "A1: comma initilizer\n";
    cout << A1 << "\n\n";
    cout << "A2: constant\n";
    cout << A2 << "\n\n";
    cout << "A3: eye\n";
    cout << A3 << "\n\n";
    cout << "A4: mapping\n";
    cout << A4 << "\n\n";
    cout << "A5: matrix multiplication\n";
    cout << A5 << "\n\n";
    cout << "A6: outer product\n";
    cout << A6 << "\n\n";    
    cout << "A7: element-wise multiplication\n";
    cout << A7 << "\n\n";     
    cout << "A8: ufunc log\n";
    cout << A8 << "\n\n";  
    cout << "A9: custom ufucn\n";
    cout << A9 << "\n\n";  
    cout << "A10: custom ufunc for normal deviates\n";
    cout << A10 << "\n\n";  
    cout << "A11: np.c_\n";
    cout << A11 << "\n\n";  
    cout << "A12: np.r_\n";
    cout << A12 << "\n\n";      
    
    cout << "2x2 block startign at (0,1)\n";
    cout << A1.block(0,1,2,2) << "\n\n";
    cout << "top 2 rows of A1\n";
    cout << A1.topRows(2) << "\n\n";
    cout << "bottom 2 rows of A1";
    cout << A1.bottomRows(2) << "\n\n";
    cout << "leftmost 2 cols of A1";
    cout << A1.leftCols(2) << "\n\n";
    cout << "rightmost 2 cols of A1";
    cout << A1.rightCols(2) << "\n\n";

    cout << "Diagonal elements of A1\n";
    cout << A1.diagonal() << "\n\n";
    A1.diagonal() = A1.diagonal().array().square();
    cout << "Transforming diagonal eelemtns of A1\n";
    cout << A1 << "\n\n";
    
    cout << "Indexing matrix\n";
    cout << A1(0,0) << "\n\n";
    
    cout << "singular values\n";
    cout << svd.singularValues() << "\n\n";
    
    cout << "U\n";
    cout << svd.matrixU() << "\n\n";
    
    cout << "V\n";
    cout << svd.matrixV() << "\n\n";
}