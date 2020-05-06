# STA-663-Final-Project

Authors: Jingxuan Zhang, Jae Hyun Lee

This is package for final project in STA-663 Python programming course.

This package can be used by install `STA-663-Final-Project-SSVD` from pypi

In this package, ssvd_opt function is included which is implementation of paper Biclustering via Sparse Singular Value Decomposition by Lee et al. 

ssvd_opt finds latent association between columns and rows under High Dimension Low Sample Size. This function is optimized with functions from `Numba`. Therefore, before execution, you should import `njit`, `prange`, `jit` from `Numba`.

# ssvd_opt
input: X, niter

X - data matrix which has much more columns than rows  

niter - Number of max iteration

output: u,s,v,iters  

u,s,v - Decomposed singular value and singular vectors with adaptive lasso penalty which corresponds to best rank 1 approximation matrix X*  = s*u@v.T.
        
iters - number of iteration before convergence.

if the algorithm fails to converge, it prints out "need to increase niter!"
