import pandas as pd
import numpy as np
from numba import jit
from numba import njit, prange

@njit(parallel=True)
def updateU(Bu,idx,n,d,pu,tu,wu,z,v1,X,sigsq):
    for i in prange(0,pu):
        luc = tu[idx[i]]
        plambda = luc/wu[wu!=0]
        tmp= np.sign(z[wu!=0])*(np.abs(z[wu!=0])>=plambda)  
        uc = tmp*(np.abs(z[wu!=0])-plambda)
        Bu[i] = np.sum((X - np.outer(uc,v1))**2)/sigsq + (i+1)*np.log(n*d) #this works best
        
    return Bu
    
@njit(parallel=True)
def updateV(Bv,idx,n,d,pv,tv,wv,z,u0,X,sigsq):
    for i in prange(0,pv):
        lvc = tv[idx[i]]
        plambda = lvc/wv[wv!=0]
        tmp= np.sign(z[wv!=0])*(np.abs(z[wv!=0])>=plambda)                         
        vc = tmp*(np.abs(z[wv!=0])-plambda)
        Bv[i] = np.sum((X - np.outer(u0,vc))**2)/sigsq + (i+1)*np.log(n*d) #this works best
        
    return Bv
    
@jit(nopython = True)
def svd(X):
    return np.linalg.svd(X)
 
@njit(parallel = True)
def ssvd_opt(X, niter = 100):
    n, d = X.shape
    ttypu = 1; ttypv = 1
    gamu = 0; gamv = 0
    u, s, v = svd(X)
    v = v.T
    u0 = u[:,0]; v0 = v[:,0]
    tol = 1e-4
    ud = 1;vd = 1
    SST = np.sum(X**2)
    for iters in prange(niter):
        zv = X.T@u0
        wv = np.abs(zv)**gamv
        sigsq = (SST - np.sum(zv**2))/(n*d-d)
        tv = np.sort(np.append(np.abs(zv**wv),0))
        pv = np.sum(tv>0)
        Bv = np.ones(d+1)*np.Inf
        idx = np.arange(d-1,-1,-1)
        Bv = updateV(Bv,idx,n,d,pv,tv,wv,zv,u0,X,sigsq)
        Iv = np.argmin(Bv) + 1
        lv = tv[d-Iv]
        plambdav = lv/wv[wv!=0]
        tmpv= np.sign(zv[wv!=0])*(np.abs(zv[wv!=0])>=plambdav)
        v1 = tmpv*(np.abs(zv[wv!=0])-plambdav)   
        v1 = v1/np.sqrt(np.sum(v1**2)) #v_new
        zu = X@v1
        wu = np.abs(zu)**gamu
        sigsq = (SST - np.sum(zu**2))/(n*d-n)
        tu = np.sort(np.append(np.abs(zu**wu),0))
        pu = np.sum(tu>0)
        Bu = np.ones(n+1)*np.Inf
        idx = np.arange(n-1,-1,-1)
        Bu = updateU(Bu,idx,n,d,pu,tu,wu,zu,v1,X,sigsq)
        Iu = np.argmin(Bu) + 1
        lu = tu[n-Iu]
        plambdau = lu/wu[wu!=0]
        tmpu= np.sign(zu[wu!=0])*(np.abs(zu[wu!=0])>=plambdau)  
        u1 = tmpu*(np.abs(zu[wu!=0])-plambdau)    
        u1 = u1/np.sqrt(np.sum(u1**2)) #u_new
        ud = np.sqrt(np.sum((u0-u1)**2))
        vd = np.sqrt(np.sum((v0-v1)**2)) 
        if ((vd<tol) and (ud<tol)):
            break
        elif iters == niter -1:
            print('need to increase niter!')
        u0 = u1
        v0 = v1
    u = u1
    v = v1
    return u,s[0],v,iters
