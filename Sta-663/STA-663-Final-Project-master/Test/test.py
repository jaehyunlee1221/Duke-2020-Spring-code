from SSVD import ssvd_opt
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np

def biclusterplot(u,s,v,ax,title = "title"):
    u = np.sort(u)/np.max(np.abs(u))
    v = np.sort(v)/np.max(np.abs(v))
    X = s*np.outer(u,v)
    sns.heatmap(X, cmap ="RdBu",vmin = -1,vmax = 1, ax = ax).set_title(title)

u_til = np.r_[np.arange(3,11)[::-1], 2*np.ones(17), np.zeros(75)].reshape(-1,1)
u_til = u_til/np.linalg.norm(u_til)
v_til = np.r_[np.array([10,-10,8,-8,5,-5]),3*np.ones(5),-3*np.ones(5),np.zeros(34)].reshape(-1,1)
v_til = v_til/np.linalg.norm(v_til)
s = 50
X_sim = s*u_til@v_til.T
n,p = X_sim.shape
X_sim = X_sim + np.random.randn(n,p)

fig, axes = plt.subplots(1,2,figsize=(15,5))
u1, s1, v1, iters = ssvd_opt(X_sim)
sns.heatmap(X_sim, cmap ="RdBu",vmin = -1,vmax = 1,ax=axes[0]).set_title("Original Simulated data")
biclusterplot(u1,s1,v1,ax = axes[1],title = "bicluster by ssvd")
