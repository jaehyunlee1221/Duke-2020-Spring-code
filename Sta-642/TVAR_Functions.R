#functions

tvar <- function(x, p, del, m0, C0, s0, n0){
  #Fit TVAR(p) model to univariate series x
  #Dicount factors del(1) for state and del(2) for obs varaince
  #input:
  #- x = data
  #- p = model order
  #- m0 = px1 vector prior mean for state
  #- C0 = pxp prior varaince matrix for state
  #- n0 = prior df
  #- s0 = prior estimate of obs variance
  #output:
  #for forward filter
  #- mf = pxT post mean vectors for state
  #- Cf = pxpxT post variance matrix for state
  #- nf = T post df
  #- sf = T post obs variance estimate
  #- ef = 1-step forecast error(x_t - f_t)
  #- qf = 1-step forecast variance factors(variance of y_t+1 given D_t) (zero to t = p)
  #for backward smooth
  #- m = pxT post mean vectors for state smoothed
  #- C = pxpxT post variance matrix for state smoothed
  #- n = T post df smoothed
  #- s = T post obs variance estimate smoothed
  #- e = estimated innovation (zero to t = p)
  
  #organize data
  d <- del[1]; b <- del[2]
  x <- unlist(x); x <- x - mean(x)
  T <- length(x)
  m <- matrix(rep(m0,T),nrow = p); C <- array(rep(C0, prod(dim(C0))*T),dim = c(dim(C0),T))
  s <- rep(s0,T); n <- rep(n0,T)
  e <- rep(0,T); q <- rep(0,T);
  mt <- m0; Ct <- C0; st <- s0; nt <- n0;
  
  #forward filtering
  for(t in (p+1):T){
    F = x[(t-1):(t-p)]
    A <- Ct %*% F/d; qt <- t(F) %*% A +st; A <- A/c(qt); et <- x[t] - F%*%mt; e[t] <- et; q[t] <- qt
    mt <- mt + A %*% et; m[,t] <- mt
    r <- b*nt + et*et/qt; nt <- b*nt + 1; r <- r/nt; st <- st*r
    n[t] <- nt; s[t] <- st
    Ct <- c(r)*(Ct/d - A%*%t(A)*c(qt)); Ct = (Ct+t(Ct))/2
    C[,,t] <- Ct
  }
  #save simulated values 
  mf <- m; Cf <- C; sf <- s; nf <- n; ef <- e; qf <- q
  
  #backward smoothing
  for(t in (T-1):1){
    m[,t] <- (1-d)*m[,t] + d*m[,(t+1)]
    if(t > p){
      e[t] <- x[t] - t(m[,t]) %*% x[(t-1):(t-p)]
    }
    n[t] <- (1-b)*n[t]+ b*n[t+1]
    st <- s[t]; s[t] <- 1/((1-b)/st + b/s[t+1])
    C[,,t] <- s[t] * ((1-d)*C[,,t]/st + d*d*C[,,(t+1)]/s[t+1])
  }
  #now ad-hoc treatment of 1st p values
  m[,1:p] <- matrix(rep(m[,p+1],p),ncol = p); C[,,1:p] <- array(rep(C[,,p+1],p),dim = c(dim(C[,,p+1]),p))
  n[1:p] <- rep(n[p+1],p); s[1:p] <- rep(s[p+1],p)
  return(list(mf=mf, Cf=Cf, nf=nf, sf=sf, ef=ef, qf=qf, m=m, C=C, n=n, s=s, e=e))
}

tvar_decomp <- function(x,m){
  #Decomposition of TVAR model based on post means in matrix m 
  #!note - we ignores issue of changing number of complex and real comps!
  #input:
  # x - series length T
  # m - pxT array of most means for state(smoothed)
  #output:
  #mods - pxT matrix of moduli of all comps
  #waves  -- floor(p/2)xT matrix of wavelengths of ARMA(2,1) components
  #decomp - pxT trajectories of all components
  #maxnr - max number of real comps
  #maxnc - max number of complex comps
  
  p <- dim(m)[1]; T <- dim(m)[2]
  x <- unlist(x)
  decomp <- waves <- mods <- matrix(rep(0,p*T),nrow = p);
  G <- rbind(matrix(m[,1],nrow = 1),diag(x = 1, nrow = p))[1:p,]
  maxnr <- maxnc <- 0
  
  #start decomposition
  for(t in (p+1):T){
    G[1,] <- m[,t]; e <- eigen(G)$vectors; l <- eigen(G)$values; arg <- Arg(l); mod <- Mod(l) 
    H <- diag(e[1,])%*%solve(e)
    
    #reorder roots in terms of 
    #-decreasing wavelengths(for complex) and remove neg args
    #-followed by reals in terms of decreasing moduli
    arg[arg==0] <- pi; i <- order(arg); arg <- arg[i];mod <- mod[i]; H <- H[i,];
    i <- arg<0; nc <- sum(i); nr = p-2*nc; nk = nc+nr;
    #remove components that has negative angle
    arg <- arg[!i]; mod <- mod[!i]; H <- H[!i,]
    H <- Re(H); H[1:nc, ] = 2*H[1:nc,]
    #decomposed series
    decomp[1:nk,t] <- H[1:nk,] %*% x[(t-1):(t-p)]
    waves[1:nc,t] <- 2*pi/arg[1:nc]
    mods[1:nk,t] <- mod[1:nk]
    maxnr <- max(nr,maxnr); maxnc <- max(nc,maxnc)
  }
  #ad-hoc treatment of 1st pvalues
  waves[,1:p] <- matrix(rep(waves[,(p+1)],p), ncol = p)
  mods[,1:p] <- matrix(rep(mods[,(p+1)],p), ncol = p)
  #trim down the output arrays
  waves <- waves[1:maxnc,]
  mods <- mods[1:min(p,(maxnc+maxnr)),]
  decomp <- decomp[1:min(p, (maxnc+maxnr)),]
  return(list(waves = waves, mods = mods, decomp = decomp, nr = nr, nc = nc))
}

decomp_plot <- function(x, decomp, k){
  require(ggplot2)
  #plot x with first k component latent series
  #organize data
  x <- unlist(x); mi <- min(x); mx <- max(x); ndat <- length(x);
  d <- 1/(mx-mi); h <- 0.02 * ndat; cen <- mean(d*(x-mi))
  nk <- min(k, nrow(decomp))
  #allocate plot
  plot_mat <- matrix(data = NA, nrow = ndat, ncol = nk+1)
  plot_mat[,1] <- d*(x - mi) - cen
  for(i in 1:nk){
    plot_mat[,i+1] <- -cen + d * (Re(decomp[i, ]) - mi)
  }
  plot_df <- as.data.frame(plot_mat)
  colnames(plot_df) <- c("Data", paste("Comp", 1:nk, sep = ""))
  
  plot_list <- list()
  for(i in colnames(plot_df)){
    plot_list[[i]] <- ggplot2::ggplot(data = plot_df, mapping = aes_string(x = 1:nrow(plot_mat), y = i), col = i) +
      geom_line() +
      ylim(mi,mx)
  }
  
  gridExtra::grid.arrange(grobs = plot_list,
                          nrow = ncol(plot_df)
  )
}

tvarforecast <- function(x,K,I,p,del,mT,CT,sT,nT){
  #Forecast ahead in a TVAR(p) model of univariate series x
  #Discount factors del(1) for state and del(2) for obs var
  #Inputs:
  # x -  series of length T, defining data DT
  # K -  number of steps ahead to forecast from current time 
  # I -  number of Monte Carlo samples to give simulate futures
  # p -  model order
  # mT -  px1 vector posterior mean for state given DT
  # CT -  pxp posterior var matrix
  # nT -  posterior df
  # sT -  posterior estimate of obs var
  #Outputs: 
  # phi  --  pxKxI    sampled future state vectors 
  # mu   --  KxI      sampled means of future series ... F'phi 
  # v    --  KxI      sampled future volatilities 
  # y    --  KxI      sampled future series ... "synthetic futures"
  # nsc   --  Kx1      per step ahead, percent nonstationary draws 
  
  # Based on D_T, we use a fixed W evolution variance matrix
  # for additive state evolution each step ahead, and a fixed beta 
  # parameters for the beta innovations in the volatility model
   
  # organise data, initialize and define arrays for outputs ...
  x <- unlist(x);T <- length(x);
  d <- del[1]; b <- del[2]
  
  phi <- array(rep(0,p*K*I),dim = c(p,K,I)); v <- matrix(rep(0,K*I), ncol = I); mu <- v
  y <- matrix(rep(0, (K+p)*I),ncol = I); y[1:p,] <- matrix(rep(x[(T-p+1):T],I),ncol = I) #prepend last p data points
  nsc <- rep(0,K) #collect MC probability of nonstationary phi vectors at each step ahead
  
  gT <- b*nT/2; hT <- (1-b)*nT/2; #1-step beta shock parameters
  W <- CT*(1/d-1)                 #1-step evolution matrix
  L <- t(chol(W))                 #transposed Cholesky
  A <- cbind(diag(1, nrow = (p-1)),rep(0,(p-1)))
  phit <- matrix(rep(0,p*I),nrow = p); vt <- rep(0,I)
  
  # step-ahead forecasting: enforcing local stationarity on phit at each t
  # by sampling step-ahead priors and only accepting those that correspond
  # to locally stationary AR(p) 
  
  rho <- 0.99 #max abs(eigen) for TVAR state vector samples: constrain to locally stny
    
  #First, make initial draws from the 1-step ahead prior:
  uT <- nT/2; zT <- nT*sT/2; aT <- mT; RT <- CT+W; LT <- t(chol(RT)); imc <- 0
  vt <- 1/rgamma(I,uT,zT) #draw inverse gamma for volatility
  phit <- matrix(rep(aT,I),ncol = I) + matrix(rep(sqrt(vt/sT),p),nrow = p, byrow = T) * LT %*% matrix(rnorm(p*I), ncol = I)
  #Check nonstationarity
  stny <- apply(phit, 2, localstnytvar, p = p, rho = rho, A = A)
  phit <- do.call(cbind,lapply(stny,function(x){x$phi}))
  nsc <- sum(do.call(cbind,lapply(stny,function(x){x$ins})))
  
  #Now move over future time steps 1,2,3 .. k:
  for(j in 1:K){
    t <- p+j
    if(j>1){
      if(b<1){
        vt <- b*vt/rbeta(I,gT,hT)             #sample next volatility
      }
      #sample next state
      phit <- phit + matrix(rep(sqrt(vt/sT),p),nrow = p, byrow = T) * LT %*% matrix(rnorm(p*I), ncol = I)
      
      stny <- apply(phit, 2, localstnytvar, p = p, rho = rho, A = A)
      phit <- do.call(cbind,lapply(stny,function(x){x$phi}))
      nsc[j] <- sum(do.call(cbind,lapply(stny,function(x){x$ins})))
      
    }
    phi[,j,] <- phit; v[j,] <- vt
    #simulate next future:
    mu[j,] <- apply(y[(t-1):(t-p),]*phit,2,sum)
    y[t,] <- mu[j,] + sqrt(vt)*rnorm(I)       #and add innovations 
  }
  y <- y[-c(1:p),]; nsc = 100*nsc/I
  return(list(phi = phi, mu = mu, v = v, y = y, nsc = nsc))
}

localstnytvar <- function(p, phi, rho, A){
  require(useful)
  #Checks to see of an AR(p) coefficicent vector corresponds to stationarity
  #If not, modify it (via eignevalues) to be stationary
  #Inputs:
  # p    --  univariate TVAR model order
  # phi  --  px1 vector of TVAR coefficients  
  # rho  --  a number just less than 1 -- the max abs(eigenvalue); e.g., 0.99
  # A = [eye(p-1) zeros(p-1,1)];
  #Outputs: 
  # phi   --  px1 vector of TVAR coefficients 
  # ins   -- 1 if the input phi was nonstationary, 0 otherwise 
  ins <- 0
  eigs <- eigen(rbind(t(phi),A))$values; ab <- Mod(eigs); ie <- ab>=1
  if(sum(ie)>0){
    ins = 1; neweigs <- eigs; ab[ie] <- rho
    xy <- useful::pol2cart(ab[ie],Arg(eigs[ie]))[,c("x","y")]; neweigs[ie] <- xy$x + xy$y*1i
    #[ real(eigs) real(neweigs)  imag(eigs) imag(neweigs) ]
    #[ abs(eigs) abs(neweigs) angle(eigs)  angle(neweigs) ]
    phi <- -Re(pracma::Poly(neweigs))[-1] 
  }
  return(list(ins = ins, phi = phi))
} 

tvarFFBS <- function(x, p, del, m0, C0, s0, n0, nmc){
  # FFBS for TVAR(p) model of univariate series x
  # Discount factors del(1) for state and del(2) for obs var
  # Inputs:
  #   x --  series of length T
  #   p --  model order
  #  m0 --  px1 vector prior mean for state
  #  C0 --  pxp prior var matrix
  #  n0 --  prior df
  #  s0 --  prior estimate of obs var
  #  nmc--  Monte Carlo sample size
  # Outputs: 
  #  thetasamp --  pxTxnmc   posterior samples of TVAR state vectors
  #  vsamp     --   Txnmc       and observation variances
  # 
  # organise data ...
  d <- del[1]; b <- del[2]
  x <- unlist(x); x <- x - mean(x)
  T <- length(x)
  m <- matrix(rep(m0,T),ncol = T); C <- array(rep(C0,T), dim = c(dim(C0),T))
  s <- rep(s0,T); n <- rep(n0,T);
  mt <- m0; Ct <- C0; st <- s0; nt <- n0;
  
  #forward filtering
  for(t in (p+1):T){
    F = x[(t-1):(t-p)]
    A <- Ct %*% F/d; q <- t(F) %*% A +st; A <- A/c(q); e <- x[t] - F%*%mt
    mt <- mt + A %*% e; m[,t] <- mt
    r <- b*nt + e*e/q; nt <- b*nt + 1; r <- r/nt; st <- st*r
    n[t] <- nt; s[t] <- st
    Ct <- c(r)*(Ct/d - A%*%t(A)*c(q)); Ct = (Ct+t(Ct))/2
    C[,,t] <- Ct
  }
  #backward sampling
  thetasamp <- array(rep(0,p*T*nmc),dim = c(p,T,nmc)); vsamp <- matrix(rep(0,T*nmc),ncol = nmc)
  
  #First, sample at end time T: !NOTE SAMPLING ALL NMC AT ONCE - EFFICIENT
  vt <- 1/rgamma(nmc,n[T]/2, (n[T]*s[T])/2)
  e <- matrix(rnorm(p*nmc),ncol = nmc) * matrix(rep(vt/s[T],p),ncol = nmc, byrow = T)#scaled normals underlying p(theta|v)
  thetat <- matrix(rep(m[,T],nmc),ncol = nmc) + t(chol(C[,,T])) %*% e
  vsamp[T,] <- vt; thetasamp[,T,] <- thetat      #save
  
  #then, recurse back sampling each time point conditional on the previous values
  for(t in (T-1):(p+1)){
    if(b<1){
      vt <- b/vt + rgamma(nmc,(1-b)*n[t]/2, (n[t]*s[t])/2); vt = 1/vt
    }
    ht <- matrix(rep((1-d)*m[,t],nmc), ncol = nmc) + d*thetat                   #conditional mean vector, expanded nmc time
    Ht <- (1-d)*C[,,t]                                                                   #conditional variance matrix
    e  <- matrix(rnorm(p*nmc),ncol = nmc) * matrix(rep(vt/s[T],p),ncol = nmc, byrow = T) #underlying scaled normals
    thetat <- ht + t(chol(Ht)) %*% e
    thetasamp[,t,] <- thetat; vsamp[t,] <- vt                                            #save
  }
  #now need to add something for ad-hoc treatment of 1st p values..?
  for(t in 1:p){
    thetasamp[,t,] <- thetat; vsamp[t,]<-vt
  }
  return(list(thetasamp = thetasamp, vsamp = vsamp))
}

