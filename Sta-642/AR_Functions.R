#functions 
library(tidyverse)


AR1simulate <- function(phi, s, n){
  #phi - AR coefficient, s - marginal variance of innovation, n - simulation time
  
  rv <- sqrt((1-phi^2)*s)   # s = v/(1-phi*2) -> v = s*(1-phi^2)
  x <- rnorm(n, 0, 1)       # normal innovation
  x[1] <- x[1] * sqrt(s)    # Marginal dist x_1 ~ N(0,s)
  x[2:n] <- x[2:n] * rv     # Conditional dist x_t ~ N(phi*x_{t-1},v)
  
  for(t in 2:n){
    x[t] <- phi*x[t-1] + x[t]
  }
  print(ggplot2::ggplot(mapping = aes(x = 1:length(x),y = x)) +
          geom_line()+
          ggplot2::labs(x = "Time t", y = "x_t"))
  return(x)
}

AR <- function(x, p){
  #x - data, p - order
  x <- unlist(x)
  dat <- x - mean(x)                                #zero-mean
  ndat <- length(x)                                 #number of observation
  y <- dat[ndat:(p+1)]                              #y_t to y_t-p+1
  n <- length(y)                                    #number of observation in target variable
  X <- matrix(rep(NA,p*n), ncol = p)                #Design matrix - ith row is y_{t-i} ... y_{t-p+i}
  for(i in 1:n) X[i,] <- dat[(ndat-i):(ndat-p-i+1)]
  B <- t(X) %*% X                                   #post precision of phi -> sum of x_i^2 / v
  b <- solve(B) %*% t(X) %*% y                      #MLE and post mean of phi
  r <- c(rep(0,p),rev(y - X %*% b))                 #residual
  nu <- n - p                                       #df
  v <- t(r) %*% r/nu                                #post mean of innovation variance
  return(list(b = b, B = B, r = r, nu = nu, v = v, H = X))
}

ARsim <- function(x, p, b, B, nu, v, nmc, npred){
  #x - data, p - order, b - post mean of phi, nu - df, 
  #v - df, nmc - simluate number, npred - number of prediction
  
  #step1 - sample variance of innovation
  vsamp  <- c(v)*nu/rchisq(n = nmc, df = nu)        #v = SSR(v*nu = r'r)/k where k~chisq(df=n-2)
  v2samp <- sqrt(vsamp)                             #sampled sd of innovation
  
  #step2 - sample phi given above variance
  bsamp  <- matrix(rep(b,nmc),ncol = nmc) +         #phi ~ N(b,v*inv(B)) -> mean vector
           chol(solve(B)) %*% t(MASS::mvrnorm(nmc, mu = rep(0,p), Sigma = diag(x = 1, nrow = p))) + #(inv(B)^{1/2} I_p)*nmc
           matrix(t(rep(v2samp,p)),ncol = nmc)      #simulated v above
  
  #step3 - step ahead prediction using above parameters
  x <- unlist(x); n <- length(x)
  X <- matrix(rep(x[n:(n-p+1)], nmc), ncol = nmc)   #Current y
  Xpred <- t(
    MASS::mvrnorm(n = nmc,
                  mu = rep(0,npred),
                  Sigma = diag(1, nrow = npred))    #Normal innovation ~ N(0,1)
  )
  for(t in 1:npred){
    Xpred[t,] <- colSums(X * bsamp) + v2samp * Xpred[t,]
    X <- rbind(Xpred[t,],X)
    X <- X[-nrow(X),]
  }
  return(list(bsamp = bsamp, vsamp = vsamp, xpred = xpred))
}

ARdecomp <- function(x, p){
  
  #organize data
  x <- unlist(x); x <- x - mean(x); mi <- min(x); mx <- max(x); ndat <- length(x); y <- x[ndat:(p+1)]
  #Fit AR process
  X <- AR(x,p)$H
  b <- AR(x,p)$b
  r <- AR(x,p)$r
  nu <- AR(x,p)$nu
  s <- AR(x,p)$v
  
  #start decomposition
  G <- rbind(matrix(b,nrow = 1),diag(x = 1, nrow = p))[1:p,] #evolution matrix
  l <- eigen(G)$values; e <- eigen(G)$vectors;               #Eigendecompose G
  mod <- Mod(l); arg <- Arg(l)                               #Decompose eigenvalue by modulus and angle
  H <- diag(e[1,]) %*% solve(e)                              #F0 = E'F, Z = inv(E)x_t where x_t = (y_t,y_{t-1},...,y_{t-p}) 
                                                             #y_t = F0_1Z_1 + F0_2Z_2 ... F0_pZ_p
                                                             #H = diag(F_0) * inv(E) -> y_t = H*x_t
  
  #reorder eigenvalue by modulus and remove negative arg(for preventing duplicated component)
  arg[arg == 0] <- pi
  i <- order(arg); mod <- mod[i]; arg <- arg[i]; H <- H[i,]  #order by arg for removing imaginary term
  nc <- sum(arg < 0)
  i <- arg>0; nk <- sum(i); mod <- mod[i]; arg <- arg[i]; H <- H[i,] #keep only real term
  H[1:nc,] <- 2*Re(H[1:nc,]);                                #multiply by 2 -> sum of conjugate complex is 2*real term. 
  decomp <- H %*% t(rbind(x[ndat:(ndat-p+1)],X, matrix(0, nrow = p-1, ncol = p)))
  decomp <- decomp[,ndat:1]
  waves <- 2*pi/arg
  return(list(decomp = decomp, mod = mod, waves = waves))
}

ARdecomp_sim <- function(x, p, nmc){
  #x - data, p - order, nmc <- number of simulation
  
  #organize data
  x <- unlist(x); x <- x - mean(x); mi <- min(x); mx <- max(x); ndat <- length(x); y <- x[ndat:(p+1)]
  #Fit AR process
  X <- AR(x,p)$H
  b <- AR(x,p)$b
  r <- AR(x,p)$r
  nu <- AR(x,p)$nu
  s <- AR(x,p)$v
  #simulate posterior phi
  bsamp <- ARsim(x = x, p = p, b = b, B = AR(x,p)$B, nu = nu, v = s, nmc = nmc, npred = 100)$bsamp
  #allocate simluation
  modsamp <- wavesamp <- matrix(rep(0,p*nmc), ncol = nmc); desamp <- array(rep(0,ndat*p*nmc),dim = c(ndat,p,nmc))
  ncompmax <- 0
  #repeat decomposition
  for(i in 1:nmc){
    G <- rbind(matrix(bsamp[,i],nrow = 1),diag(x = 1, nrow = p))[1:p,]
    l <- eigen(G)$values; e <- eigen(G)$vectors;               
    mod <- Mod(l); arg <- Arg(l)                               
    H <- diag(e[1,]) %*% solve(e)                              
    arg[arg == 0] <- p
    i <- order(arg); mod <- mod[i]; arg <- arg[i]; H <- H[i,]  
    nc <- sum(arg < 0)
    i <- arg>0; nk <- sum(i); mod <- mod[i]; arg <- arg[i]; H <- H[i,]
    ncompmax <- max(nk,ncompmax)
    H[1:nc,] <- 2*Re(H[1:nc,]);                                 
    decomp <- H %*% t(rbind(x[ndat:(ndat-p+1)],X, matrix(0, nrow = p-1, ncol = p)))
    decomp <- decomp[,ndat:1]
    waves <- 2*pi/arg
    modsamp[1:nk,i] <- mod; wavesamp[1:nk,i] <- waves; desamp[,1:nk,i] <- decomp
  }
  modsamp <- modsamp[1:ncompmax,]; wavesamp <- wavesamp[1:ncompmax,]; desamp <- desamp[1:ncompmax,]
  return(list(modsamp = modsamp, wavesamp = wavesamp, desamp = desamp))
}

decomp_plot <- function(x, decomp, k){
  #plot x with first k component latent series
  #organize data
  x <- unlist(x); x <- x - mean(x); mi <- min(x); mx <- max(x); ndat <- length(x);
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
    plot_list[[i]] <- ggplot2::ggplot(data = plot_df, mapping = aes_string(x = 1:nrow(plot_mat), y = i)) +
      geom_line()
  }
  
  gridExtra::grid.arrange(grobs = plot_list,
                          nrow = ncol(plot_df)
  )
}
