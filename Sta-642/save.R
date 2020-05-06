```{r AR(p) function}
AR_p <- function(x, p){
  
  #zero mean
  data <- x - mean(x)
  #number of observation
  n_data <- length(x)
  
  y <- data[n_data:(p+1)]
  n <- length(y)
  
  library(orthogonalsplinebasis)
  #design matrix
  X <- Hankel(x = data[(n_data-1):1],
              nrow = n_data - p,
              ncol = p)
  B <- t(X) %*% X
  library(pracma)
  #point estimate of phi
  b <- mldivide(B, t(X) %*% y)
  #residual
  r <- c(rep(0,p), rev(y - X %*% b))
  #df
  nu <- n - p
  #Q(b) - SSR
  v = t(r) %*% r /nu
  
  return(list(B,b,r,nu,v))
}
```

```{r}
AR_p_sim <- function(x, p, B, b, nu, v, n_mc, n_pred){
  #sample v from post dist
  vsamp <- c(v) * nu/rchisq(n = n_mc, df = nu)
  v2samp <- sqrt(vsamp)
  
  #sample phi from post dist
  bsamp <- matrix(rep(b, n_mc), nrow = length(b)) +
    t(chol(x = solve(B))) %*%
    t(MASS::mvrnorm(n_mc, mu = rep(0,p), Sigma = diag(x = 1, nrow = p))) *
    matrix(data = t(rep(v2samp, p)),nrow = p)
  
  #sample future data from post predict dist
  x_end <- length(x)
  X <- matrix(data = rep(x[x_end:(x_end-p+1)],n_mc),ncol = n_mc)
  
  xpred <- t(
    MASS::mvrnorm(n = n_mc,
                  mu = rep(0,n_pred),
                  Sigma = diag(x = 1, nrow = n_pred))
  )
  
  for(t in 1:n_pred){
    xpred[t,] <- colSums(X*bsamp) + v2samp + xpred[t,]
    X <- rbind(xpred[t,], X)
    X <- X[-nrow(X),]
  }
  return(list(bsamp, vsamp, xpred))
}
```


test <- post_eigen(result$phi)
post_phi <- apply(result[["phi"]],2,mean)
post_phi2 <- apply(result2[["phi"]],2,mean)
G <- rbind(matrix(post_phi,nrow = 1),diag(x = length(post_phi)))[1:length(post_phi),]
G2 <- rbind(matrix(post_phi2, nrow = 1),diag(x = length(post_phi2)))[1:length(post_phi2),]
lambda <- eigen(G)$values
lambda2 <- eigen(G2)$values
modulus <- Mod(lambda)
modulus2 <- Mod(lambda2)
modulus; modulus2

comp_lambda <- lambda[abs(Re(lambda)) != modulus]

angle <- Arg(comp_lambda)

wavelength <- 2*pi/angle

table <- data.frame(cbind(unique(Mod(comp_lambda)),unique(abs(wavelength))))

colnames(table) <- c("modulus","wavelength"); row.names(table) <- c("comp1","comp2","comp3")

kable(table, caption = "quasi-periodic component")

table
