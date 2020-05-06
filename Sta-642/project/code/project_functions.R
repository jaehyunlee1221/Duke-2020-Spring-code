VAR <- function(x,p, type = NULL){
  x <- as.matrix(x)
  n <- dim(x)[1] #number of time point
  q <- dim(x)[2] #number of series
  
  X <- cbind(matrix(rep(1,(n-p)),nrow = n-p),seq(1,n-p,1))
  for(j in 0:(p-1)){
    X <- cbind(X,x[(p-j):(n-1-j),])
  }
  if(is.null(type)){
    b <- matrix(rep(NA,(q)*q*p),nrow = q)
    X <- X[,-(1:2)]
  }
  else if(type == "c"){
    b <- matrix(rep(NA,q*q*p+q),nrow = q)
    X <- X[,-2]
  }
  else if(type == "t"){
    b <- matrix(rep(NA,q*q*p+q),nrow = q)
    X <- X[,-1]
  }
  else if(type == "b"){
    b <- matrix(rep(NA,q*q*p+2*q),nrow = q)
  }
  B <- t(X) %*% X  
  for(i in 1:q){
    y <- x[-(1:p),i]
    b[i,] <- solve(B) %*% t(X) %*% y
  }
  return(list(b = b, B = B))
}



Tbeta <- function(data,predictors){
  ti <- sort(unique(data$date))
  T <- length(ti)
  p <- length(predictors)
  #allocate return objects
  betas <- matrix(rep(NA,T*(p+1)),ncol = p+1)
  sigmas <- matrix(rep(NA,T))
  n = matrix(rep(NA,T))
  resid <- list()
  cor_mat <- list()
  form <- as.formula(paste("lprice",paste0(predictors,collapse = "+"),sep = "~"))
  #fit linear regression model each time 
  for(i in 1:T){
    idx <- data$date == ti[i] 
    newdata <- data[idx,]
    model <- lm(data = newdata, form)
    betas[i,] <- model$coefficients
    sigmas[i] <- summary(model)$sigma^2
    n[i] <- nrow(newdata)
    resid[[i]] <- summary(model)$residuals
  }
  betas <- as.data.frame(betas) %>% `colnames<-`(c("intercept",predictors))
  return(list(beta = betas, sigma = sigmas, count = n, residual = resid))
}



evolve <- function(G,p,const = NULL,m = NULL,C = NULL){
  if(!is.null(m)){
    if(!is.null(const)){
      a <- const
    }
    else{
      a <- 0
    }
    if(is.null(dim(m))){
      a <- a + G[,,1]%*%m
    }
    else {
      for(i in 1:p){
        a <- a + G[,,i]%*%m[,i]
      }
    }
    return(a)
  }
  if(!is.null(C)){
    R <- 0
    if(is.na(dim(C)[3])){
      R <- R + G[,,1]%*%C%*%t(G[,,1])
    }
    else{
      for(i in 1:p){
        R <- R + G[,,i]%*%C[,,i]%*%t(G[,,i])
      }
    }
    return(R)
  }
}

lmr <- function(data,predictors){
  ti <- sort(unique(data$date))
  y <- data$lprice
  n <- nrow(data)
  p <- length(predictors)
  X <- as.matrix(cbind(rep(1,nrow(data)),data[,predictors]))
  m <- solve(t(X)%*%X)%*%t(X)%*%y
  T <- length(ti)
  e <- list()
  mlik <- rep(0,T)
  for(t in 1:T){
    idx = data$date == ti[t]
    yt = data$lprice[idx]; ot = length(yt)
    X = as.matrix(cbind(rep(1,sum(idx)),data[idx,colnames(beta)[-1]]))    #regression matrix
    e[[t]] <- yt - X%*%m
  }
  S <- sum(unlist(e)^2)/(n-p)
  for(t in 1:T){
    mlik[t] <- dchisq(sum((e[[t]]^2)/S),length(e[[t]]))
  }
  return(list(m = m,S = S, e = e, mlik = mlik))
}

lmvr <- function(data,predictors,del,m0,C0,n0,S0){
  #dimension of process
  ti <- sort(unique(data$date))
  beta <- Tbeta(data,predictors)$beta
  T <- dim(beta)[1]
  q <- dim(beta)[2]
  
  #return object allocation
  m <- a <- matrix(rep(0,q*T),nrow = q); C <- R <- array(rep(0,q*q*T),dim = c(q,q,T)) 
  f <- Q <- e <- list()
  S <- n <- rep(0,T)
  mlik <- rep(0,T)
  
  #initial values
  b <- del[1]; d <- del[2]
  St <- S0; nt <- n0
  mt <- m0
  Ct <- C0
  
  #forwarding filtering
  for(t in 1:T){
    idx = data$date == ti[t]
    yt = data$lprice[idx]; ot = length(yt)
    X = as.matrix(cbind(rep(1,sum(idx)),data[idx,colnames(beta)[-1]]))    #regression matrix
    
    #evolve
    nt <- b*nt
    a[,t] <- at <- mt
    R[,,t] <- Rt <- Ct/d
    
    #forecast
    f[[t]] <- ft <- X%*%at
    Q[[t]] <- Qt <- X%*%Rt%*%t(X) + diag(rep(St,ot),nrow = ot)
    e[[t]] <- yt - ft
    #update
    Tt <- solve(t(X)%*%X + c(St)*solve(Rt))
    m[,t] <- mt <- Tt%*%(t(X)%*%yt + c(St)*solve(Rt)%*%at)
    S[t] <- St <- (nt*c(St) + t(yt)%*%yt + c(St)*t(at)%*%solve(Rt)%*%at - t(mt)%*%solve(Tt)%*%mt)/(nt +ot)
    n[t] <- nt <- nt + ot
    C[,,t] <- Ct <- Tt/c(St)
    mlik[t] <- dchisq(sum((e[[t]]^2)/S[t]),length(e[[t]]))
  }
  #backward smoothing
  mf <- m; Cf <- C; Sf <- S; nf <- n;
  #Gb <- rbind(Gs,diag(rep(1,dim(Gs)[2])))[1:dim(Gs)[2],]
  #mb <- rbind(mb[,T+p])
  Ct <- C[,,T]; St <- S[T]; nt <- n[T]
  for(t in (T-1):1){
    m[,t] <- (1-d)*m[,t] + d*m[,(t+1)]
    n[t] <- (1-b)*n[t]+ b*n[t+1]
    St <- S[t]; S[t] <- 1/((1-b)/St + b/S[t+1])
    C[,,t] <- S[t]*((1-d)*C[,,t]/St + d*d*C[,,(t+1)]/S[(t+1)])
  }
  return(list(m = m, C = C, n = n, S = S, mf = mf, Cf = Cf, Sf = S, nf = nf,e = e, mlik = mlik))
}


lmvar <- function(data,predictors,p,del,m0,C0,n0,S0){
  #dimension of process
  ti <- sort(unique(data$date))
  beta <- Tbeta(data,predictors)$beta
  T <- dim(beta)[1]
  q <- dim(beta)[2]
  
  #evolution matrix based on beta
  F <- cbind(diag(rep(1,q)),matrix(rep(0,q*q*(p-1)),nrow = q))
  Gs <- VAR(beta,p,"c")$b
  const <- Gs[,1]; Gs <- Gs[,-1]
  G <- rbind(Gs,diag(rep(1,dim(Gs)[2])))[1:dim(Gs)[2],]
  moduli <- Mod(eigen(G)$values)
  
  #return object allocation
  m <- a <- matrix(rep(0,p*q*(T)),nrow = p*q); C <- R <- array(rep(0,p*q*p*q*T),dim = c(p*q,p*q,T)) 
  f <- Q <- e <- list()
  S <- n <- rep(0,T)
  mlik <- rep(0,T)
  
  #initial values
  b <- del[1]; d <- (moduli[which.min(moduli)]^2)*0.95
  St <- S0; nt <- n0
  mt <- rep(m0,p)
  Ct <- diag(rep(diag(C0),p),nrow = p*nrow(C0))
  
  #forwarding filtering
  for(t in 1:T){
    idx = data$date == ti[t]
    yt = data$lprice[idx]; ot = length(yt)
    X = as.matrix(cbind(rep(1,sum(idx)),data[idx,colnames(beta)[-1]])) %*% F    #regression matrix
    
    #evolve
    nt <- b*nt
    a[,t] <- at <- G%*%mt + rep(const,p)#evolve(G=G,p=p,m = m[,(t+p-1):t])
    R[,,t] <- Rt <- G%*%Ct%*%t(G)/d#evolve(G=G,p=p,C = C[,,(t+p-1):t])/d
    
    #forecast
    f[[t]] <- ft <- X%*%at
    Q[[t]] <- Qt <- X%*%Rt%*%t(X) + diag(rep(St,ot),nrow = ot)
    e[[t]] <- yt - ft
    
    #update
    Tt <- solve(t(X)%*%X + c(St)*solve(Rt))
    m[,t] <- mt <- Tt%*%(t(X)%*%yt + c(St)*solve(Rt)%*%at)
    S[t] <- St <- (nt*c(St) + t(yt)%*%yt + c(St)*t(at)%*%solve(Rt)%*%at - t(mt)%*%solve(Tt)%*%mt)/(nt+ot)
    n[t] <- nt <- nt + ot
    C[,,t] <- Ct <- Tt/c(St) 
    mlik[t] <- dchisq(sum((e[[t]]^2)/S[t]),length(e[[t]]))
  }
  #backward smoothing
  mf <- m; Cf <- C; Sf <- S; nf <- n;
  Ct <- C[,,T];
  B <- d*solve(G)
  for(t in (T-1):1){
    m[,t] <- (1-d)*m[,t] + B%*%(m[,(t+1)] - rep(const,p))
    n[t] <- (1-b)*n[t] + b*n[t+1]
    St <- S[t]; S[t] <- 1/((1-b)/St + b/S[t+1])
    C[,,t] <- S[t]*((1-d)*C[,,t]/St + B%*%C[,,(t+1)]%*%t(B)/S[t+1])
  }
  return(list(m = m, C = C, n = n, S = S, mf = mf, Cf = Cf, Sf = S, nf = nf, G = G, const = rep(const,p),F = F, e = e,
              mlik = mlik))
}




forecast <- function(data,predictors,p,K,G,const = NULL,del,mT,CT,nT,ST,nmc){
  # organise data, initialize and define arrays for outputs ...
  ti <- sort(unique(data$date))
  T <- length(ti)
  q <- length(predictors)+1
  if(is.null(const)) const = 0
  F <- cbind(diag(rep(1,q)),matrix(rep(0,q*q*(p-1)),nrow = q))
  
  #if(identical(G,diag(rep(1,q),nrow = q))){
    b <- del[1]; d <- del[2];
  #}else{
  #  moduli <- Mod(eigen(G)$values)
  #  b <- del[1]; d <- (moduli[which.min(moduli)]^2)*0.95  
  #}
  
  a <- array(rep(0,p*q*K*nmc),dim = c(p*q,K,nmc))
  beta <- array(rep(0,q*K*nmc), dim = c(q,K,nmc))
  R <- array(rep(0,dim(G)[1]*dim(G)[2]*K*nmc),dim = c(dim(G),K,nmc))
  v <- matrix(rep(0,K*nmc), ncol = nmc)
  
  gT <- b*nT/2; hT <- (1-b)*nT/2; #1-step beta shock parameters
  W <- CT*(1/d-1)                 #1-step evolution matrix
  L <- t(chol(W))  
  
  uT <- nT/2; zT <- nT*ST/2; aT <- const + G%*% mT; RT <- G%*%CT%*%t(G)+W; LT <- t(chol(RT)); imc <- 0
  vt <- 1/rgamma(nmc,uT,zT) #draw inverse gamma for volatility
  betat <- matrix(rep(aT,nmc),ncol = nmc) +
    matrix(rep(sqrt(vt/ST),p*q),nrow = p*q, byrow = T) * LT %*% matrix(rnorm(p*q*nmc), ncol = nmc)
  
  for(t in 1:K){
    if(b<1){
      vt <- b*vt/rbeta(nmc,gT,hT)             #sample next volatility
    }
    
    betat <- const + G%*%betat + 
      matrix(rep(sqrt(vt/ST),p*q),nrow = p*q, byrow = T) * LT %*% matrix(rnorm(p*q*nmc), ncol = nmc)
    beta[,t,] <- F%*%betat; v[t,] <- vt
  }
  return(list(beta = beta,v = v))
}



aic <- function(e,p){
  e <- unlist(e)
  return(sum(e^2)+2*p)
}



MSE <- function(test, predictors, beta){
  ti <- sort(unique(test$date))
  X <- cbind.data.frame(intercept = rep(1,nrow(test)),test[,predictors])
  X <- as.matrix(X)
  pred <- err <- list()
  for(t in 1:3){
    idx <- test$date == ti[t]
    Xt <- X[idx,]
    pred[[t]] <- Xt%*%beta[,t]
    err[[t]] <- test$lprice[idx] - pred[[t]]
  }
  pred <- map_df(pred, ~data.frame(a=.x))
  err <- map_df(err, ~data.frame(a = .x))
  return(list(pred = pred, err = err))
}



interval <- function(test, predictors, beta, S){
  ti <- sort(unique(test$date))
  test <- test %>% arrange(date, lprice)
  X <- cbind.data.frame(intercept = rep(1,nrow(test)),test[,predictors])
  X <- as.matrix(X)
  y <- list()
  for(t in 1:length(ti)){
    idx <- test$date == ti[t]
    Xt <- X[idx,]
    mu <- Xt%*%beta[,t,]
    v <- matrix(rnorm(nrow(Xt)*ncol(S)),ncol = ncol(S))*matrix(rep(S[t,]^{1/2},nrow(Xt)),ncol = ncol(S),byrow = T)
    y[[t]] <- mu + v
  }
  y <- map_df(y, ~data.frame(a=.x))
  interval <- t(apply(y,1,function(x){quantile(x, c(0.025, 0.975))}))
  return(interval)
}



coverage <- function(y, bound){
  return(mean(y >= bound[,1] & y <= bound[,2]))
}


compare <- function(data,predictors){
  p <- length(predictors)
  candidate <- list()
  performance <- list()
  num <- 0
  for(i in 1:p){
    candidate[[i]] <- combn(predictors,i)
  }
  for(i in 1:p){
    for(j in 1:ncol(candidate[[i]])){
      num <- num+1
      predictor <- candidate[[i]][,j]
      beta <- Tbeta(data,predictors = predictor)$beta
      del <- c(0.99,0.99)
      m0 <- apply(beta,2,mean)#m0 <- c(1,rep(0,ncol(beta)-1));
      C0 <- diag(diag(cov(beta)),nrow = ncol(beta))
      n0 <- 100; S0 <- 0.3^2
      T <- dim(beta)[1]
      q <- dim(beta)[2]
      nmc = 5000
      form <- as.formula(paste("lprice",paste0(predictors,collapse = "+"),sep = "~"))
      K = length(unique(test$date))
      model <- lmvr(data = data, predictors = predictor, del = del, m0 = m0, C0 = C0, n0 = n0, S0 = S0)
      fore <- forecast(data = data, predictors = predictor, p=1, K = K, G = diag(rep(1,q),nrow = q), del = del,
                       mT = model$m[,T], CT = model$C[,,T], nT = model$n[T], ST = model$S[T], nmc = nmc)
      aics <- aic(model$e,q)
      beta3 <- apply(fore$beta,c(1,2),mean)
      err <- MSE(test,predictor,beta3)$err
      pMSE <- apply(err, 2, function(x){mean(x^2)})
      performance[[num]] <- list("aic" = aics,"mse" = pMSE,"pred" = paste(predictor,collapse = "+"),"mlik" = model$mlik)
      print(num)
    }
  }
  return(performance)
}

discount <- function(data,predictors){
  p <- length(predictors)
  beta <- Tbeta(data,predictors = predictors)$beta
  m0 <- apply(beta,2,mean)#m0 <- c(1,rep(0,ncol(beta)-1));
  C0 <- diag(diag(cov(beta)),nrow = ncol(beta))
  n0 <- 100; S0 <- 0.3^2
  T <- dim(beta)[1]
  q <- dim(beta)[2]
  nmc = 5000
  form <- as.formula(paste("lprice",paste0(predictors,collapse = "+"),sep = "~"))
  K = length(unique(test$date))
  result <- matrix(rep(NA,20*20),nrow = 20)
  discount <- seq(0.8,0.99,0.01)
  for(i in 1:20){
    for(j in 1:20){
      del <- c(discount[i],discount[j])
      model <- lmvr(data = data, predictors = predictors, del = del, m0 = m0, C0 = C0, n0 = n0, S0 = S0)
      result[i,j] <- aic(model$e,q)
      print(i,j)
    }
  }
  return(result)
}

grid_arrange_shared_legend <-
  function(...,
           ncol = length(list(...)),
           nrow = 1,
           position = c("bottom", "right")) {
    
    plots <- list(...)
    position <- match.arg(position)
    g <-
      ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x)
      x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x)
      x + theme(legend.position = "none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)
    
    combined <- switch(
      position,
      "bottom" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight)
      ),
      "right" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 2,
        widths = unit.c(unit(1, "npc") - lwidth, lwidth)
      )
    )
    
    grid.newpage()
    grid.draw(combined)
    
    # return gtable invisibly
    invisible(combined)
    
  }
