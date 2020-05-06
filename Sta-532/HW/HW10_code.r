## R script for STA532 hw10, Spring 2020
## Author: Sheng Jiang 
## If you spot any error or have any suggestions/questions,
## please email me at sj156@duke.edu 


######## Question 2 ########

chi.sq.test <- function(y,alpha){
    ## D=1, reject the null  
    statistic <- sum(y^2)
    m <- length(y)
    D <- as.numeric(statistic > qchisq(1-alpha,m))
    return(D)
}

Bonferroni.test <- function(y,alpha){
    ## D=1, reject the null  
    p <- 2*(1-pnorm(abs(y)))
    m <- length(y)
    D <- as.numeric(min(p)<alpha/m)
    return(D)
}

Fisher.test <- function(y,alpha){
    ## D=1, reject the null  
    p <- 2*(1-pnorm(abs(y)))
    m <- length(y)
    statistic <- -2*sum(log(p))
    D <- as.numeric(statistic>qchisq(1-alpha,2*m))
    return(D)
}

numerical.experiment <- function(y,alpha,list.k){
    Nmc <- dim(y)[2]
    ell <- dim(y)[3]
    D.chisq <- D.Bonferroni <- D.Fisher <- matrix(-99,Nmc,ell)


    for(i in 1:ell){
        dta <- y[,,i]
        D.chisq[,i] <- apply(dta,2,function(x) chi.sq.test(x,alpha))
        D.Bonferroni[,i] <- apply(dta,2,function(x) Bonferroni.test(x,alpha))
        D.Fisher[,i] <- apply(dta,2,function(x) Fisher.test(x,alpha))
    }

    res <- matrix(0,3,ell) 
    res[1,] <- apply(D.chisq,2,mean)
    res[2,] <- apply(D.Bonferroni,2,mean)
    res[3,] <- apply(D.Fisher,2,mean)
    colnames(res) <- list.k
    rownames(res) <- c("Chi.sq","Bonferroni","Fisher")
    return(res)
}


###### (a) ######

## generate synthetic data 
Nmc <- 1e5
m <- 100
list.k.a <- c(1,4,16,64)
theta <- ya <- array(0,dim=c(m,Nmc,length(list.k.a))) 
for(i in 1:length(list.k.a)){
    k <- list.k.a[i]
    theta[,,i] <- matrix(rnorm(m*Nmc,0,sqrt(k/100)),ncol=Nmc)
    ya[,,i] <- matrix(mapply(function(x) rnorm(1,x,1), x=theta[,,i]),ncol=Nmc)
}

## compare tests 
alpha <- 0.05
numerical.experiment(ya,alpha,list.k.a)



###### (b) ######

## generate synthetic data 
m <- 100
Nmc <- 1e5
list.k.b <- c(1,3,5,7)

theta <- matrix(0,m,length(list.k.b))
theta[1,] <- list.k.b 
yb <- array(0,dim=c(m,Nmc,length(list.k.b)))
for(i in 1:length(list.k.b)){
    mu <- theta[,i]
    yb[,,i] <- t(mapply(function(x) rnorm(Nmc,x,1),x=mu)) 
}

## compare tests 
alpha <- 0.05
numerical.experiment(yb,alpha,list.k.b)


######## Question 4 ########

fdr <- function(x,gamma,b){
    res <- (1-gamma)*x/((1-gamma)*x+gamma *(1-(1-x)^b))
    return(res)
} 

BH.mod <- function(p,H,alpha,b,gamma){
    ### solve the threshold by grid search 
    Ngrid <- 1e4
    s <- 1:Ngrid/Ngrid
    fdr.s <- fdr(s,gamma,b)
    min.fs <- min(fdr.s)
    
    if(min.fs <= alpha){
        id <- max(which(fdr.s <= alpha))  
        Dis <- as.numeric(p< s[id])
        if(sum(Dis)>0){
            fdp <- sum(Dis*(1-H))/sum(Dis)
        }else{
            fdp <- 0
        }
        return(c(fdp,sum(Dis))) 
    }else{
        return("error")
    }
}

fn <- function(x,y){
    ## x=gamma, y=b 
    res <- c(0,0)
    res[1] <- (1-x)/2 + x/(y+1)
    res[2] <- (1-x)/12 + x*y/(y+1)^2/(y+2) + x*(1-x)*(1/4-y/(y+1)^2)
    return(res)
}


BH <- function(p,H,alpha){
    m <- length(p)
    
    pbar <- mean(p)
    pvar <- var(p)*(m-1)/m
    
    ## method of moment 
    eq <- optim(c(0.5,1),function(x) crossprod(fn(x[1],x[2])-c(pbar,pvar)))
    gamma <- eq$par[1]
    b <- eq$par[2]

    ## plug-in + grid search 
    Ngrid <- 1e4
    s <- 1:Ngrid/Ngrid
    fdr.s <- fdr(s,gamma,b)
    min.fs <- min(fdr.s)     
    if(min.fs <= alpha){
        id <- max(which(fdr.s <= alpha))  
        Dis <- as.numeric(p< s[id])
        if(sum(Dis)>0){
            fdp <- sum(Dis*(1-H))/sum(Dis)
        }else{
            fdp <- 0
        }
        return(c(fdp,sum(Dis))) 
    }else{
        return("error")
    }
}


###### numerical experiment ###### 
list.b <- c(1,2,4,8)
list.gamma <- c(0.1,0.5,0.9)

m <- 100 
Nmc <- 1e4
H <- U <- B <- p <- array(0,dim=c(m,Nmc,length(list.gamma), length(list.b)))

## generate synthetic data 
for(i in 1:length(list.gamma)){
    for(j in 1:length(list.b)){
        ga <- list.gamma[i]
        b <- list.b[j] 
        H[,,i,j] <- matrix(rbinom(m*Nmc,1,prob=ga),ncol=Nmc)
        U[,,i,j] <- matrix(runif(m*Nmc),ncol=Nmc)
        B[,,i,j] <- matrix(rbeta(m*Nmc,1,b),ncol=Nmc)
        p[,,i,j] <- H[,,i,j]*B[,,i,j]+(1-H[,,i,j])*U[,,i,j]
    }
}

## store results 
FDR.mod <- TD.mod <- matrix(0,length(list.gamma),length(list.b))
FDR <- TD <- Nerr <- matrix(0,length(list.gamma),length(list.b))
rownames(FDR.mod) <- rownames(TD.mod) <- rownames(FDR) <- rownames(TD) <- rownames(Nerr) <- list.gamma
colnames(FDR.mod) <- colnames(TD.mod) <- colnames(FDR) <- colnames(TD) <- colnames(Nerr) <- list.b

## numerical experiment

alpha <- 0.1 
for(i in 1:length(list.gamma)){
    for(j in 1:length(list.b)){
        ## trial (i,j)
        dta <- p[,,i,j]
        HH <- H[,,i,j]
        ga <- list.gamma[i]
        bb <- list.b[j]

        ## modified BH with (gamma,b) known 
        res.mod <- sapply(1:Nmc,function(x) BH.mod(dta[,x],HH[,x],alpha,bb,ga))
        if(!is.null(dim(res.mod))){
            FDR.mod[i,j] <- mean(res.mod[1,])
            TD.mod[i,j] <- mean(res.mod[2,]) 
        }else{
            FDR.mod[i,j] <- "NA"
            TD.mod[i,j] <- "NA" 
        }

        ## modified BH with (gamma,b) unknown 
        res.bh <- sapply(1:Nmc,function(x) BH(dta[,x],HH[,x],alpha))
        id.error <- which(res.bh=="error") 
        Nerr[i,j] <- round(length(id.error)/Nmc ,3) 
        if(Nerr[i,j]< 1 & Nerr[i,j]>0){
            ## calculate based on "controllable trials" 
            res.bh.sub <- do.call(cbind, res.bh[-id.error]) 
            FDR[i,j] <- round(mean(res.bh.sub[1,]),3)
            TD[i,j] <- round(mean(res.bh.sub[2,])  ,3)
        }else if(Nerr[i,j]==1){
            FDR[i,j] <- "NA"
            TD[i,j] <- "NA" 
        }else{
            FDR[i,j] <- round(mean(res.bh[1,]),3)
            TD[i,j] <- round(mean(res.bh[2,]),3)
        }
    }
}


## proportion of valid trials 
(1-Nerr) *100

## average total discovery based on vaild trials 
TD
TD.mod

## average FDR based on valid trials
FDR
FDR.mod ## FDR is more accurate. For some cases, it is impossible to control FDR at 0.1.  
