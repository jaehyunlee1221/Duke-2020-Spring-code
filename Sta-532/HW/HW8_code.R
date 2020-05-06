## R script for STA532 hw08, Spring 2020
## Author: Sheng Jiang 
## If you spot any error or have any suggestions/questions,
## please email me at sj156@duke.edu 




######## Question 1 ########

score <- function(a,b,x,y){
  ## a is alpha, b is beta, x and y are data vectors
  ## output: score function for n observations 
  pi <- 1/(1+exp(-(a+b*x)))
  res <- c(sum(y-pi), sum(x*(y-pi))) 
  return(as.matrix(res,ncol=1)) 
}

info <- function(a,b,x,y){
  ## a is alpha, b is beta, x and y are data vectors
  ## output: observed info matrix for n observations 
  pi <- 1/(1+exp(-(a+b*x)))
  qi <- pi*(1-pi)
  res <- matrix(0,2,2)
  res[1,1] <- sum(qi)
  res[1,2] <- res[2,1] <- sum(x*qi)
  res[2,2] <- sum(x^2*qi)
  return(res)
}

## simulate synthetic data 
n <- 100
a <- -1; b <- 2

x <- as.matrix(rnorm(n,0,3),ncol=1)
p0 <- 1/(1+exp(-a-b*x))
y <- apply(p0,1,function(x) rbinom(1,1,x))

## Newton-Raphson algo 
theta <- as.matrix(c(mean(y), sum(x*y)/sum(x^2))) ## initialize from somewhere 
rownames(theta) <- c("alpha","beta")

tol <- 1 ; iter <- 0
while(tol>0.00001){
  theta.new <- theta + solve(info(theta[1],theta[2],x,y)) %*% score(theta[1],theta[2],x,y)
  tol <- sum(abs(theta.new-theta))
  iter <- iter+1
  theta <- theta.new
}
print(iter) ## number of iterations to converge 
print(theta) ## point estimate 
print(sqrt(diag(solve(info(theta[1],theta[2],x,y))))) ## se 
print(solve(info(theta[1],theta[2],x,y))) ## vcov matrix 

## check previous answers with standard package 
fit <- glm(y~x,family=binomial(link='logit'))
summary(fit)
vcov(fit)




######## Question 2 ########

compare.var <- function(theta0,n,Nmc){
  ## returns the ratio of the var of mle over the var of y bar
  ## we expect this ratio to be less than 1 
  
  ## use inverse CDF method to simulate Yi
  u <- matrix(runif(n*Nmc),ncol=n)
  y <- u^(1/theta0)
  
  ## mle for mu
  theta.hat <- apply(y,1,function(x) -1/mean(log(x)))
  mu.hat <- theta.hat/(1+theta.hat)
  
  ## y bar
  ybar <- apply(y,1,mean)
  
  return(var(mu.hat)/var(ybar))
}


theta.list <- c(1,11,111)  ## pick a sequence of theta0 
n.list <- c(10,100,1000)  ## pick a sequence of sample size 
Nmc <- 1e4 ## Large MC sample size reduces MC errors

res <- matrix(0,length(theta.list),length(n.list))
colnames(res) <- n.list
rownames(res) <- theta.list

for(i in 1:length(theta.list)){
  for(j in 1:length(n.list)){
    res[i,j] <- compare.var(theta.list[i],n.list[j],Nmc)
  }
}

print(res)

## Message: 
## larger theta and sample size makes the difference smaller