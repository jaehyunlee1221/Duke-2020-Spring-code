#class note as R

x=AR1simulate(0.9,1,100) 
x=AR1simulate(-0.9,1,100)

y <- read.table("Sta-642/soi.txt")

y <- readxl::read_xlsx("Sta-642/USmacrodata1960-2014.xlsx")
infln <- x <- c(0,diff(y$Infln))
p=4;
m0=rep(0,p); n0=1; s0=0.05; C0=diag(p); 
del=c(0.98, .98)
model <- tvar(x, p ,del, m0, C0,s0,n0)
decomp_result <- tvar_decomp(x, model$m)
decomp_plot(x, decomp_result$decomp, 4)
a <- decomp_result$decomp




mT <- model$m[,220]; CT <- model$C[,,220]; sT <- model$s[220]; nT <- model$n[220]
forecast2 <- tvarforecast(x, K = 12, I = 2500, p ,del, mT, CT, sT,nT)

a <- tvarFFBS(x, p ,del, m0, C0, s0, n0, 1000)
mean(a$thetasamp[2,220,])
