library(readxl)
library(tidyverse)
library(lubridate)
library(grid)
library(gridExtra)
library(ggthemr)
set.seed(642)

##################################
#data loading
apartment <- read_csv("gangnam.csv")
apartment <- apartment %>% 
  mutate_if(is.character,as.factor) %>% 
  dplyr::select(-city, -name, -road_name, -district) %>% 
  mutate(price = price/100, date = as_date(str_c(month,"01")), age = year(date)-built) %>% 
  mutate(lprice = log(price), lsize = log(size), lfloor = log(floor), lage = log(age+2)) %>% 
  arrange(date,lprice)
idx <- apply(apartment,1,function(x){sum(is.na(x))})#is.na(apartment$lage)
apartment <- apartment[idx==0,]
test <- read_csv("test.csv")
test <- test %>% 
  mutate_if(is.character,as.factor) %>% 
  dplyr::select(-city, -name, -road_name, -district) %>% 
  mutate(price = price/100, date = as_date(str_c(month,"01")), age = year(date)-built) %>% 
  mutate(lprice = log(price), lsize = log(area), lfloor = log(floor), lage = log(age+2)) %>% 
  arrange(date,lprice)
idx <- is.na(test$lfloor)
test <- test[!idx,]






m_price <- apartment %>% 
  group_by(date) %>% 
  summarise(price = mean(price))

myplot <- ggplot() +
  geom_line(data = m_price, mapping = aes(x = date, y = price)) +
  labs(title = "average price of apartment in Korea") +
  ggthemes::theme_tufte()

beta <- Tbeta(data,predictors)$beta
beta <- as.data.frame(beta) %>% 
  `colnames<-`(c(paste0("beta",1:4)))
plot_list <- list()
plot_list[[1]] <- ggplot(data = beta) +
  geom_line(mapping = aes(x = ti, y = beta1)) +
  labs(title = "coefficient of intercept") +
  ggthemes::theme_tufte()
plot_list[[2]] <- ggplot(data = beta) +
  geom_line(mapping = aes(x = ti, y = beta2)) +
  labs(title = "coefficient of Size") +
  ggthemes::theme_tufte()
plot_list[[3]] <- ggplot(data = beta) +
  geom_line(mapping = aes(x = ti, y = beta3)) +
  labs(title = "coefficient of Floor") +
  ggthemes::theme_tufte()
plot_list[[4]] <- ggplot(data = beta) +
  geom_line(mapping = aes(x = ti, y = beta4)) +
  labs(title = "coefficient of Age")+
  ggthemes::theme_tufte()
grid.arrange(grobs = plot_list,nrow = 2)


data <- apartment %>% 
  dplyr::select(lprice,lsize,lfloor,lage,date)
predictors <- c("lsize","lage","lfloor")
beta <- Tbeta(data,predictors)$beta
del <- c(0.90,0.90)
m0 <- c(1,1,0,0); C0 <- diag(diag(cov(beta)),nrow = ncol(beta));
n0 <- 100; S0 <- 0.3^2
T <- dim(beta)[1]
q <- dim(beta)[2]
nmc = 5000
form <- as.formula(paste("lprice",paste0(predictors,collapse = "+"),sep = "~"))


model1 <- lm(form, data = data)
n <- nrow(data)
X <- as.matrix(cbind(rep(1,nrow(data)),data[,predictors]))
S <- summary(model1)$sigma^2
m <- model1$coefficients; C <- S*solve(t(X)%*%X)
fore1 <- list(beta = array(t(MASS::mvrnorm(n = 3*nmc,mu = m, Sigma = C)),dim = c(q,3,nmc)),
              v = matrix(1/rgamma(3*nmc, n/2, n*S/2),nrow = 3))

model2 <- lmvr(data = data, predictors = predictors, del = del, m0, C0, n0, S0)
fore2 <- forecast(data = data, predictors = predictors, p=1, K = 3, G = diag(rep(1,q),nrow = q),del = del,
                  mT = model2$m[,T], CT = model2$C[,,T], nT = model2$n[T], ST = model2$S[T], nmc = nmc)

model3 <- lmvar(data = data, predictors = predictors, p=1, del = del, m0 = m0, C0 = C0, n0 = n0, S0 = S0)
fore3 <- forecast(data = data, predictors = predictors, p=1, K = 3, G = model3$G, del = del, const = model3$const,
                  mT = model3$m[,T], CT = model3$C[,,T], nT = model3$n[T], ST = model3$S[T], nmc = nmc)

#comparison by RMSE, prediction accuracy, coverage, AIC
#model deviance
devs <- c(dev(model1$residuals), dev(model2$e), dev(model3$e))
#model prediction accuracy by RMSE
beta1 <- matrix(rep(model1$coefficients,3),ncol = 3)
beta2 <- apply(fore2$beta,c(1,2),mean)
beta3 <- apply(fore3$beta,c(1,2),mean)
#point prediction
est <- cbind(MSE(test,predictors,beta1)$pred,MSE(test,predictors,beta2)$pred,MSE(test,predictors,beta3)$pred)
err <- cbind(MSE(test,predictors,beta1)$err,MSE(test,predictors,beta2)$err,MSE(test,predictors,beta3)$err)
pMSE <- apply(err, 2, function(x){mean(x^2)})
#Coverage
y_int1 <- interval(test,predictors, beta = fore1$beta, S = fore1$v)
y_int2 <- interval(test,predictors, beta = fore2$beta, S = fore2$v)
y_int3 <- interval(test,predictors, beta = fore3$beta, S = fore3$v)
coverage1 <- coverage(test$lprice, y_int1)
coverage2 <- coverage(test$lprice, y_int2)
coverage3 <- coverage(test$lprice, y_int3)


df_plot1 <- data.frame(lprice = test$lprice, 
                       pred = MSE(test,predictors,beta1)$pred,
                       lwr = y_int1[,1],upr = y_int1[,2]) %>% arrange(pred)
df.test <- data.frame(college = linear.test.norm$college,
                      Apps = exp(linear.test.norm$Apps),
                      pred = exp(predict(glm.norm.2, linear.test.norm, type = "response")),
                      lwr = bound.test.norm[,1], upr = bound.test.norm[,2]) %>%
  arrange(pred)

ggplot(df.norm, aes(x = pred, y = Apps)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr),
              fill = "skyblue", alpha = 0.5) +
  geom_point() +
  #geom_text() +
  annotate("text", x = c(15000,25000), y = c(45000,6000), label = c("Rutgers", "Brigham Young")) +
  labs(x = "Predicted application", y = "Application",
       title = "95% Prediction Intervals under the linear model")
```