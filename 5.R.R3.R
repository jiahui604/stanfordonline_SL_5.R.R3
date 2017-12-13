#5.R.R3 Now, use the (standard) bootstrap to estimate s.e.(β^1). To within 10%, what do you get?

setwd("~/Documents/STATS/R/Standford_online")
load("5.R.RData")
summary(Xy)
plot(Xy$X1)
plot(Xy$X2)
attach(Xy)
plot(X1,y)
abline(lm(y ~ X1))
summary(lm(y~.,data=Xy))
matplot(Xy,type="l")
legend

#bootstrap - Standard Error 
se = function(data,depend,z1,z2){
  lr = lm(depend ~ z1+z2,data=data)
  return(coef(lr))
}

se(Xy,y,X1,X2)

se.fn = function(data2,index){
  with(data2[index,],se(Xy,y,X1,X2))
}

se.fn(Xy,1:1000)

set.seed(17)
se.fn(Xy,sample(1:1000,1000,replace = TRUE))

require(boot)
boot.out= boot(Xy,se.fn, R=1000)
boot.out

#5.R.R4 Block bootstrapping
#Finally, use the block bootstrap to estimate s.e.(β^1). 
#Use blocks of 100 contiguous observations, and resample ten whole blocks with replacement 
#then paste them together to construct each bootstrap time series.
tsboot(Xy, se.fn, R = 1000, sim = "fixed", l = 100)

#######bootstrap - standard error 2, another way to calculate s.e. for bootstrapping
boot.fn <- function(data, index) {
  lr <- lm(y~.,data=Xy,subset = index)
  return (coef(lr))
}

boot(Xy,boot.fn,R=1000)

