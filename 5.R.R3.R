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
  #y_bar = mean(y)
  lr = lm(depend ~ z1+z2,data=data)
  #y_pred = predict(lr)
  beta = summary(lr)$coefficients[2, 2]
  #standardd = sqrt(sum((y_pred-y_bar)^2/(length(y)-1)))
  #standarde = standardd/sqrt(length(y))
  beta
}

se(Xy,y,X1,X2)

se.fn = function(data2,index){
  with(data2[index,],se(Xy,y,X1,X2))
}

se.fn(Xy,1:1000)

set.seed(17)
alpha.fn(Xy,sample(1:1000,1000,replace = TRUE))

boot.out= boot(Xy,se.fn, R=1000)
boot.out