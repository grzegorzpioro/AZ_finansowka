library(ggplot2)
library(dplyr)
library(ppcor)
dane <- read.table("realest.txt", header  =TRUE)

model <- lm(Price~.,data = dane)

X <- as.matrix(cbind(1,dane[,-1]))

model.matrix(model)

Y <- as.matrix(dane[1])

price <- dane$Price
# %*% mnozenie macierzy t()-transpozycja solve()-odwrotnosc macierzy 
beta <- solve(t(X) %*% X) %*%t(X) %*% Y
solve(t(X) %*% X,t(X) %*% Y)
coef(model)

beta

summary(model)$coef[,1]


SST <- sum((price-mean(price))^2) #total sum of squares
SSR <- sum((model$fitted.values-mean(price))^2)
SSE <- sum((model$fitted.values-price)^2)

sum(model$resid^2)
sum(residuals(model)^2)

SSR/SST
1-SSE/SST
summary(model)$r.squared

#c)
model$coef

m2 <- lm(Price~Bedroom,data = dane)
m2$coef
predict(model, newdata = data.frame(Bedroom =3,Space = 1500,Room=8,Lot=40,Tax=1000,Bathroom=2,Garage=1,Condition=0))
#d
c(1,3,1500,8,40,1000,2,1,0) %*% model$coef
#e
#estymator wariancji bledow
n<- nrow(X)
p<-ncol(X)
estwar <- SSE/(n-p)
estwar

summary(model)

#3.2
x1 <-rnorm(100)
x2 <-rnorm(100)
x3 <-rnorm(100)
eps <- rnorm(100,0,10)
b0 <- 2
b1 <- 0.5
b2 <- 1
b3 <- 0.7

y <- b0 + b1*x1 +b2*x2 + b3*x3 + eps

dane <- data.frame(y = y, a = x1, b= x2,c=x3)

m1 <- lm(y~.,data=dane)

sum(m1$residuals)
k <- 1000
sigmas2 <- numeric(k)

for(i in 1:k){
  x1 <-rnorm(100)
  x2 <-rnorm(100)
  x3 <-rnorm(100)
  eps <- rnorm(100,0,10)
  b0 <- 2
  b1 <- 0.5
  b2 <- 1
  b3 <- 0.7
  
  y <- b0 + b1*x1 +b2*x2 + b3*x3 + eps
  
  dane <- data.frame(y = y, a = x1, b= x2,c=x3)
  
  m1 <- lm(y~.,data=dane)
  
  sigmas2[i] <- summary(m1)$sigma^2
}
sigmas2
mean(sigmas2)
