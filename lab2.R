library(ggplot2)
library(dplyr)
library(ppcor)
#2.1
dane <- read.table("airpollution.txt", header  =TRUE)

mor <- dane$Mortality
edu <- dane$Education
cor1 <- cor(mor,edu) 
cor1  
#b
k<-100000
corrs <- numeric(k)
for(i in 1:k)
{
  morper <- sample(mor)
  corrs[i]<-cor(morper, edu)
}
hist(corrs)
abline(v = cor1, col = 'red')
#d
(1+sum(abs(corrs)>abs(cor1)))/(1+length(corrs))

#2.2
-10/sqrt(5*26)

install.packages('ppcor')
library(ppcor)

z <- rnorm(10000)  
nx <- rnorm(10000)  
ny <- rnorm(10000)  
x <- 2*z+nx
y <- -5*z + ny
cor(x,y)
ramka <- data.frame(z,x,y)
pcor(ramka)

cor(nx,ny)
v<-z^2
nv <- rnorm(10000)
m1 <- lm(x~z-1)
m1$coef

m2 <- lm(y~z-1)
m2$coef

cor(x-m1$coef*z,y-m2$coef*z)

nv <- rnorm(10000)
v<-z^2+nv

m3 <- lm(v~z-1)
m3$coef


cor(v-m3$coef*z,y-m2$coef*z)

#2.3

x <- seq(0,10,0.1)
n<-length(x)
eps <- rnorm(n,0,3)
y <- x + eps

ramka <- data.frame(x,y)
ggplot(data = ramka,aes(x = x, y= y))+
  geom_point()

cov(x,y)/sqrt(var(x)*var(y))
cor(x,y)


model <- lm(y~x)
ggplot(data = ramka,aes(x = x, y= y))+
  geom_point()+
  geom_line(y=model$coef[1]+model$coef[2]*x,color = 'red')

plot(x,y)
abline(model$coef[1],model$coef[2],col = 'red')
abline(model,col='red')

x <- seq(0,10,0.1)
n<-length(x)
eps <- rnorm(n,0,0.5)
y <- x + eps

ramka <- data.frame(x,y)

model <- lm(y~x)
ggplot(data = ramka,aes(x = x, y= y))+
  geom_point()+
  geom_line(y=model$coef[1]+model$coef[2]*x,color = 'red')

plot(x,y)
abline(model,col='red')


x <- seq(0,10,0.1)
n<-length(x)
eps <- rnorm(n,0,5)
y <- x + eps

ramka <- data.frame(x,y)

model <- lm(y~x)
ggplot(data = ramka,aes(x = x, y= y))+
  geom_point()+
  geom_line(y=model$coef[1]+model$coef[2]*x,color = 'red')

plot(x,y)
abline(model,col='red')

b1 <- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2)
b1
b0 <- mean(y)-b1*mean(x)
b0

library(MASS)
hills
dist <- hills$dist
climb <- hills$climb
time <- hills$time
par(mfrow = c(2,1))
plot(time,dist)
plot(time,climb)
cor(time,dist)
cor(time,climb)

m1 <- lm(dist~time)
m2 <- lm(climb~time)

plot(time,dist)
abline(m1,col='red')

plot(time,climb)
abline(m2,col='red')

SST <- sum((time-mean(time))^2)
SSRm1 <- sum((m1$fitted.values-mean(time))^2)
SSRm2 <- sum((m2$fitted.values-mean(time))^2)
SSEm1 <- sum((m1$fitted.values-time)^2)
SSEm2 <- sum((m2$fitted.values-time)^2)

SSEm1
sum(m1$residuals^2)

SSRm1/SST
SSRm2/SST

summary(m1)
summary(m1)$r.squared
cor(time,dist)^2
#c
sum(m1$coef*c(1,15))
predict(m1,data.frame(dist = 15))


dane <- read.table("anscombe_quartet.txt", header  =TRUE)

m1 <- lm()






