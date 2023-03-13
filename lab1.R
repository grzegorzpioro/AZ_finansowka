library(ggplot2)
library(dplyr)
dane <- read.csv("daneSoc.csv", sep = ';', header  =TRUE)
#1.1
#a
head(dane)
str(dane)
names(dane)
length(dane)
typeof(dane)
summary(dane)
class(dane)
dim(dane)
nrow(dane)
ncol(dane)
sapply(dane, class)
lapply(dane, summary)
#b
table(dane$wyksztalcenie,dane$praca)
table(dane$praca)
#c
summary(dane$cisnienie.skurczowe[dane$plec == 'mezczyzna' & dane$wyksztalcenie == 'srednie'])
#d
dane %>% 
  ggplot(aes(x = praca, y = cisnienie.skurczowe,fill = praca))+
  geom_boxplot()
plot(dane$cisnienie.rozkurczowe~dane$cisnienie.skurczowe)
plot(as.factor(dane$plec),dane$cisnienie.skurczowe)
plot(as.factor(dane$plec),as.factor(dane$wyksztalcenie))
#e
dane[dane$cisnienie.skurczowe>=140 & dane$cisnienie.skurczowe<=150 &
       dane$wyksztalcenie == 'srednie',]
#f
dane[dane$cisnienie.skurczowe== max(dane$cisnienie.skurczowe),]
#g
dane[dane$cisnienie.skurczowe>= quantile(dane$cisnienie.skurczowe,0.8),]

#1.2
#a
par(mfrow = c(2,2))
probka <- rnorm(10)
qqnorm(probka)
qqline(probka)

probka <- rnorm(50)
qqnorm(probka)
qqline(probka)

probka <- rnorm(100)
qqnorm(probka)
qqline(probka)

probka <- rnorm(500)
qqnorm(probka)
qqline(probka)

#b
probka <- rgamma(10,2,2)
qqnorm(probka)
qqline(probka)

probka <- rgamma(50,2,2)
qqnorm(probka)
qqline(probka)

probka <- rgamma(100,2,2)
qqnorm(probka)
qqline(probka)

probka <- rgamma(500,2,2)
qqnorm(probka)
qqline(probka)

probka <- rgamma(10,2,2)
qqnorm(probka)
qqline(probka)

probka <- rgamma(50,2,2)
qqnorm(probka)
qqline(probka)

probka <- rgamma(100,2,2)
qqnorm(probka)
qqline(probka)

#c
probka <- rcauchy(10)
qqnorm(probka)
qqline(probka)

probka <- rcauchy(50)
qqnorm(probka)
qqline(probka)

probka <- rcauchy(100)
qqnorm(probka)
qqline(probka)

probka <- rcauchy(500)
qqnorm(probka)
qqline(probka)

#1.3
#a
par(mfrow=c(1,1))
dane <- read.table("skorelowana_probka.txt", sep = ' ', header  =TRUE)
plot(dane$x,dane$y)
x<-dane$x
y<-dane$y
cov(dane$x,dane$y)/sqrt(var(dane$x)*var(dane$y))

ro <- sum((x-mean(x))*(y-mean(y)))/sqrt(sum((x-mean(x))**2)*sum((y-mean(y))**2))
dane
dane$x
dane$y
cor(dane$x,dane$y,method = 'pearson')

#cor.test(dane$x,dane$y,conf.level = 0.95, alternative = 'two.sided',method = 'pearson')
#C)
#stat testowa = sqrt(n)*ro z daszkiem
sqrt(dim(dane)[1])*ro
2*(1- pnorm(sqrt(dim(dane)[1])*ro,0,1))

#d)
tanh(qnorm(0.025)/sqrt(nrow(dane))+atanh(ro))
tanh(qnorm(0.975)/sqrt(nrow(dane))+atanh(ro))

#e
xr <- rank(x)
yr <- rank(y)
cor(xr,yr)
plot(xr,yr)

cor(x,y,method = 'spearman')
#f
cor(x,y,method = 'kendall')
zl <-0
n <- nrow(dane)
for(i in 1:n)
{
  for(j in i:n)
  {
    zl <- zl + sign((x[i]-x[j])*(y[i]-y[j]))
  }
}
zl/(n*(n-1)/2)

#f
x1 <- c(x,x[n])
y1 <- c(y,y[n])
rank(x1)
rank(y1)
cor(rank(x1),rank(y1))

zl <- 0 
n <- length(x1)
for(i in 1:n)
{
  for(j in i:n)
  {
    zl <- zl + sign((x[i]-x[j])*(y[i]-y[j]))
  }
}
zl/(n*(n-1)/2)




