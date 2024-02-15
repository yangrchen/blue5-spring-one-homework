
library(quantmod)
install.packages('triangle')
library(triangle)
library(ks)

drill <- read.csv("//Users/samdotson/Documents/IAA/Spring 1/SimulationHW1datacleaned.csv")

Density.drill <- density(drill$Average.Return)
Density.drill
sd(drill$Average.Return)
mean(drill$Average.Return)
tail(drill$Average.Costs)

set.seed(1234)
Est.drill <- rkde(fhat=kde(drill$Average.Return, h=Density.drill$bw), n=1000)
hist(Est.drill, breaks=50, main='KDE of Historical Drilling', xlab='Drilling Annual Returns')
mean(Est.drill)
sd(Est.drill)

r <- Est.drill
P0 <- 1000
P1 <- P0*(1+r)

hist(P1, breaks=50, main='One Year Value Distribution', xlab='Final Value')
abline(v = 1000, col="red", lwd=2)
mtext("Initial Inv.", at=1000, col="red")

P2024 <- rep(0,10000)

for(i in 1:10000){
  P0 <- 2279.80
  r <- rnorm(n=1, mean= 0.1239563, sd=0.1242374)
  
  Pt <- P0*(1 + r)
  
  for(j in 1:5){
    r <- rnorm(n=1, mean= 0.1239563, sd=0.1242374)
    Pt <- Pt*(1+r)
  }
  
  for(j in 1:3){
   
    r <- rnorm(1, mean = 0.0917, sd = 0.1242374)
   
    lower_bound <- 0.07
    upper_bound <- 0.22
    r <- pmax(pmin(r, upper_bound), lower_bound)
    
    Pt <- Pt * (1 + r)
  }
  
  for(j in 1:9){
    r <- rnorm(1, mean = 0.05, sd = 0.1242374)
    
    lower_bound <- 0.02
    upper_bound <- 0.06
    r <- pmax(pmin(r, upper_bound), lower_bound)
    
    Pt <- Pt * (1 + r)
  }
  P2024[i] <- Pt
}

hist(P2024, breaks=50, main='2024 projected cost', xlab='Final Value')
abline(v = mean(P2024), col="red", lwd=2)
mtext("Mean Value", at=mean(P2024), col="red")

summary(P2024)





