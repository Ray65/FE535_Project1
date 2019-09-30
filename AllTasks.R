library(quantmod)
library(lubridate)
library(PerformanceAnalytics)

rm(list  =ls())
# Getting Data
tics <- c("AAPL", "AMZN", "JNJ", "BAC", "ORCL", "XOM", "GOOG", "WMT", "PFE", "JPM", "T", "CVX")
SPY <- get(getSymbols("SPY", src= "yahoo", from= as.Date("2004-08-01"), to= as.Date("2019-08-31")))
p_list <- list()
for(s in tics){
  cat(s,"\n")
  p_s <- get(getSymbols(s, src="yahoo", from= as.Date("2004-08-01"), to= as.Date("2019-08-31")))
  p_list <- c(p_list, list(p_s[,6]))
}
length(p_list)

# Getting the adj.column
# f_adj <- function(p_s){
#  return (p_s[,6])
# }
# 
# p_adj <- merge(p_list_adj[[1]], p_list_adj[[2]])
# p_adj <- merge(p_adj, p_list_adj[[3]])
# p_adj <- merge(p_adj, p_list_adj[[4]])
# p_adj <- merge(p_adj, p_list_adj[[5]])
# p_adj <- merge(p_adj, p_list_adj[[6]])
# p_adj <- merge(p_adj, p_list_adj[[7]])
# p_adj <- merge(p_adj, p_list_adj[[8]])
# p_adj <- merge(p_adj, p_list_adj[[9]])
# p_adj <- merge(p_adj, p_list_adj[[10]])
# p_adj <- merge(p_adj, p_list_adj[[11]])
# p_adj <- merge(p_adj, p_list_adj[[12]])

p <- Reduce(merge,p_list)
head(p)
tail(p)
summary(p)

# Calculate Return
R <- log(p/lag(p))
R <- na.omit(R)
head(R)

# average of adj.column
m <- mean(R)
m <- 252*apply(R,2,mean)
m

#volatility
vol <- sqrt(252)*apply(R,2,sd)
vol
SR <- m/vol
SR

plot(m-vol)

#min, mean and max of mean return, vol and SR
summary(m)
summary(vol)
summary(SR)

# merge SPY
p_list2 <- c(p_list, list(SPY[,6]))
p2 <- Reduce(merge,p_list2)

class(p2)
x <- p2$SPY.Adjusted
RS <- na.omit(log(x/lag(x)))

R2 <- merge(R,RS,all=F)

# Getting the measures
msr <- table.CAPM(R2[,1:12],R2[,13])
msr
alphas <- msr[1,]
betas <- msr[2,]
tr <- msr[12,]
omega <- msr[9,]
ir <- msr[11,]

#min
min(alphas)
min(betas)
min(tr)
min(omega)
min(ir)

#max
max(alphas)
max(betas)
max(tr)
max(omega)
max(ir)

#mean
apply(alphas, 1, mean)
apply(betas, 1, mean)
apply(tr, 1, mean)
apply(omega, 1, mean)
apply(ir, 1, mean)

#plot
plot(m,betas)

#3.1
f_sim <- function(n) {
  i <- 0
  differenceList <- list()
  while(i<1000){
    x <- runif(6, min = 1, max = 6)
    difference <- max(x) - min(x)
    differenceList <- c(differenceList, difference)
    i <- i+1
  }
  j <- 0
  for (n in differenceList)
  {
    if (n < 3)
    {
      j <- j + 1
    }
  }
  p_1000 <- (j/1000)
  
  return (p_1000)
}
#sim_list <- lapply(1:1000, f_sim)
#head(sim_list) 
p <- f_sim(1)  #'p' represents the probability of difference between min & max of a game being less than 3 measured for 1000 games


#3.1 version 2 
#MC method
f_sim2 <- function(n){
  difList <- list()
  i <- 1
  j <- 0
  while(i < 1000)
  {
    x <- sample(1:6, 6, replace = TRUE)
    dif = max(x) - min(x)
    difList <- c(difList, dif)
    i <- i + 1
  }
  for(n in difList)
  {
    if (n < 3){
      j <- j + 1
    }
  }
  return(j/1000)
}

p2 <- mean(sapply(1:100, f_sim2))   #Answer for MC method

#True answer



#3.2
f_toss <- function(n)
{
  x_seq <- sample(1:2, 100, replace = TRUE)
  X <- min(which(x_seq[-1] + x_seq[-length(x_seq)] == 4) + 1)
  return (X)
}

N <- 10^3
tossList <- sapply(1:N, f_toss)
mean(tossList)  #Answer

#3.3








