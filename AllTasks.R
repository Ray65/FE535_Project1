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
RS <- na.omit(log(x/log(x)))

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

# 2 Back-Testing
#2.1 Task 1

# IN 2017 and 2018 Date Range:
IN.Start <- as.Date("2017-01-03") # first day of data in 2017
IN.End <- as.Date("2018-12-31") # last day of data in 2018
# OUT 2019 Back-Testing Date Range: 
OUT.Start <- as.Date("2019-01-02") # first day of data in 2019
OUT.End <- as.Date("2019-08-31") # last day of data in 2019
  
#2.2
# IN sample to compute the portfolio weights

# IN 2017 and 2018 Date Range:
IN.Start <- as.Date("2017-01-03") # first day of data in 2017
IN.End <- as.Date("2018-12-31") # last day of data in 2018

# Getting IN Data
tics <- c("AAPL", "AMZN", "JNJ", "BAC", "ORCL", "XOM", "GOOG", "WMT", "PFE", "JPM", "T", "CVX")
SPY <- get(getSymbols("SPY", src= "yahoo", from = IN.Start, to = IN.End))
p_IN_list <- list()
for(s in tics){
  cat(s,"\n")
  p_IN_s <- get(getSymbols(s, src="yahoo", from = IN.Start, to = IN.End))
  p_IN_list <- c(p_IN_list, list(p_IN_s[,6]))
}
p_IN <- Reduce(merge,p_IN_list)

# Use the OUT sample to compute the return of each portfolio
# OUT 2019 Back-Testing Date Range: 
OUT.Start <- as.Date("2019-01-02") # first day of data in 2019
OUT.End <- as.Date("2019-08-31") # last day of data in 2019

# Getting OUT Data
tics <- c("AAPL", "AMZN", "JNJ", "BAC", "ORCL", "XOM", "GOOG", "WMT", "PFE", "JPM", "T", "CVX")
SPY <- get(getSymbols("SPY", src= "yahoo", from = OUT.Start, to = OUT.End))
p_OUT_list <- list()
for(s in tics){
  cat(s,"\n")
  p_OUT_s <- get(getSymbols(s, src="yahoo", from = OUT.Start, to = OUT.End))
  p_OUT_list <- c(p_OUT_list, list(p_OUT_s[,6]))
}
p_OUT <- Reduce(merge,p_OUT_list)

#2.2
# IN sample to compute the portfolio weights
NN <- 12
R_IN <- log(p_IN/log(p_IN))
R_IN <- na.omit(R_IN)
head(R_IN)
#Portfolio 1
# average of adj.column
m_IN <- mean(R_IN)
m_IN <- 252*apply(R_IN,2,mean)

#volatility
vol_IN <- sqrt(252)*apply(R_IN,2,sd)
vol_IN
SR_IN <- m_IN/vol_IN
SR_IN

SUM_INV_VOL_IN <-  sum(1/vol_IN)
#Portfolio 1
w1 <- (1/vol_IN)/(SUM_INV_VOL_IN)
# AAPL.Adjusted AMZN.Adjusted  JNJ.Adjusted  BAC.Adjusted ORCL.Adjusted  XOM.Adjusted GOOG.Adjusted 
# 0.04559818    0.02393229    0.10548685    0.06899140    0.11329813    0.18381731    0.05761265 
# WMT.Adjusted  PFE.Adjusted  JPM.Adjusted    T.Adjusted  CVX.Adjusted 
# 0.05750270    0.07106038    0.06271684    0.11298948    0.09699378 
#Portfolio 2
SUM_SR_IN <-  sum(SR_IN)
w2 <- SR_IN/SUM_SR_IN
# AAPL.Adjusted AMZN.Adjusted  JNJ.Adjusted  BAC.Adjusted ORCL.Adjusted  XOM.Adjusted GOOG.Adjusted 
# 0.04559818    0.02393229    0.10548685    0.06899140    0.11329813    0.18381731    0.05761265 
# WMT.Adjusted  PFE.Adjusted  JPM.Adjusted    T.Adjusted  CVX.Adjusted 
# 0.05750270    0.07106038    0.06271684    0.11298948    0.09699378 
#Portfolio 3
w3 <-  w2
w3[] <- 1/NN
# APL.Adjusted AMZN.Adjusted  JNJ.Adjusted  BAC.Adjusted ORCL.Adjusted  XOM.Adjusted GOOG.Adjusted 
# 0.08333333    0.08333333    0.08333333    0.08333333    0.08333333    0.08333333    0.08333333 
# WMT.Adjusted  PFE.Adjusted  JPM.Adjusted    T.Adjusted  CVX.Adjusted 
#  0.08333333    0.08333333    0.08333333    0.08333333    0.08333333 

# 12 × 3 table report the weight (in percentage) allocated to each asset. 
# Use the OUT sample to compute the return of each portfolio
R_OUT <- log(p_OUT/lag(p_OUT))
R_OUT <- na.omit(R_OUT)
head(R_OUT)
tail(R_OUT)

#Portfolio 1
R_OUT_P1 <-  R_OUT %*% w1 # returns on portfolio 1
R_OUT_P1_TOTAL <- apply(R_OUT_P1,2,sum) # column sum
R_OUT_P1_TOTAL
# [1] 0.1000919
#Portfolio 2
R_OUT_P2 <-  R_OUT %*% w2 # returns on portfolio 2
R_OUT_P2_TOTAL <- apply(R_OUT_P2,2,sum)
R_OUT_P2_TOTAL
# [1] 0.09281034
#Portfolio 3
R_OUT_P3 <-  R_OUT %*% w3 # returns on portfolio 3
R_OUT_P3_TOTAL <- apply(R_OUT_P3,2,sum)
R_OUT_P3_TOTAL
# [1] 0.1128321

#2.3

# Provide a plot showing the cumulative return of each portfolio with respect to the SPY
# get SPY data


# get SPY OUT data
tics <- c("SPY")
SPY <- get(getSymbols("SPY", src= "yahoo", from= as.Date("2019-01-02"), to= as.Date("2019-08-31")))
p_SPY_list <- list()
for(s in tics){
  cat(s,"\n")
  p_SPY_s <- get(getSymbols(s, src="yahoo", from= as.Date("2019-01-02"), to= as.Date("2019-08-31")))
  p_SPY_list <- c(p_SPY_list, list(p_SPY_s[,6]))
}
length(p_SPY_list)
p_SPY <- Reduce(merge,p_SPY_list)

# Calculate Return
R_OUT_SPY <- log(p_SPY/lag(p_SPY))
R_OUT_SPY <- na.omit(R_OUT_SPY)
head(R_OUT_SPY)

N_POINTS <- 167
x <-  seq (1, N_POINTS)
#Portfolio 1 plot
#plot(x,R_OUT_P1,type="p",main="Portfolio 1", xlab="Days", ylab="Cumulative Return")
y11 <-cumsum(R_OUT_SPY$SPY.Adjusted)
y12 <-cumsum(R_OUT_P1)
plot(x,y11 ,type="p",main="Portfolio 1", xlab="Days", ylab="Cumulative Return")
#points(x,R_OUT_SPY$SPY.Adjusted, col="red")
points(x,y12, col="red")

#Portfolio 2 plot
y21 <-cumsum(R_OUT_SPY$SPY.Adjusted)
y22 <-cumsum(R_OUT_P2)
plot(x,y21,type="l",main="Portfolio 2", xlab="Days",ylab="Cumulative Return")
lines(x,y22,col="red")

#Portfolio 3 plot
y31 <-cumsum(R_OUT_SPY$SPY.Adjusted)
y32 <-cumsum(R_OUT_P3)
plot(x,y31, type="l",main="Portfolio 3", xlab="Days",ylab="Cumulative Return")
lines(x,y32,col="red")
# Provide a summary table showing the SR, the beta, and the Jensen’s alpha for each portfolio


#Portfolio 1
m1 <- N_POINTS * apply(R_OUT_P1,2,mean)
vol1 <- sqrt(N_POINTS)*apply(R_OUT_P1,2,sd)
SR1 <- m1/vol1
# [1] 0.9039753
# run CAPM
R_1_MERGED <- merge(R_OUT_SPY, R_OUT_P1)
msr <- table.CAPM(R_1_MERGED[,2],R_1_MERGED[,1])
msr
alpha1 <- msr[1,]
alpha1
# -3e-04
beta1 <- msr[2,]
beta1
# 0.9654

#Portfolio 2
m2 <- N_POINTS * apply(R_OUT_P2,2,mean)
vol2 <- sqrt(N_POINTS)*apply(R_OUT_P2,2,sd)
SR2 <- m2/vol2
# [1] 0.6223605
# run CAPM
R_2_MERGED <- merge(R_OUT_SPY, R_OUT_P2)
msr <- table.CAPM(R_2_MERGED[,2],R_2_MERGED[,1])
msr
alpha2 <- msr[1,]
alpha2
# -6e-04
beta2 <- msr[2,]
beta2
# 1.1372

#Portfolio 3
m3 <- N_POINTS * apply(R_OUT_P3,2,mean)
vol3 <- sqrt(N_POINTS)*apply(R_OUT_P3,2,sd)
SR3 <- m3/vol3
# [1] 0.9893792
# run CAPM
R_3_MERGED <- merge(R_OUT_SPY, R_OUT_P3)
msr <- table.CAPM(R_3_MERGED[,2],R_3_MERGED[,1])
msr
alpha3 <- msr[1,]
alpha3
# -3e-04
beta3 <- msr[2,]
beta3
# 0.9654


# Discuss the absolute/relative performance of each. Which portfolio would you pick and why? What
# do these result say about portfolio selection compared to a passive fund as the SPY


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

#4 Value at Risk and Stress Testing
# 4.1 Task 1
# 4.1.1
# compute mean p and sigma p for p = 1, 2, 3 using the daily returns in the OUT period.

# Portfolio 1
#m1 <- 252*apply(R_OUT_P1,2,mean)
# 0.1510369
m1 <- 1*apply(R_OUT_P1,2,mean) # daily
#vol1 <- sqrt(252)*apply(R_OUT_P1,2,sd)
# 0.1360142
vol1 <- sqrt(1)*apply(R_OUT_P1,2,sd) # daily

# Portfolio 2
m2 <- 252*apply(R_OUT_P2,2,mean)
# 0.09281034
m2 <- 1*apply(R_OUT_P2,2,mean) # daily
vol2 <- sqrt(252)*apply(R_OUT_P2,2,sd)
# 0.1831877
vol2 <- sqrt(1)*apply(R_OUT_P2,2,sd) # daily

# Portfolio 3
m3 <- 252*apply(R_OUT_P3,2,mean)
# 0.1702616
m3 <- 1*apply(R_OUT_P3,2,mean) # daily 
vol3 <- sqrt(252)*apply(R_OUT_P3,2,sd)
# 0.1400915
vol3 <- sqrt(1)*apply(R_OUT_P3,2,sd) # daily


# 4.1.2
# simulate N = 1000 paths. Given the simulated paths, provide a distribution plot, e.g. 
# boxplot or histogram, for each portfolio. Provide a couple of insights
N_SIM <- 1000
N <-  seq(1,N_SIM)
N_DAYS <- 252

F_t1_ALL <-  matrix(, nrow = N_DAYS, ncol = N_SIM)
for (n in N) {
  # Portfolio 1
  t <- seq (1, N_DAYS)
  Zt <- rnorm(t, mean = 0, sd = 1)  
  F_01 <-  100
  F_t1 <- F_01 * exp( (m1 - vol1^2 / 2) * t + vol1 * Zt)
  F_t1_ALL[, n] <-  F_t1
}
hist(F_t1_ALL)

# Portfolio 2
F_t2_ALL <-  matrix(, nrow = N_DAYS, ncol = N_SIM)
for (n in N) {
  # Portfolio 1
  t <- seq (1, N_DAYS)
  Zt <- rnorm(t, mean = 0, sd = 1)  
  F_02 <-  100
  F_t2 <- F_02 * exp( (m2 - vol2^2 / 2) * t + vol2 * Zt)
  F_t2_ALL[, n] <-  F_t2
}
hist(F_t2_ALL)

# Portfolio 3
F_t3_ALL <-  matrix(, nrow = N_DAYS, ncol = N_SIM)
for (n in N) {
  # Portfolio 1
  t <- seq (1, N_DAYS)
  Zt <- rnorm(t, mean = 0, sd = 1)  
  F_03 <-  100
  F_t3 <- F_02 * exp( (m3 - vol3^2 / 2) * t + vol3 * Zt)
  F_t3_ALL[, n] <-  F_t3
}
hist(F_t3_ALL)

# 4.1.3
# What’s the expected value of each portfolio one year from now?
E_P1 <- mean(F_t1_ALL[252, ])
# 115.202

E_P2 <- mean(F_t2_ALL[252, ])
# 113.088 

E_P3 <- mean(F_t3_ALL[252, ])
# 117.3901

# 4.1.4
# With 95% level of confidence, what is the Value-at-Risk, i.e. V aR(0.05), for each portfolio?
P1 <- F_t1_ALL[252, ] 
P1_SORTED = sort(P1)
P1_VaR = P1_SORTED[50]
P1_VaR 
#  113.6219, so VaR is 0

P2 <- F_t2_ALL[252, ] 
P2_SORTED = sort(P2)
P2_VaR = P2_SORTED[50]
# 110.9185, so VaR is 0

P3 <- F_t3_ALL[252, ] 
P3_SORTED = sort(P3)
P3_VaR = P3_SORTED[50]
# 115.6425, so VaR is 0

# 4.2 Task 2
# To do so, you need to estimate the market beta beta p for each portfolio and the market 
# volatility sigma p during the OUT period.

# After doing so, consider the scenario
# in which the market volatility increases by a = 10% and generate 1000 paths for each portfolio.
# Given these simulations, compute the V aR(0.05) for each portfolio and summarize the results in a
# single table
# 4.2 Task 2
# To do so, you need to estimate the market beta beta p for each portfolio and the market 
# volatility sigma p during the OUT period.

# Portfolio 1
F_t1_ALL <-  matrix(, nrow = N_DAYS, ncol = N_SIM)
a = 0.10 # increase volatility by 10 percent
vol1 <-  vol1 + a
for (n in N) {
  t <- seq (1, N_DAYS)
  Zt <- rnorm(t, mean = 0, sd = 1)  
  F_01 <-  100
  F_t1 <- F_01 * exp( (m1 - vol1^2 / 2) * t + vol1 * Zt)
  F_t1_ALL[, n] <-  F_t1
}
P1 <- F_t1_ALL[252, ] 
P1_SORTED = sort(P1)
P1_VaR = P1_SORTED[50]
P1_VaR 
# 21.50, so VaR is 78.49

# Portfolio 2

F_t2_ALL <-  matrix(, nrow = N_DAYS, ncol = N_SIM)
vol2 <- vol2 + 0.10
for (n in N) {
  t <- seq (1, N_DAYS)
  Zt <- rnorm(t, mean = 0, sd = 1)  
  F_02 <-  100
  F_t2 <- F_02 * exp( (m2 - vol2^2 / 2) * t + vol2 * Zt)
  F_t2_ALL[, n] <-  F_t2
}

P2 <- F_t2_ALL[252, ] 
P2_SORTED = sort(P2)
P2_VaR = P2_SORTED[50]
P2_VaR
# 19.94983, so VaR is 80.05

# Portfolio 3
F_t3_ALL <-  matrix(, nrow = N_DAYS, ncol = N_SIM)
vol3 <- vol3 + 0.10
for (n in N) {
  t <- seq (1, N_DAYS)
  Zt <- rnorm(t, mean = 0, sd = 1)  
  F_03 <-  100
  F_t3 <- F_02 * exp( (m3 - vol3^2 / 2) * t + vol3 * Zt)
  F_t3_ALL[, n] <-  F_t3
}
P3 <- F_t3_ALL[252, ] 
P3_SORTED = sort(P3)
P3_VaR = P3_SORTED[50]
P3_VaR
# 22.19547, so VaR is 77.81










