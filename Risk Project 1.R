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

#assessing CAPM
rsq <- function(m[,], betas[,]) summary(lm(m~betas))$r.squared
rsq






