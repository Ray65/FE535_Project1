library(quantmod)
library(lubridate)
library(PerformanceAnalytics)

# Getting Data
tics <- c("AAPL", "AMZN", "JNJ", "BAC", "ORCL", "XOM", "GOOG", "WMT", "PFE", "JPM", "T", "CVX")
#SPY <- get(getSymbols("SPY", src= "yahoo", from= as.Date("2014-08-01"), to= as.Date("2019-08-31")))
p_list <- list()
for(s in tics){
  cat(s,"\n")
  p_s <- get(getSymbols(s, src="yahoo", from= as.Date("2014-08-01"), to= as.Date("2019-08-31")))
  p_list <- c(p_list, list(p_s))
}
length(p_list)
head(p_list[[10]])
# ASK: only reads 10! doesn't return JPM, T and CVX

# Getting the adj.column
f_adj <- function(p_s){
  return (p_s[,6])
}
p_adj <- merge(p_list_adj[[1]], p_list_adj[[2]])
p_adj <- merge(p_adj, p_list_adj[[3]])
p_adj <- merge(p_adj, p_list_adj[[4]])
p_adj <- merge(p_adj, p_list_adj[[5]])
p_adj <- merge(p_adj, p_list_adj[[6]])
p_adj <- merge(p_adj, p_list_adj[[7]])
p_adj <- merge(p_adj, p_list_adj[[8]])
p_adj <- merge(p_adj, p_list_adj[[9]])
p_adj <- merge(p_adj, p_list_adj[[10]])
p_adj <- merge(p_adj, p_list_adj[[11]])
p_adj <- merge(p_adj, p_list_adj[[12]])

p <- Reduce(merge,p_list_adj)
head(p)
summary(p)

# Calculate Return
class(p)
x <- p$SPY.Adjusted[1:10]
x
lag(x)
log(x/lag(x))
x
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

