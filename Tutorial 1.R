# tutorial 1

#average

mean_column <- numeric()
mean_column <- c(mean_column.mean(A[,1]))
mean_column <- c(mean_column.mean(A[,2]))
mean_column <- c(mean_column.mean(A[,3]))

#loop
1:ncol(A)
for(i in 1:ncol(A)){
  mean_column <- c(mean_column.mean(A[,i]))
}
mean_column

#apply function
#1 is the row
#2 is the column
#c(1,2) is the rows and columns
apply(A,2,mean)

#lapply fundtion
A1 <- matrix(1:9,3,3)
A2 <- matrix(11:19,3,3)
A31 <- matrix(101:109,3,3)

A_list <- list(A1,A2,A3)

#gives A1
A_list[[1]]

apply(A_list[[1]],2,mean)
apply(A_list[[2]],2,mean)
apply(A_list[[3]],2,mean)

lapply(A_list, function(A_l)apply(A_l[[1]],2,mean))

#financial Data
library(quantmod)
library(lubridate)
library(PerformanceAnalytics)

#getting data
tics <- c("SPY", "GOOG")

p <- get(getSymbols("SPY", from="2017-01-01"))
head(p)

dim(p)

#gives the adj.closed(column)
class(p)
p[,6]
plot(p[,6])

p[,6]
P$SPY.Adjusted
p[,"SPY.Adjusted"]

p_high_low <- p[,c("SPY.High")]

p_low <- p[,"SPY.Low"]
p_high <- p[,"SPY.High"]
head(p_high)

p_high_low <- merge(p_low, p_high)
head(p_high_low)

# Plot
plot(p_high_low)
plot(p_high_low, legend.loc="topleft")

# same command
p_high_low <- p[,c("SPY.Low","SPY.High")]
plot(p_high_low, legend.loc="topleft")

#getting data in loop
tics <- c("SPY", "GOOG", "AMZN")
p_list <- list()
for(s in tics){
  # shows the symbols names
  cat(s,"\n")
  p_s <- get(getSymbols(s, from= "2017-01-01"))
  p_list <- c(p_list, list(p_s))
}

length(p_list)

head(p_list[[1]])

# gives the min
lapply(p_list,min)

f_adj <- function(p_s){
  return (p_s[,6])
  }

p_list_adj <- lapply(p_list,f_adj)

head(p_list_adj[[1]])
head(p_list_adj[[2]])

#merging all the data together, column specified
p_adj <- merge(p_list_adj[[1]], p_list_adj[[2]])
p_adj <- merge(p_adj, p_list_adj[[3]])

p <- Reduce(merge,p_list_adj)
head(p)
summary(p)

# getting the log
class(p)
x <- p$SPY.Adjusted[1:10]
x
lag(x)
log(x/lag(x))

R <- log(p/lag(p))
R <- na.omit(R)
head(R)

#average of the adj column
m <- mean(R)
m <- 252*apply(R,2,mean)
m

barplot(m)

vol <- sqrt(252)*apply(R,2,sd)
vol
SR <- m/vol
SR

#plot with dots
plot(m-vol)






























