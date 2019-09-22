require("quantmod")
spy <- new.env()
stocks <- new.env()
getSymbols("SPY", env = spy, src = "yahoo", from = as.Date("2004-07-31"), to = as.Date("2019-09-01"))
SPY <- spy$SPY
getSymbols("AAPL", env = stocks, src = "yahoo", from = as.Date("2004-07-31"), to = as.Date("2019-09-01"))
AAPL <- stocks$AAPL
getSymbols("AMZN", env = stocks, src = "yahoo", from = as.Date("2004-07-31"), to = as.Date("2019-09-01"))
AMZN <- stocks$AMZN
getSymbols("JNJ", env = stocks, src = "yahoo", from = as.Date("2004-07-31"), to = as.Date("2019-09-01"))
JNJ <- stocks$JNJ
getSymbols("BAC", env = stocks, src = "yahoo", from = as.Date("2004-07-31"), to = as.Date("2019-09-01"))
BAC <- stocks$BAC
getSymbols("ORCL", env = stocks, src = "yahoo", from = as.Date("2004-07-31"), to = as.Date("2019-09-01"))
ORCL <- stocks$ORCL
getSymbols("XOM", env = stocks, src = "yahoo", from = as.Date("2004-07-31"), to = as.Date("2019-09-01"))
XOM <- stocks$XOM
getSymbols("GOOG", env = stocks, src = "yahoo", from = as.Date("2004-07-31"), to = as.Date("2019-09-01"))
GOOG <- stocks$GOOG
getSymbols("WMT", env = stocks, src = "yahoo", from = as.Date("2004-07-31"), to = as.Date("2019-09-01"))
WMT <- stocks$WMT
getSymbols("PFE", env = stocks, src = "yahoo", from = as.Date("2004-07-31"), to = as.Date("2019-09-01"))
PFE <- stocks$PFE
getSymbols("JPM", env = stocks, src = "yahoo", from = as.Date("2004-07-31"), to = as.Date("2019-09-01"))
JPM <- stocks$JPM
getSymbols("T", env = stocks, src = "yahoo", from = as.Date("2004-07-31"), to = as.Date("2019-09-01"))
T <- stocks$T
getSymbols("CVX", env = stocks, src = "yahoo", from = as.Date("2004-07-31"), to = as.Date("2019-09-01"))
CVX <- stocks$CVX
MergedStockData <- merge(AAPL, AMZN, JNJ, BAC, ORCL, XOM, GOOG, WMT, PFE, JPM, T, CVX, fill = 0)[, c("AAPL.Adjusted", "AMZN.Adjusted", "JNJ.Adjusted", "BAC.Adjusted", "ORCL.Adjusted", "XOM.Adjusted", "GOOG.Adjusted", "WMT.Adjusted", "PFE.Adjusted", "JPM.Adjusted", "T.Adjusted", "CVX.Adjusted")]
