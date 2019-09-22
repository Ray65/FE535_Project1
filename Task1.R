install.packages("bizdays")
require("bizdays")
#source("DatasetGen.R")
#tail(MergedStockData)


#Calculating annual mean returns of each stock
#cal <- create.calendar("Brazil/ANBIMA", holidaysANBIMA, weekdays=c("saturday", "sunday"))
#cal <- create.calendar(name='MyCalendar', weekdays=c('sunday', 'saturday'), adjust.from=adjust.next, adjust.to=adjust.previous)
cal <- create.calendar(name='MyCalendar', weekdays=c('sunday', 'saturday'))
#dates_2004 = bizseq(as.Date("2004-08-02"), as.Date("2004-12-31"), "Brazil/ANBIMA")
dates_2004 = bizseq(as.Date("2004-08-02"), as.Date("2004-12-31"), "MyCalendar")
StockData_2004 <- MergedStockData[index(dates_2004)]
dates_2005 = bizseq(as.Date("2005-01-01"), as.Date("2005-12-31"), "MyCalendar")
StockData_2005 <- MergedStockData[index(dates_2005)]
