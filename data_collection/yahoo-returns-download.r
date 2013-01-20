# Goal: download adjusted price data for selected security, convert to returns, and write to output file
 
library(tseries)
 
# Load security data from Yahoo! Finance
prices1 <- get.hist.quote("SPY", quote="Adj", start="2005-12-25", retclass="zoo")
 
prices1 <- na.locf(prices1)               # Copy last traded price when NA
 
# To make week end prices:
nextfri.Date <- function(x) 7 * ceiling(as.numeric(x - 1)/7) + as.Date(1)
weekly.prices <- aggregate(prices, nextfri.Date,tail,1)
 
# To convert month end prices:
monthly.prices <- aggregate(prices1, as.yearmon, tail, 1)
 
# Convert weekly prices into weekly returns
r.weekly <- diff(log(weekly.prices)) # convert prices to log returns
r1.weekly <- exp(r.weekly)-1          # back to simple returns
 
# Convert monthly prices into monthly returns
r.monthly <- diff(log(monthly.prices))  # convert prices to log returns
r1.monthly <- exp(r.monthly)-1          # back to simple returns
 
# Write output data to csv file
write.zoo(r1.weekly, file="weekly.csv",sep=",",col.names=c("Dates","Percent Return"))
write.zoo(r1.monthly, file="monthly.csv",sep=",",col.names=c("Dates","Percent Return"))
