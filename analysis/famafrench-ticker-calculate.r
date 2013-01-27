# Goal: Using data from Yahoo finance, estimate the Fama-French Factors for any security
# using monthly returns
 
library(tseries)
library(zoo)

# Load FF factor returns
startyear = 2000;
startmonth = 11;
endyear = 2010;
endmonth = 10;
 
start = (startyear - 1926.5)*12+startmonth;
stop = (endyear - 1926.5)*12+endmonth;
 
ff_returns <- read.table("../data/fama-french/F-F_Research_Data_Factors-monthly.txt", header = TRUE);
rmrf <- ff_returns[start:stop,2]/100
smb <- ff_returns[start:stop,3]/100
hml <- ff_returns[start:stop,4]/100
rf <- ff_returns[start:stop,5]/100
 
# Load Fund Data
prices <- get.hist.quote("IVV", quote="Adj", start="2000-10-30", retclass="zoo")
prices <- na.locf(prices)               # Copy last traded price when NA
 
# To make weekly returns, you must have this incantation:
monthly.prices <- aggregate(prices, as.yearmon, tail, 1)
 
# Convert monthly prices to monthly returns
r <- diff(log(monthly.prices))
r1 <- exp(r)-1
 
# Now shift out of zoo to become an ordinary matrix --
rj <- coredata(r1)
rj <- rj[1:120]
rjrf <- rj - rf
 
d <- lm(rjrf ~ rmrf + smb + hml)               # FF model estimation.
print(summary(d))