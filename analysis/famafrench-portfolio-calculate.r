# Goal: Using data from Yahoo finance, estimate the Fama-French Factors for any security
# using monthly returns
 
library(tseries);
library(zoo);

# Load FF factor returns
startyear = 1990;
startmonth = 11;
endyear = 2012;
endmonth = 11;
 
start = (startyear - 1926.5)*12+startmonth;
stop = (endyear - 1926.5)*12+endmonth;
 
ff_returns = read.table("F-F_Research_Data_Factors.txt", header=TRUE);

#hackish way to get the french data dates in a workable monthly form
ff_returns[[1]] <- paste(ff_returns[[1]], '01', sep="-");
ff_returns[[1]] <- strptime(ff_returns[[1]], '%Y%m-%d');
ff_returns[[1]] <- as.yearmon(ff_returns[[1]]);

ff_returns <- zoo(ff_returns[2:5], ff_returns$Date);

rmrf = ff_returns[start:stop, c('Date', 'RMRF')];
rmrf[2] <- rmrf[2]/100;

smb = ff_returns[start:stop, c('Date', 'SMB')];
smb[2] <- smb[2]/100;

hml = ff_returns[start:stop, c('Date', 'HML')];
hml[2] <- hml[2]/100;

rf = ff_returns[start:stop, c('Date', 'RF')];
rf[2] <- rf[2]/100;
 
#Load Portfolio Data
portfolio = read.csv("2013-01-portfolio-positions.csv");
portfolio <- subset(portfolio, select=c(Symbol, Quantity, Most.Recent.Price, Most.Recent.Value));

positions <- aggregate(Quantity ~ Symbol, data = portfolio, FUN = sum);
prices <- aggregate(Most.Recent.Price ~ Symbol, data = portfolio, FUN = max);
values <- aggregate(Most.Recent.Value ~ Symbol, data = portfolio, FUN = sum);

portfolio <- merge(positions, prices);
portfolio <- merge(portfolio, values);

colnames(portfolio)[1] = 'symbol';
colnames(portfolio)[2] = 'positions';
colnames(portfolio)[3] = 'price';
colnames(portfolio)[4] = 'value';

total_value <- sum(portfolio$value);

portfolio$weight <- (portfolio$value / total_value);

for (i in 1:nrows(portfolio)) {
	# Load Ticker Timeseries Data
	hist.prices <- get.hist.quote(portfolio[i,]$symbol, quote="Adj", start="1990-10-30", retclass="zoo");
	hist.prices <- na.locf(hist.prices);             # Copy last traded price when NA
	 
	# To make weekly returns, you must have this incantation:
	monthly.prices <- aggregate(hist.prices, as.yearmon, tail, 1);
	 
	# Convert monthly prices to monthly returns
	r <- diff(log(monthly.prices));
	r1 <- exp(r)-1;
	 
	# Now shift out of zoo to become an ordinary matrix --
	rj <- coredata(r1);
	#rj <- rj[1:120]
	rjrf <- rj - rf;
}
d <- lm(rjrf ~ rmrf + smb + hml);          # FF model estimation.
print(summary(d));