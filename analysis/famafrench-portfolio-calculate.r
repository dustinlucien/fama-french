# Goal: Using data from Yahoo finance, estimate the Fama-French Factors for any security
# using monthly returns
 
library(tseries);
library(zoo);

portfolio_file <- NULL

args <- commandArgs(TRUE);
if (length(args) < 1) {
	portfolio_file <- '../data/portfolio/alison-ira-allocation.csv';
} else {
	batch_args <- read.table(args[1], sep = ",");
	portfolio_file <- batch_args[1];
}

#Load Portfolio Data
portfolio <- read.csv(portfolio_file);

# Load FF factor returns
start_date_string <- "1990-11-01";
end_date_string <- "2012-11-30";

ff_returns <- read.table("../data/fama-french/Global_Factors-monthly.txt", header = TRUE);

#hackish way to get the french data dates in a workable monthly form
returns.Date <- paste(ff_returns$Date, '01', sep = "-");
ff_returns$Date <- NULL;

returns.Date <- strptime(returns.Date, '%Y%m-%d');
returns.Date <- as.yearmon(returns.Date);

ff_returns <- zoo(ff_returns, returns.Date);

rmrf <- ff_returns$RMRF/100;
smb <- ff_returns$SMB/100;
hml <- ff_returns$HML/100;
rf <- ff_returns$RF/100;

rj <- zoo(order.by = index(rf));
rj <- merge(rj, dummy = 0);

for (i in 1:nrow(portfolio)) {
	# Load Ticker Timeseries Data
	hist.prices <- get.hist.quote(portfolio[i,]$symbol, quote = "Adj", start = start_date_string, retclass = "zoo");
	hist.prices <- na.locf(hist.prices);             # Copy last traded price when NA
	 
	# To make weekly returns, you must have this incantation:
	monthly.prices <- aggregate(hist.prices, as.yearmon, tail, 1);
	 
	# Convert monthly prices to monthly weighted returns
	r <- diff(log(monthly.prices));
	r <- (exp(r)-1);

	rj <- (r * portfolio[i,]$weight) + rj;
}

rjrf <- rj - rf;

#window the vectors in case any securities haven't been around long enough
rmrf <- window(rmrf, start = start(rjrf), end = end(rjrf));
smb <- window(smb, start = start(rjrf), end = end(rjrf));
hml <- window(hml, start = start(rjrf), end = end(rjrf));

d <- lm(rjrf ~ rmrf + smb + hml);          # FF model estimation.
print(summary(d));
