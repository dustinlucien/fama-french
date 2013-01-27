#Massage Fidelity portfolio data into something a little more useful
library(quantmod)
library(zoo)

#Load Portfolio Data
portfolio = read.csv("../data/portfolio/current-portfolio-download.csv");
portfolio <- subset(portfolio, select=c(Symbol, Quantity, Cost.Per.Share));

colnames(portfolio)[1] = 'symbol';
colnames(portfolio)[2] = 'positions';
colnames(portfolio)[3] = 'cost';

portfolio$cost <- as.numeric(sub("\\$","", portfolio$cost));

positions <- aggregate(positions ~ symbol, data = portfolio, FUN = sum);
costbasis <- aggregate(cost ~ symbol, data = portfolio, FUN = mean);

portfolio <- merge(positions, costbasis)

portfolio[complete.cases(portfolio),];

#go get the most recent price for each security

quotes = getQuote(paste(portfolio$symbol, collapse = ';'));

portfolio$close <- quotes$Last;
portfolio$returns <- (portfolio$close - portfolio$cost)/portfolio$cost
portfolio$value <- portfolio$positions * portfolio$close;

total_value <- sum(portfolio$value);

portfolio$weight <- (portfolio$value / total_value);

portfolio$weighted.returns <- (portfolio$returns * portfolio$weight)

write.table(portfolio, "../data/portfolio/current-weighted-portfolio.csv", row.names=FALSE, sep=',');
