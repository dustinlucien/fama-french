# Fama-French Regression example in R

# Load CSV file into R
ff_data <- read.table("ffdata.csv",header=TRUE,sep=",")

# Extract Fama-French Factors and Fund Returns
rmrf <- ff_data[,2]/100
smb <- ff_data[,3]/100
hml <- ff_data[,4]/100
rf <- ff_data[,5]/100

# Run Fama-French Regression
for(i in 6:194) {
	fund <- ff_data[,i]/100
	fund.xcess <- fund – rf
	ffregression <- lm(fund.xcess ~ rmrf + smb + hml)

	# Print summary of regression results
	print(summary(ffregression))
}