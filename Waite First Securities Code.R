rm(list=ls(all=TRUE))
options(digits=12)
WFSStockData <- read.csv("WFS - 2015 to 2020 Stock Data - Adj. Close.csv", header=TRUE)
# Generate descriptive statistics
# Computation of Arithmetic mean return in percentage terms
newWFSStockData <- WFSStockData[-1,]
round(mean(newWFSStockData$SR_SP.500.Percentage),1)
round(mean(newWFSStockData$SR_AAPL.Percentage),1)
round(mean(newWFSStockData$SR_INTC.Percentage),1)
round(mean(newWFSStockData$SR_KR.Percentage),1)
# Computation of Geometric mean return in proportion terms
colnames(WFSStockData)
str(newWFSStockData$SR_SP.500)
str(newWFSStockData$SR_AAPL)
str(newWFSStockData$SR_INTC)
str(newWFSStockData$SR_KR)
# Creating new variable that represents the (return in month t) + 1
newWFSStockData <- transform(newWFSStockData, SR_SP.500plus1 = SR_SP.500 + 1)
newWFSStockData <- transform(newWFSStockData, SR_AAPLplus1 = SR_AAPL + 1)
newWFSStockData <- transform(newWFSStockData, SR_INTCplus1 = SR_INTC + 1)
newWFSStockData <- transform(newWFSStockData, SR_KRplus1 = SR_KR + 1)
geomeanreturn <- exp(mean(log(newWFSStockData$SR_SP.500plus1))) - 1
geomeanreturn <- exp(mean(log(newWFSStockData$SR_AAPLplus1))) - 1
geomeanreturn <- exp(mean(log(newWFSStockData$SR_INTCplus1))) - 1
geomeanreturn <- exp(mean(log(newWFSStockData$SR_KRplus1))) - 1
# Computation of Standard deviation of returns in percentage terms
round(sd(newWFSStockData$SR_SP.500.Percentage),1)
round(sd(newWFSStockData$SR_AAPL.Percentage),1)
round(sd(newWFSStockData$SR_INTC.Percentage),1)
round(sd(newWFSStockData$SR_KR.Percentage),1)
# Estimating Simple linear regression equations
# Extracting variables to be used in the analysis
sr_sp500 <- newWFSStockData[,"SR_SP.500"]
sr_aapl <- newWFSStockData[,"SR_AAPL"]
sr_intc <- newWFSStockData[,"SR_INTC"]
sr_kr <- newWFSStockData[,"SR_KR"]
# Regress Apple Stock return on S&P 500 Index
mod.1 <- lm(sr_aapl ~ sr_sp500)
summary(mod.1)
# Regress Intel Stock return on S&P 500 Index
mod.2 <- lm(sr_intc ~ sr_sp500)
summary(mod.2)
# Regress Kroger Stock return on S&P 500 Index
mod.3 <- lm(sr_kr ~ sr_sp500)
summary(mod.3)
# Calculating Coefficients of variation
round(sd(newWFSStockData$SR_SP.500.Percentage) / mean(newWFSStockData$SR_SP.500.Percentage),1)
round(sd(newWFSStockData$SR_AAPL.Percentage) / mean(newWFSStockData$SR_AAPL.Percentage),1)
round(sd(newWFSStockData$SR_INTC.Percentage) / mean(newWFSStockData$SR_INTC.Percentage),1)
round(sd(newWFSStockData$SR_KR.Percentage) / mean(newWFSStockData$SR_KR.Percentage),1)
