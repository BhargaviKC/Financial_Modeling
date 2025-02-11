##############################################################################################
# Course: Financial Modeling
# Created by: Bhargavi K Chandrasekaran
# University of Denver
##############################################################################################





##############################################################################################
# Load the required libraries/packages
##############################################################################################

library(tidyverse) 
library(tseries)
library(xts) 
library(fBasics) 
library(fPortfolio) 
library(PortfolioAnalytics) 
library(PerformanceAnalytics) 
library(quantmod) 
library(AER) 
library(stargazer)
library(lfe) 

##############################################################################################
# Set some basic options 
##############################################################################################

options(scipen=999)

##############################################################################################
# Set the working directory to the desired location 
##############################################################################################

setwd("C:/Users/bharg/OneDrive - University of Denver/Documents/MASTERS at DU/Financial Modelling/Assignments")

##############################################################################################

# In this assignment, you, as a financial adviser, are going to help a client who is planing on constructing a portfolio of only stocks. The client needs to know how much money she should invest in each stock to achieve her investment goals. The client has already decided to invest in Bank of America Corp, Intel Corporation, Coca Cola Company, Walt Disney Company, Caterpillar Inc., and American Express Company. Use the data from May 1, 2015 to May 1, 2020 to answer the following questions. For each part of this question, write neat and efficient R codes to answer the questions. Test each line of your code to make sure it does whatever it is supposed to do before moving to the next line of the code. Write your codes below each part. 

tickers <-c("BAC", "INTC", "KO", "DIS", "CAT", "AXP")

head(tickers)

nstocks <- length(tickers)

nstocks

getSymbols(Symbols = tickers, from = "2015-05-01", to = "2020-05-01", src = "yahoo", auto.assign = TRUE)

boa.prices <- BAC 
int.prices <- INTC
cc.prices <- KO
dis.prices <- DIS  
cat.prices <- CAT 
amex.prices <- AXP

head(boa.prices)
head(int.prices)
head(cc.prices)
head(dis.prices)
head(cat.prices)
head(amex.prices)


# Part (a): To get a sense of the data, provide a summary statistics of both the adjusted daily stock prices, and the monthly returns. Save the summary stats in nicely formatted tables that can be added to a Word document if needed.  


# Calculate the monthly returns
boa.returns <-  monthlyReturn(boa.prices)
int.returns <- monthlyReturn(int.prices)
cc.returns <- monthlyReturn(cc.prices)
dis.returns <- monthlyReturn(dis.prices)  
cat.returns <- monthlyReturn(cat.prices) 
amex.returns <- monthlyReturn(amex.prices)

head(boa.returns)
head(int.returns)
head(cc.returns)
head(dis.returns)
head(cat.returns)
head(amex.returns)

# Put all the returns together and rename column names

returns.all <- cbind(boa.returns, int.returns, cc.returns, dis.returns, cat.returns, amex.returns)

colnames(returns.all) <- tickers
head(returns.all)

# Remove NAs if any

returns.all.clean <- na.omit(returns.all)
head(returns.clean)


# Convert data to a timeSeries object
returns.all.final <- timeSeries(data = returns.all.clean)


# Save as dataframe

returns.all.final.df <- as.data.frame(returns.all.clean)

View(returns.all.final.df)

# Get the adjusted prices

boa.prices.adjusted <- boa.prices$BAC.Adjusted 
int.prices.adjusted <- int.prices$INTC.Adjusted
cc.prices.adjusted <- cc.prices$KO.Adjusted
dis.prices.adjusted <- dis.prices$DIS.Adjusted
cat.prices.adjusted <- cat.prices$CAT.Adjusted
amex.prices.adjusted <- amex.prices$AXP.Adjusted

head(boa.prices.adjusted)
head(int.prices.adjusted)
head(cc.prices.adjusted)
head(dis.prices.adjusted)
head(cat.prices.adjusted)
head(amex.prices.adjusted)



# Put all the adjusted prices together and rename all column names. Save the adjusted price as a dataframe

adjustedprices.all <- cbind(boa.prices.adjusted, int.prices.adjusted, cc.prices.adjusted, cc.prices.adjusted, dis.prices.adjusted, amex.prices.adjusted)

colnames(adjustedprices.all) <- tickers
head(adjustedprices.all)

adjustedprices.all.df <- as.data.frame(adjustedprices.all)

View(adjustedprices.all.df)

# Save

stargazer(title = "Monthly Returns and Adjusted Prices", returns.all.final.df, adjustedprices.all.df, out = "MonthlyReturns_AdjPrices.doc", type = "text", digits = 1)



# Part (b): Plot the daily stock prices for each stock in separate graphs. Use proper titles for each graph. 

if(!is.null(dev.list())){dev.off()}

par(mfrow = c(3,2))

plot.xts(boa.prices.adjusted$BAC.Adjusted, main = "Bank of America Prices")
plot.xts(int.prices.adjusted$INTC.Adjusted, main = "Intel Prices")
plot.xts(cc.prices.adjusted$KO.Adjusted, main = "Coca Cola Prices")
plot.xts(dis.prices.adjusted$DIS.Adjusted, main = "Disney Prices")
plot.xts(cat.prices.adjusted$CAT.Adjusted, main = "Caterpillar Prices")
plot.xts(amex.prices.adjusted$AXP.Adjusted, main = "American Express Prices")



# Part (c): Plot the "2017" daily stock prices for Bank of America and Intel Corporation in separate graphs. 

if(!is.null(dev.list())){dev.off()}

par(mfrow = c(1,2))

plot.xts(boa.prices.adjusted$BAC.Adjusted ["2017"], main = "Bank of America Prices")
plot.xts(int.prices.adjusted$INTC.Adjusted["2017"], main = "Intel Prices")


# Part (d): If the client would like to go long in each stock and wishes to have an efficient portfolio with the lowest risk possible (global minimum variance portfolio), how much does she need to invest in each asset? 

#define specification

def.spec <- portfolioSpec()
print (def.spec)

#define long only constraint

def.const <- "LongOnly"
print(def.const)

# get minimum variance portfolio

return.min <- minvariancePortfolio(data = returns.all.final,spec = def.spec, constraints = def.const)
return.min.weights <- getWeights(return.min)

print(return.min.weights)


# Part (e): Provide a plot of the weights you obtained in part (d). Use proper title and axis labels for the graph. 

if(!is.null(dev.list())){dev.off()}

barplot(return.min.weights, legend = tickers, col = rainbow(nstocks),main = "Portfolio Investment Weights",xlab = "Companies", ylab = "Weights" )



# Part (f): How much is the expected annualized return and risk of the global minimum variance portfolio? 

return.min.weights.return <- getTargetReturn(return.min)[1] * 12

print(return.min.weights.return)

return.min.weights.risk <- getTargetRisk(return.min)[2] * sqrt(12)

print(return.min.weights.risk)




# Part (g): If the client would like to go long in each stock and wishes to have an efficient portfolio with the highest Sharpe Ratio (optimal portfolio), how much does she need to invest in each asset, assuming the monthly risk-free rate is 0.1%? 

#define specification

rf.spec <- portfolioSpec()
setRiskFreeRate(rf.spec) <- 0.001
print (rf.spec)

#define long only constraint

def.const <- "LongOnly"
print(def.const)

# get optimalportfolio

return.optimal <- tangencyPortfolio(data = returns.all.final ,spec = rf.spec, constraints = def.const)

return.optimal.weights <- getWeights(return.optimal)

print(return.optimal.weights)


# Part (h): How much is the expected annualized return and risk of the optimal portfolio obtained from part (g)?

return.optimal.weights.return <-getTargetReturn(return.optimal)[1]*12

print(return.optimal.weights.return)

return.optimal.weights.risk <- getTargetRisk(return.optimal)[2]* sqrt(12)

print(return.optimal.weights.risk)



# Part (i): Provide a barplot of the weights corresponding to the portfolios on the efficient frontier set. Also, provide a plot showing the efficient frontier set along with the tangency portfolio, global min variance portfolio, and the equally weighted portfolio.

# Let's look at the efficient frontier set curve

efficient.set <- portfolioFrontier(data = returns.all.final, spec = def.spec, constraints = def.const)

print(efficient.set)

return.eff.weights <- getWeights(efficient.set)

print(return.eff.weights)


# Plot the frontier

if(!is.null(dev.list())){dev.off()}

plot.fPORTFOLIO(x = efficient.set, which = c(1, 2, 3, 5))

# Get bar plot

if(!is.null(dev.list())){dev.off()}

par(mfrow = c(1,2))

barplot(t(return.eff.weights), col = rainbow(nstocks), legend = tickers,main = "Weights Plot",xlab = "Points", ylab = "Weights")



# Part (j): If the client would like to go long in each stock and wishes to have an efficient portfolio with a target annual return of 10.8%, how much does she need to invest in each stock? What is the expected annual risk of the portfolio? 


# returns.all.final is in timeseries

# define target specification

target.spec <- portfolioSpec()
setTargetReturn(target.spec) <- 0.108 / 12
print(target.spec)

# Define constraint

def.const <- "LongOnly"
print(def.const)

# get efficient portfolio

return.eff.port <- efficientPortfolio(data = returns.all.final, spec = target.spec, constraints = def.const)

print(return.eff.port)

# get annual efficient portfolio return
return.effport.return <- getTargetReturn(return.eff.port)[1]*12
print(return.effport.return)

# get annual efficient portfolio return
return.effport.risk <- getTargetRisk(return.eff.port)[2]* sqrt(12)
print(return.effport.risk)


# Part (k): If the client would like to go long in each stock and wishes to invest equally in each asset (the equally weighted portfolio), what would be the expected annual return and risk of this portfolio? (Hint: The equally weighted portfolio is not necessarily an efficient portfolio. To answer this question, make a proper vector of the weights and change the target weights option in the portfolio specification from NULL to the constructed weight vector. Then look at fPortfolio package manual and figure out what function should be used to solve this problem.)

# returns.all.final is in timeseries

# define specifications for equally weighted portfolio

wgt.spec <- portfolioSpec()


# create equal weights matrix

equal.weights.matrix <- matrix(data = 1.00 / nstocks, nrow = 1, ncol = nstocks)

colnames(equal.weights.matrix) <- tickers

print(equal.weights.matrix)

setWeights(wgt.spec) <- equal.weights.matrix

print (wgt.spec)

# Define constraints

def.const <- "LongOnly"

print(def.const)

# get the equal returns portfolio

return.equalport <- feasiblePortfolio(data = returns.all.final, spec = wgt.spec, constraints = def.const)

print(return.equalport)

# get the annual returns and risks
return.eqweights.return <- getTargetReturn(return.equalport)[1]*12

print(return.eqweights.return)

return.eqweights.risk <- getTargetRisk(return.equalport)[2]* sqrt(12)

print(return.eqweights.risk)


# Part (l): If the client works with a broker who allows her to short sell stocks, how much should she invest in each stock to achieve highest Sharpe Ratio portfolio, if the monthly risk free rate is 0? How much is the expected annual risk and return of the portfolio in this case? Provide a barplot of the optimal weights.

#Define SHORT specification

short.spec <- portfolioSpec()

setSolver(short.spec) <- "solveRshortExact"

print(short.spec)

# Define short constraint

short.const <- "Short"

print(short.const)

# get optimized portfolio

return.shortport <- tangencyPortfolio(data = returns.all.final, spec = short.spec, constraints = short.const)

print(return.shortport)

# get the optimized portfolio weights

return.shortport.weights <- getWeights(return.shortport)

print(return.shortport.weights)

# get annual return and risk

return.short.return <- getTargetReturn(return.shortport)[1]*12

print(return.short.return)

return.short.risk <- getTargetRisk(return.shortport)[2]* sqrt(12)

print(return.short.risk)

# get the barplot of weights

if(!is.null(dev.list())){dev.off()}

barplot(return.shortport.weights, col = rainbow(nstocks), legend = tickers, main = "Weights Plot", xlab = "Stocks", ylab = "Weights")

# Part (m): Put the expected annual returns and risk for the long only scenario calculated in part (h) and the short scenario values calculated in part (l) in one matrix. What changes do you see when short selling is allowed? 

return.comp.long <- cbind(return.optimal.weights.return, return.optimal.weights.risk)

return.comp.short <- cbind(return.short.return, return.short.risk)

return.comp <- rbind(return.comp.long, return.comp.short)

colnames(return.comp) <- c("Return", "Risk")

rownames(return.comp) <- c("Long", "Short")

print(return.comp)


print("In Short scenario, the Return is increasing with the Risk decreasing than the long only scenario. It is the other way around where the risk is higher with higher returns.")


# Part (n): Assume the client would like to invest at least 15% in each of Bank of America and Coca Cola stocks but no more than 50% in each, and also the client does not want to put more than 30% in Intel. If the client's goal is to form the optimal portfolio (highest Sharpe Ratio), how much does she need to invest in each stock? Answer the same question if the client is seeking to form the global minimum variance portfolio. Risk free rate is assumed to be 0. Provide side-by-side bar chart of the weights for these two portfolios with proper titles. 

# Define specification

def.spec <- portfolioSpec()
print (def.spec)

# define constraints for specific allocation

alloc.const <- c("minW[c('BAC', 'KO')] = 0.15", "maxW[c('BAC', 'KO')] = 0.50", "maxW[c('INTC')] = 0.30")
print(alloc.const)

# get the optimized portfolio
return.allocport <- tangencyPortfolio(data = returns.all.final, spec = def.spec,constraints = alloc.const)
print(return.allocport)

# get the weights for the optimized portfolio
return.allocport.weights <- getWeights(return.allocport)
print(return.allocport.weights)

# get the minimum variance portfolio
return.allocport.min <- minvariancePortfolio(data = returns.all.final, spec = def.spec,constraints = alloc.const)

print(return.allocport.min)

# get the wrights for minimum variance portfolio
return.allocport.min.weights <- getWeights(return.allocport.min)
print(return.allocport.min.weights)

# create bar plots

if(!is.null(dev.list())){dev.off()}
par(mfrow = c(1,2))

barplot(return.allocport.weights, col = rainbow(nstocks), legend = tickers,main = "Highest Sharp Weights Plot", xlab = "Stocks", ylab = "Weights")

barplot(return.allocport.min.weights, col = rainbow(nstocks), legend = tickers,main = "Minimum Varince Weights Plot", xlab = "Stocks", ylab = "Weights")


# Part (o): Assume the client would like to invest exactly 40% of her money in the group consisting of Intel and Walt Disney, at least 20% of her money in the group consisting of Caterpillar and American Express, and no more than 30% of her money in the group consisting of Caterpillar and Bank of America. If the client's goal is to form the optimal portfolio (highest Sharpe Ratio), how much does she need to invest in each stock? Answer the same question if the client's goal is to form the global minimum variance portfolio. Risk free rate is 0 (Hint: You learned in class about "minsumW, maxsumW, etc. The equal sum constraint is done through "eqsumW" option.). Provide side-by-side pie chart of the weights for these two portfolios.   

# Define Specifications

def.spec <- portfolioSpec()

print (def.spec)

# define constraints for the group 

alloc.const.group <- c("eqsumW[c('INTC', 'DIS')] = 0.40", "minsumW[c('CAT', 'AXP')] = 0.20", "maxsumW[c('CAT', 'BAC')] = 0.30")

print(alloc.const.group)

# get the optimized portifolio and its weights

return.allocport.group <- tangencyPortfolio(data = returns.all.final, spec = def.spec,constraints = alloc.const.group)

print(return.allocport.group)

return.allocport.group.weights <- getWeights(return.allocport.group)

print(return.allocport.group.weights)

# Get the minimum variance portfolio and its weights

return.allocport.group.min <- minvariancePortfolio(data = returns.all.final, spec = def.spec,constraints = alloc.const.group)

print(return.allocport.group.min)

return.allocport.group.min.weights <- getWeights(return.allocport.group.min)

print(return.allocport.group.min.weights)

# draw the pie charts for the two

if(!is.null(dev.list())){dev.off()}
par(mfrow = c(1,2))

pie(return.allocport.group.weights, col = rainbow(nstocks), labels = tickers, main = "Highest Sharp Weights Charts")

pie(return.allocport.group.min.weights, col = rainbow(nstocks), labels = tickers, main = "Minimum Varince Weights Charts")



# Part (p): Assume the client would like to invest at least 5% in each individual stock but does not wish to put more than 35% in the group consisting of Bank of America, Intel, and Walt Disney. If the client's goal is to form the optimal portfolio (highest Sharpe Ratio), how much does she need to invest in each stock? Answer the same question if the client's goal is to form the global minimum variance portfolio. Risk free rate is 0. Provide side-by-side pie chart of the weights for these two portfolios.   


# Define the portfolio specification

def.spec <- portfolioSpec()
print (def.spec)

# define constraints for group allocation

alloc.group <- c("minW[1:nstocks] = 0.05", "maxsumW[c('BAC', 'INTC', 'DIS')] = 0.35")
print(alloc.group)

# get the optimized portfolio and its weights

return.allocport.groupmax <- tangencyPortfolio(data = returns.all.final, spec = def.spec, constraints = alloc.group)

print(return.allocport.groupmax)

return.allocport.groupmax.weights <- getWeights(return.allocport.groupmax)

print(return.allocport.groupmax.weights)

# Get the minimum variance portfolio and its weights

return.allocport.groupmin <- minvariancePortfolio(data = returns.all.final, spec = def.spec,constraints = alloc.group)

print(return.allocport.groupmin)

return.allocport.groupmin.weights <- getWeights(return.allocport.groupmin)

print(return.allocport.groupmin.weights)


# plot pie charts for the optimized and minimum portfolio

if(!is.null(dev.list())){dev.off()}
par(mfrow = c(1,2))

pie(return.allocport.groupmax.weights, col = rainbow(nstocks), labels = tickers, main = "Highest Sharp Weights Charts")

pie(return.allocport.groupmin.weights, col = rainbow(nstocks), labels = tickers, main = "Minimum Varince Weights Charts")

##############################################################################################
# Conclusion & Inference
##############################################################################################
# In this study, we constructed an optimal stock portfolio for a client investing in six 
# selected stocks: BAC, INTC, KO, DIS, CAT, and AXP. The analysis covered risk-return 
# optimization, efficient frontier analysis, and the impact of short selling and constraints.
#
# The Global Minimum Variance Portfolio provided the lowest risk, while the Tangency Portfolio
# maximized the Sharpe Ratio. Custom constraints altered allocations, demonstrating that 
# investment restrictions influence risk-return trade-offs.
#
# Overall, the best strategy depends on the investor's risk tolerance, with the optimal portfolio
# outperforming the minimum variance portfolio in terms of risk-adjusted returns.
