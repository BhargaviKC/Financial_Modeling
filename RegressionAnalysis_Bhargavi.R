##############################################################################################
# Author: Bhargavi K Chandrasekaran
# Course: Financial Modeling
# University of Denver
##############################################################################################





##############################################################################################
# Load the required libraries/packages
##############################################################################################

library(tidyvrse) 
library(tseries)
library(xts) 
library(fBasics) 
library(fPortfolio) 
library(PortfolioAnalytics) 
library(PerformanceAnalytics) 
library(quantmod) 
library(stargazer)
library(AER)
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

# Q1: 
# Fatal car accidents under the influence of alcohol have increased recently in the country. Congress is thinking about increasing the tax on different kinds of beer in the hope of reducing the car accidents due to DIU. You are hired to provide an analysis of whether the change in beer tax affects the number of car accidents and, if so, what the magnitude of the effect is. To do so, you have collected data on many factors. Let's help the congressmen! 

# Part (a): Load a build-in dataset called Fatalities. Look at the help file and familiarize yourself with each single variable in the dataset. 

data("Fatalities", package = "AER")
help("Fatalities")


# Part (b): Provide a table of summary statistics of the data. Check the mean, min, and max for each variable and make sure the values look reasonable and there are not some crazy outliers. 

summary(Fatalities)


# Part (c): The main variable of interest is "fatal" which shows the number of vehicle fatalities and you want to see whether the variable "beertax" has any impact on the number of fatalities. First, run a simple regression including only beetrax as the control variable with no fixed effects. Look at the result. Is the sign of the coefficient what you expected? Is it significant? What is your interpretation of the coefficient? 

reg1 <- felm(fatal ~ beertax, data = Fatalities)
summary(reg1)

print("The coefficient for beer tax is positive, indicating a potential increase in fatalities with higher taxes. However, this result is not statistically significant, so it does not have any impact on the number of vehicle fatalities. The low R^2 suggests that beer tax alone is not a good predictor of vehicle fatalities. Hence, it may be necessary to consider additional variables or factors to better understand the relationship between beer tax and vehicle fatalities.")


# Part (d): As we know, alcohol is not the only factor causing car accidents. That means you have to control for other potential factors too. Now, look at other variables in the dataset. Decide which ones probably should be included in the regression. Run a regression including the beertax and those control variables you chose with no fixed effects. Look at the coefficient on beertax and provide your intuition. How about the coefficients of other variables in your model? Do they have the expected signs? For instance, I am told that Mormons do not drink (they drink only Pepsi?). If alcohol is a major cause of car accidents, the number of accidents should be lower in states with higher percentage of Mormons controlling for other factors. Is that the case in your regression? What is the coefficient on "mormon" variable? 

reg2 <- felm(fatal ~ beertax + mormon + income + miles + unemp + pop + afatal,  data = Fatalities)

summary(reg2)

print("This analysis shows that the beer tax has a statistically significant positive impact on vehicle fatalities. This is contrary to the expectation that higher beer taxes would reduce fatalities. The percentage of Mormons in a state does not significantly impact the number of fatalities, suggesting that the lower alcohol consumption among Mormons does not lead to fewer accidents. Other significant predictors include income (negative effect), miles driven (positive effect), unemployment rate (negative effect), population (positive effect), and alcohol-related fatalities (positive effect).")



# Part(e): Now, answer all the questions in part (d) but this time include STATE and YEAR fixed effects in your regression. How do your conclusions change when you include fixed effects in your models? What do these fixed effects do in these regressions? 

reg3 <- felm(fatal ~ beertax + mormon + income + miles + unemp + pop + afatal | state + year,  data = Fatalities)

summary(reg3)

print("Including state and year fixed effects reveals that higher beer taxes are linked to fewer vehicle fatalities, contrary to the previous model. This highlights the importance of accounting for state-specific and year-specific factors to accurately estimate beer tax impacts on fatalities. Other variables like income, unemployment, population, and alcohol-related fatalities remain significant. The proportion of Mormons and miles driven do not show significant impacts in this model.")

# Part (f): Repeat part (c), (d) and part (e) using "nfatal" (the number of night-time vehicle fatalities) as the main variable of interest. What do you conclude? 

reg4 <- felm(nfatal ~ beertax, data = Fatalities)

summary(reg4)

print ("Beer tax does not significantly impact night-time vehicle fatalities.")

reg5 <- felm(nfatal ~ beertax + mormon + income + miles + unemp + pop + afatal,  data = Fatalities)

summary (reg5)

print ("While beer tax remains insignificant, higher income, more miles driven, larger populations, and alcohol-related fatalities significantly increase night-time vehicle fatalities.")

reg6 <- felm(nfatal ~ beertax + mormon + income + miles + unemp + pop + afatal | state + year,  data = Fatalities)

summary(reg6)

print("Beer tax, mormon, income, miles, unemployment, and population are not significant predictors of night-time fatalities. However, alcohol-related fatalities remain a significant predictor. This highlights the importance of controlling for unobserved heterogeneity across states and over time.")


# Part (f): Put all the results of your regressions from parts (c), (d), (e) and (f) in one table and export it as a word document. 

stargazer(reg1, reg2, reg3, reg4, reg5, reg6, type = 'text', out = "fatalities_analysis.doc")


# Q2. 
# Download the debt_ratio.csv data from Canvas and load it into R. This database includes information on firms' capital structure and several control variables. The variable measuring debt ratio or leverage (known as capital structure) is called tdm in this database. In this question, we want to analyze the effect of profitability on firm's capital structure. Some theories predict that as firms become more profitable, they reduce their debt or leverage because they simply do not need to raise much money anymore and have ample internally generated money. Some theories predict the opposite. They argue that as firms become more profitable, they become less risky and can raise money in the debt market at lower cost so they tend to increase debt or leverage when their profitability increases. As you see one theory predicts a positive effect of profitability on leverage and one theory predicts negative effect of profitability on leverage. Therefore, we need to do empirical analyses and see which theoretical view is supported by real data. To do so, we run regressions in which we regress the tdm variable on a series of control variables including a measure of firm profitability. Let's do this! 

# Part(a): The main variable we are going to use are: tdm (leverage), lagprofit (profitability measure), lagassets (firm size), lagcapex (capital expenditure), lagindustlev (industry median leverage), and lagmktbk (firm market-to-book). First, create a table of summary statistics of these variables in a nicely formatted table. Check the table for any out of ordinary values to make sure there are no extreme outliers in your data. 
debt_data <- read.csv("debt_ratio.csv")

View (debt_data)

summary(debt_data)

print("From the summary statistics, I see potential outliers in variables such as 'tdm', 'lagprofit', 'lagassets', 'lagmktbk', and 'lagrnd'")


# Part(b): Regress tdm on only the measure of profitability with no fixed effects. Interpret the coefficient on profitability. What do you conclude? Which theory is supported by data? 

reg7 <- felm(tdm ~ lagprofit, data = debt_data)

summary(reg7)

print ("The summary shows that the coefficient for lagged profitability is positive but not statistically significant, suggests that there is no strong evidence to support a significant relationship between profitability and leverage based on this model. The data does not support either theory conclusively, however, the positive but insignificant coefficient aligns more closely with the theory that firms may slightly increase leverage with higher profitability.")


# Part(c): Profitability is not the only factor that could affect a firm's leverage. Run another regression with no fixed effects and this time use firm size, capital expenditure, industry median leverage, and firm market-to-book in addition to profitability in your regression. Provide a full interpretation of the all the coefficients. How does the coefficient on profitability compare to the coefficient from part (b)? 

reg8 <- felm(tdm ~ lagprofit + lagassets + lagcapex + lagindustlev + lagmktbk, data = debt_data)

summary(reg8)

print ("The results reveals that profitability, when considered with other controls, significantly reduces leverage, contrary to the initial model (reg7). Including more variables offers a better understanding. Larger firms and those with higher capital expenditures and industry leverage have more leverage, while firms with higher market-to-book ratios have less, supporting the pecking order theory")

# Part (d): Now, answer all the questions in part (b) and (c) but this time include FIRM and YEAR fixed effects in your regressions. How do the previous results change when fixed effects are included in the regression models? 

reg9 <- felm(tdm ~ lagprofit | permno + calyear, data = debt_data)

summary(reg9)

reg10 <- felm(tdm ~ lagprofit + lagassets + lagcapex + lagindustlev + lagmktbk | permno + calyear, data = debt_data)

summary(reg10)

print("Including firm and year fixed effects, along with control variables, confirms that profitability negatively impacts leverage, supporting the pecking order theory. Firm size and industry leverage positively influence leverage, while the market-to-book ratio negatively affects it. Capital expenditure is not significant, highlighting the importance of fixed effects.")

# Part(e): Put the regression estimates from parts (b), (c) and (d) in one table. 

stargazer(reg7, reg8, reg9, reg10, type = 'text', out = "debt_ratio_analysis.doc")

# THE END !!!
