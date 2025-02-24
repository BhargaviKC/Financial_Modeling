Financial Modeling in R: Regression Analysis Project
Overview
This project applies regression analysis in R to study the relationship between key economic variables. It focuses on two key case studies:

1️) The impact of beer tax on vehicle fatalities
2️) The effect of profitability on a firm's capital structure (leverage)

Using econometric techniques such as fixed effects models, multivariate regression, and significance testing, we analyze how these factors influence policy and corporate finance decisions.

Project Structure
fatalities_analysis- Examines whether beer tax affects vehicle fatalities using state and year fixed effects models.
debt_ratio_analysis - Explores the impact of profitability on firm leverage using panel data regressions.
Contains datasets: Fatalities (AER package) & debt_ratio.csv (firm financials).

Key Findings
1️) Impact of Beer Tax on Vehicle Fatalities
Initial simple regression showed that beer tax had no significant impact on fatalities.
After controlling for income, unemployment, population, and alcohol-related fatalities, beer tax appeared positively correlated with fatalities (unexpected result).
However, state and year fixed effects reversed this relationship, indicating higher beer taxes reduce fatalities, reinforcing the importance of controlling for regional and time-based differences.
- Conclusion: Fixed effects matter! Ignoring them may misrepresent causal relationships.

2️) Profitability and Firm Leverage (Capital Structure)
Simple regression showed no significant relationship between profitability and leverage.
Adding firm controls (size, capex, industry leverage, market-to-book ratio) revealed a strong negative relationship, supporting the pecking order theory (firms use profits to reduce debt).
When including firm and year fixed effects, the relationship remained negative and statistically significant, reinforcing that profitable firms reduce leverage over time.
- Conclusion: Firm size and industry leverage increase debt usage, while high profitability and market-to-book ratios reduce leverage.

Regression Models Used
- Simple OLS Regression
- Multivariate Regression with Controls
- State & Year Fixed Effects Models
- Firm & Year Fixed Effects for Panel Data

Key Libraries Used: AER, lfe, stargazer, dplyr

Final Thoughts
This project demonstrates how controlling for unobserved factors (fixed effects) significantly changes empirical findings. Policymakers and firms must consider these complexities when interpreting data-driven insights.

Next Steps: Extend analysis using instrumental variables (IV) to further explore causality.
