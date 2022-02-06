# stock-simulation
3rd Year Group Project. Portfolio optimisation and simulation project, code by Tom Aldridge in R/Rshiny using RStudio.
-	Tasked with creating software in order to create and automatically manage optimal stock portfolios with user defined risk and profitability characteristics.
-	User was able to “buy” and “sell” individual stocks and add them to a cloud-based portfolio or use our bespoke algorithm to automatically generate a suitable portfolio. Automatic adjustments could be made by the software to sell unprofitable stocks and replace them with stocks that would suit the portfolio characteristics. 
-	Real-time stock data was imported from Yahoo Finance in order to generate covariance matrices for a shortlist of stocks that had desirable attributes. Constrained optimisation was performed to return the optimal stock weights within the portfolio, then time-series analysis was used to forecast the returns from the individual stocks. 
-	Project was delivered as a web app using R and various external packages such as RShiny and was accompanied by a 30-page report detailing the project, research and results.

Main portfolio page is app.R, shows portfolio, growth, value, and options to buy and sell individual stocks.
Optimisation and management page is beta.R, allows users to select risk profiles and automatically "invest" in a mathematically optimal portfolio. The user then has the option to "maintain" their portfolio, where the portfolio is adjusted to remove failing stocks are replaces them with a similar risk profile.

