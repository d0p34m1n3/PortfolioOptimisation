library(PortfolioAnalytics)

# ShortCuts
Symbols <- Candidates$Symbol

rownames(Returns) <- Returns$Date

BasePortfolio <- portfolio.spec(assets=Symbols)
BasePortfolio <- add.constraint(portfolio=BasePortfolio, type="weight_sum", max_sum=9999999)
BasePortfolio <- add.objective(portfolio=BasePortfolio, type="risk", name="StdDev")
print(BasePortfolio)

MinVarPortfolio <- optimize.portfolio(R = NAFreeReturns[, -1], portfolio = BasePortfolio, optimize_method = "ROI", trace = TRUE)
print(MinVarPortfolio)
