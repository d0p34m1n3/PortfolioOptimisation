library(PortfolioAnalytics)
library(caret)

# Create Base Portfolio
BasePortfolio <- portfolio.spec(assets = Symbols, weight_seq = Candidates$CurrentAmount)

# Add Constraints
BasePortfolio <- add.constraint(portfolio = BasePortfolio, type = "weight_sum",
                                min = -MaxLeverage * PortfolioValue, max = MaxLeverage * PortfolioValue)
# BasePortfolio <- add.constraint(portfolio = BasePortfolio, type = "leverage_exposure",
#                                 leverage = MaxLeverage * PortfolioValue, message = T)
BasePortfolio <- add.constraint(portfolio = BasePortfolio, type = "box",
                               min = -MaxPositionSize * PortfolioValue, max = MaxPositionSize * PortfolioValue)
# BasePortfolio <- add.constraint(portfolio = BasePortfolio, type = "position_limit", max_pos = NoOfPositionsToKeep)
BasePortfolio <- add.constraint(portfolio = BasePortfolio, type = "factor_exposure", B = Candidates$BetaPrediction,
                               lower = (TargetBeta - 0.05) * PortfolioValue, upper = (TargetBeta + 0.05) * PortfolioValue)

UtilityFunction <- function(X) {
  print(X)
  mean(X) / sd(X)
}

# Add Objectives
add.objective(portfolio=BasePortfolio, type="return", name="UtilityFunction")
#BasePortfolio <- add.objective(portfolio=BasePortfolio, type="quadratic_utility", risk_aversion=30/PortfolioValue)
#BasePortfolio <- add.objective(portfolio=BasePortfolio, type="risk", name="StdDev")
#BasePortfolio <- add.objective(portfolio=BasePortfolio, type="return", name="mean", target=mean(Returns) * PortfolioValue * 3)
#print(BasePortfolio)


# Porform Optimisation
MVP <- optimize.portfolio(R = Returns, portfolio = BasePortfolio, optimize_method = "GenSA",  search_size=10, trace = TRUE, message=TRUE, verbose=TRUE)

print(MVP)
print(sum(round(MVP$weight) != 0))
print(min(MVP$weights))
print(max(MVP$weights))
print(sum(MVP$weight))
print(sum(abs(MVP$weight)))
print(sum(MVP$weight * Candidates$BetaPrediction))
