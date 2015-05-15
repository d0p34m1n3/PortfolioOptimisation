NoOfSymbols <- length(Symbols)
Cov <- cov(Returns)
Cov  <-  Cov *  (matrix(0.5, nrow=NoOfSymbols, ncol=NoOfSymbols) + 0.5 * diag(NoOfSymbols)) # Deflate Covariance Matrix
PCACov <- cov(Returns %*% PCAModel$rotation)

# FIX
#Steps <- rbind(diag(100, NoOfSymbols), diag(-100, NoOfSymbols))
Steps <- rbind(diag(1000, NoOfSymbols), diag(-1000, NoOfSymbols))
ClosePositionMultiplier <- matrix(1, nrow=NoOfSymbols, ncol=NoOfSymbols) - diag(NoOfSymbols)

CalcTransCost <- function(PriceTrading, Shares, AvgDailyShares, AnnualVolatility) {
  InstantImpact1  = 35 * (100 * abs(Shares) / AvgDailyShares) ^ 0.65 + 0.3 * (10000 * AnnualVolatility / sqrt(252)) + 15
  InstantImpact2  = 25 * (100 * abs(Shares) / AvgDailyShares) ^ 0.38 * (10000 * AnnualVolatility / sqrt(252)) ^ 0.28
  InstantImpact  = (InstantImpact1 + InstantImpact2) / 2
  MarketImpactCost  = InstantImpact * (0.95 * abs(Shares) / (abs(Shares) + AvgDailyShares / 2) + 0.05)
  PriceMovementCost   = 0
  FixedBrokerageCostBPS  = 0
  FixedBrokerageCostCents  = 1.75

  return ((MarketImpactCost + PriceMovementCost  + FixedBrokerageCostBPS) / 10000 + FixedBrokerageCostCents / PriceTrading / 100)
}

UtilityFunction <- function(Quantities) {
  Leverage <- as.numeric(sum(abs(Quantities) * Candidates$Price) / PortfolioValue)
  Beta <- as.numeric(sum(Quantities * Candidates$Price * Candidates$BetaPrediction) / PortfolioValue)
  ExpectedReturn <- ((1 + 0.10 * pmax(Leverage, 1)) ^ (1 / PeriodsPerYear) - 1)
  LongPositions <- sum(Quantities > 0)
  ShortPositions <- sum(Quantities < 0)

  SD <- as.numeric(sqrt(t(Quantities * Candidates$Price) %*% Cov %*% (Quantities * Candidates$Price) / PortfolioValue^2 * (252 / PeriodsPerYear)))
  PCA_SD <- as.numeric(sqrt(t(Quantities * Candidates$Price) %*% PCAModel$rotation %*% PCACov %*% t(t(Quantities * Candidates$Price) %*% PCAModel$rotation) / PortfolioValue^2 * 252 / PeriodsPerYear))
  UseSD <- pmax(PCA_SD, SD)

  Return  <- as.numeric(sum(Quantities * Candidates$Price * Candidates$ReturnPrediction) / PortfolioValue)

  LeveragePenalty <-
    if (Leverage > MaxLeverage) -Inf else
      (0.05 + -0.05 / (1 - (Leverage / MaxLeverage))) * ExpectedReturn

  PositionSizePenalty <- if(any(abs(Quantities * Candidates$Price) > MaxPositionSize * pmax(Leverage, 1) * PortfolioValue)) -Inf else 0

  TargetVolatilityPenalty <- if(UseSD == 0) -Inf else -log(UseSD * sqrt(PeriodsPerYear) / TargetVolatility)^2 * 20 * ExpectedReturn

  TransactionCostPenalty <- -2 * sum(CalcTransCost(Candidates$TCPriceTrading, abs(Candidates$CurrentQuantity - Quantities), Candidates$TCAvgVolume, Candidates$TCVolatility) * abs(Candidates$CurrentQuantity - Quantities) * Candidates$Price) / PortfolioValue

  UtilityScore <-
    if(UseSD == 0) -Inf else
      Return / UseSD * sqrt(PeriodsPerYear) +
    (LeveragePenalty + PositionSizePenalty + TargetVolatilityPenalty + TransactionCostPenalty) / pmax(UseSD * sqrt(PeriodsPerYear), TargetVolatility)

  Utility <- data.frame(NoOfLongPositions=LongPositions, NoOfShortPositions=ShortPositions, Leverage=Leverage, Beta=Beta,
                        SD=SD, PCA_SD=PCA_SD, Return=Return, LeveragePenalty=LeveragePenalty,
                        TargetVolatilityPenalty=TargetVolatilityPenalty, PositionSizePenalty=PositionSizePenalty,
                        TransactionCostPenalty=TransactionCostPenalty, Score=UtilityScore)
}

Quantities <- Candidates$CurrentQuantity
BestUtility <- UtilityFunction(Quantities)
Utility <- BestUtility
Iteration <- 0
Iterations <- data.frame()
MaxPositionsUtility <- BestUtility
MaxPositionsQuantities <- numeric()

repeat {
  Iterations <- rbind(Iterations, data.frame(Iteration=Iteration, Utility=BestUtility))
  Iteration <- Iteration + 1
  NewQuantities <- rep(1, times = 2 * NoOfSymbols) %*% t(Quantities) + Steps
  for (i in 1:(2 * NoOfSymbols)) {
    NewQuantities[i, NewQuantities[i, ] != 0 & abs(NewQuantities[i, ]) * Candidates$Price * Candidates$BetaPrediction <
                       sum(abs(NewQuantities[i, ] * Candidates$Price)) / NoOfPositionsToKeep / 4 &
                     abs(NewQuantities[i, ]) < abs(Quantities)] <- 0

  }
  NewUtilities <- foreach (Row = 1:nrow(NewQuantities), .combine=rbind) %do% UtilityFunction(NewQuantities[Row, ])
  NewUtility <- NewUtilities[NewUtilities$Score == max(NewUtilities$Score), ]
  if (NewUtility$Score <= BestUtility$Score) {
    break
  }
  BestUtility <- NewUtility
  Quantities <- NewQuantities[NewUtilities$Score==BestUtility$Score,]
  if (!is.null(nrow(Quantities)))
    if (nrow(Quantities) > 1) {
      Quantities <- Quantities[1, ]
    }

  if (sum(Quantities != 0) >= NoOfPositionsToKeep) {
    if (BestUtility$Score <= MaxPositionsUtility$Score) {
      BestUtility <- MaxPositionsUtility
      Quantities <- MaxPositionsQuantities
      break
    }
    else {
      MaxPositionsUtility <- BestUtility
      MaxPositionsQuantities <- Quantities

      for (j in 1:(NoOfPositionsToKeep / 10)) {
        NewQuantities <- ((rep(1, times = NoOfSymbols) %*% t(Quantities)) * ClosePositionMultiplier)[Quantities != 0, ]
        NewUtilities <- foreach (Row = 1:nrow(NewQuantities), .combine=rbind) %do% UtilityFunction(NewQuantities[Row, ])
        NewUtility <- NewUtilities[NewUtilities$Score == max(NewUtilities$Score), ]
        Quantities <- NewQuantities[NewUtilities$Score==NewUtility$Score,]
        if (!is.null(nrow(Quantities)))
          if (nrow(Quantities) > 1) {
            Quantities <- Quantities[1, ]
          }
      }
      BestUtility <- NewUtility
    }
  }

  cat(sprintf("%i : Long=%i, Shorts=%i, Leverage=%.1f, Beta=%.1f, Return=%.1f%%, SD=%.1f%%, PCA-SD=%.1f%%, LeveragePen=%.2f%%, TargetVolPen=%.2f%%, TransCostPen==%.2f%%, Score=%.3f\n",
              Iteration, BestUtility$NoOfLongPositions, BestUtility$NoOfShortPositions, BestUtility$Leverage, BestUtility$Beta,
              100*(((BestUtility$Return + 1) ^ 2 - pmax(BestUtility$SD, BestUtility$PCA_SD)^2)^(PeriodsPerYear / 2) - 1),
              100*BestUtility$SD*sqrt(PeriodsPerYear), 100*BestUtility$PCA_SD*sqrt(PeriodsPerYear),
              100*BestUtility$LeveragePenalty, 100*BestUtility$TargetVolatilityPenalty, 100*BestUtility$TransactionCostPenalty, BestUtility$Score))
}
