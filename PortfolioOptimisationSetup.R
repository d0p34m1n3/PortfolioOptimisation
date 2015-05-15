rm(list=ls())

ObjectDir <- 'F:/MindlessInvesting/R/Objects'
ScriptDir <- 'D:/MindlessInvesting/R/Scripts'
ConnectionString <- 'Server=CICERO\\SQLPROD;Database=MindlessInvesting;Trusted_Connection=True;Connection Timeout=600'
DivinationEntityId <- 1348808 # Equity Model 4W
PortfolioEntityId <- 324893 # Portfolio A
Date <- '2015-01-23'

NoOfCandidatesToDownload <- 200
NoOfPositionsToKeep <- 100
TargetBeta <- 0.5
TargetVolatility <- 0.15
MaxPositionSize <- 0.05
MaxLeverage <- 4
PeriodsPerYear <- 52 / 4
PredictedMarketReturn <- (1 + 0.10) ^ (1/PeriodsPerYear) - 1
PredictedMarketDispersion <- 0.1 # 0.1 for 4 Weeks, 0.05 for 1 Week
PredictedMarketVolatility <- 0.18 / sqrt(PeriodsPerYear)

Beep <- function(n=3) {
  for (i in seq(n)) {
    system("rundll32 user32.dll, MessageBeep -1")
    Sys.sleep(.5)
  }
}

require(VGAM)
require(rsqlserver)
require(DBI)
require(caret)
require(MASS)
require(car)
require(doParallel)
require(scales)
require(dplyr)
require(tidyr)
require(xts)

cat("PORTFOLIO OPTIMISATION SETUP\n")

# Load all Common Functions
#source(sprintf("%s/CommonFunctions.R", ScriptDir))
Log <- function(message, ...) cat(format(Sys.time(), "%H:%M:%S"), "-", sprintf(message, ...), "\n")

# Connect to SQL Server
MDC <- dbConnect("SqlServer", url=ConnectionString)
Log("Connected to SQL Server")

# Download Divination
Divination <-
  dbGetQuery(MDC, paste(sep="\n",
                        sprintf("select EntityId, Name, ShortName, SourceView, "),
                        sprintf("PeriodType, GroupingLevel"),
                        sprintf("from DivinationsPlus where EntityId = %i", DivinationEntityId)
                        ))
Log("Divination: %s (%s)", Divination$Name, DivinationEntityId)

# Download Portfolio Entity
PortfolioEntity <-
  dbGetQuery(MDC,
             paste(sep="\n",
                   sprintf("select EntityId, Symbol, Name"),
                   sprintf("from Entities"),
                   sprintf("where EntityId = %i", PortfolioEntityId)
                   ))
Log("Portfolio: %s (%s)", PortfolioEntity$Name, PortfolioEntityId)

#Download Portfolio Positions
PortfolioPositions <-
  dbGetQuery(MDC,
             paste(sep="\n",
                   sprintf("select PP.EntityId, E.Symbol, "),
                   sprintf("convert(float, sum(PP.Quantity)) as CurrentQuantity, avg(PP.Price) as Price, sum(PP.Amount) as CurrentAmount,"),
                   sprintf("avg(coalesce(iif(PP.Quantity>0, R.%sLongPred, -R.%sShortPred), 0)) as AlphaPrediction, ",
                           Divination$ShortName, Divination$ShortName),
                   sprintf("avg(PCR.ChangeVolPrediction) as VolatilityPrediction, ",
                           Divination$ShortName, Divination$ShortName),
                   sprintf("avg(coalesce(PC.BetaPrediction, case when PP.Quantity > 0 then 1.25 else 0.75 end)) as BetaPrediction,"),
                   sprintf("coalesce(exp(PC.ActualNext1WLogChange)-1, -999) as ActualReturn1W,"),
                   sprintf("coalesce(exp(PC.ActualNext4WLogChange)-1, -999) as ActualReturn4W,"),
                   sprintf("2*dbo.CalcTransCost(TCPriceTrading, 20000/nullif(TCPriceTrading, 0), TCAvgVolumn13W_WMavg * TCSplitFactor, TCVolatility, null) as Penalty,"),
                   sprintf("TCPriceTrading, TCAvgVolumn13W_WMavg * TCSplitFactor as TCAvgVolume, TCVolatility"),
                   sprintf("from PortfolioPositionsByDatePlus PP"),
                   sprintf("join PriceChangesRankings PCR"),
                   sprintf("on PCR.EntityId = PP.EntityId"),
                   sprintf("and PCR.Date = PP.Date"),
                   sprintf("join Entities E"),
                   sprintf("on E.EntityId = PP.EntityId"),
                   sprintf("and E.EntityType <> 'X'"),
                   sprintf("left outer join %sRankings R", Divination$SourceView),
                   sprintf("on R.EntityId = PP.EntityId"),
                   sprintf("and R.Date = PP.Date", Date),
                   sprintf("and iif(PP.Quantity>0, R.%sLongPred, R.%sShortPred) is not null", Divination$ShortName, Divination$ShortName),
                   sprintf("and iif(PP.Quantity>0, R.%sLongStdDev, R.%sShrtStdDev) > 0", Divination$ShortName, Divination$ShortName),
                   sprintf("left outer join PriceChanges PC"),
                   sprintf("on PC.EntityId = PP.EntityId"),
                   sprintf("and PC.Date = PP.Date"),
                   sprintf("left outer loop join ("),
                   sprintf("  select EntityID as TCEntityId, Date as TCDate, [Close] as TCPriceTrading,"),
                   sprintf("  convert(real, AdjVolume13W_WMAvg) as TCAvgVolumn13W_WMavg, "),
                   sprintf("  convert(real, Volume) / nullif(AdjVolume, 0) as TCSplitFactor,"),
                   sprintf("  Prev52WAnnualStdevLogChange as TCVolatility"),
                   sprintf("  from PriceChanges"),
                   sprintf(") as TC"),
                   sprintf("on TC.TCEntityId = PP.EntityId"),
                   sprintf("and TC.TCDate = (select max(Date) from PriceChanges where EntityId = PP.EntityId and Date <= PP.Date)"),
                   sprintf("where PP.PortfolioEntityId = %i", PortfolioEntityId),
                   sprintf("and PP.Date = '%s'", Date),
                   sprintf("group by PP.EntityId, E.Symbol, PC.ActualNext1WLogChange, PC.ActualNext4WLogChange, "),
                   sprintf("TCPriceTrading, TCAvgVolumn13W_WMavg, TCSplitFactor, TCVolatility"),
                   sprintf("having sum(PP.Quantity) <> 0")
             ))
PortfolioPositions$ActualReturn1W[PortfolioPositions$ActualReturn1W == -999] <- NA_real_
PortfolioPositions$ActualReturn4W[PortfolioPositions$ActualReturn4W == -999] <- NA_real_
PortfolioPositions$ReturnPrediction[PortfolioPositions$Quantity < 0] <-
  (PortfolioPositions$AlphaPrediction[PortfolioPositions$Quantity < 0] - PortfolioPositions$Penalty[PortfolioPositions$Quantity < 0] / 0.1) *
  PredictedMarketDispersion + PredictedMarketReturn
PortfolioPositions$ReturnPrediction[PortfolioPositions$Quantity > 0] <-
  (PortfolioPositions$AlphaPrediction[PortfolioPositions$Quantity > 0] + PortfolioPositions$Penalty[PortfolioPositions$Quantity > 0] / 0.1) *
  PredictedMarketDispersion + PredictedMarketReturn
Log("Portfolio Positions: %s", paste(sort(PortfolioPositions$Symbol), collapse=", "))

#Download Long Candidates
LongCandidates <-
  dbGetQuery(MDC,
             paste(sep="\n",
                   sprintf("select top %i R.EntityId, E.Symbol, ", NoOfCandidatesToDownload / 2),
                   sprintf("0 as CurrentQuantity, PC.AdjClose as Price, 0 as CurrentAmount,"),
                   sprintf("coalesce(R.%sLongPred, 0) as AlphaPrediction, ", Divination$ShortName),
                   sprintf("PCR.ChangeVolPrediction as VolatilityPrediction, ", Divination$ShortName),
                   sprintf("coalesce(PC.BetaPrediction, 0.75) as BetaPrediction,"),
                   sprintf("coalesce(exp(PC.ActualNext1WLogChange)-1, -999) as ActualReturn1W,"),
                   sprintf("coalesce(exp(PC.ActualNext4WLogChange)-1, -999) as ActualReturn4W,"),
                   sprintf("2*dbo.CalcTransCost(TCPriceTrading, 20000/nullif(TCPriceTrading, 0), TCAvgVolumn13W_WMavg * TCSplitFactor, TCVolatility, null) as Penalty,"),
                   sprintf("TCPriceTrading, TCAvgVolumn13W_WMavg * TCSplitFactor as TCAvgVolume, TCVolatility"),
                   sprintf("from %sRankings R", Divination$SourceView),
                   sprintf("join PriceChangesRankings PCR"),
                   sprintf("on PCR.EntityId = R.EntityId"),
                   sprintf("and PCR.Date = R.Date"),
                   sprintf("join Entities E"),
                   sprintf("on E.EntityId = R.EntityId"),
                   sprintf("left outer join PriceChanges PC"),
                   sprintf("on PC.EntityId = R.EntityId"),
                   sprintf("and PC.Date = R.Date"),
                   sprintf("left outer loop join ("),
                   sprintf("  select EntityID as TCEntityId, Date as TCDate, [Close] as TCPriceTrading,"),
                   sprintf("  convert(real, AdjVolume13W_WMAvg) as TCAvgVolumn13W_WMavg, "),
                   sprintf("  convert(real, Volume) / nullif(AdjVolume, 0) as TCSplitFactor,"),
                   sprintf("  Prev52WAnnualStdevLogChange as TCVolatility"),
                   sprintf("  from PriceChanges"),
                   sprintf(") as TC"),
                   sprintf("on TC.TCEntityId = R.EntityId"),
                   sprintf("and TC.TCDate = (select max(Date) from PriceChanges where EntityId = R.EntityId and Date <= R.Date)"),
                   sprintf("where R.PassesFilter = 1"),
                   sprintf("and R.Passes%sFilter = 1", Divination$Name),
                   sprintf("and R.Date = '%s'", Date),
                   sprintf("and R.%sLongPred is not null", Divination$ShortName),
                   sprintf("and R.%sLongStdDev > 0", Divination$ShortName),
                   if (nrow(PortfolioPositions) > 0)
                     sprintf("and R.EntityId not in (%s)", paste(PortfolioPositions$EntityId, collapse=", ")),
                   sprintf("order by R.%sLongRskRwd desc", Divination$ShortName)
             ))
LongCandidates$ActualReturn1W[LongCandidates$ActualReturn1W == -999] <- NA_real_
LongCandidates$ActualReturn4W[LongCandidates$ActualReturn4W == -999] <- NA_real_
LongCandidates$ReturnPrediction <-
  (LongCandidates$AlphaPrediction + LongCandidates$Penalty / 0.1) * PredictedMarketDispersion + PredictedMarketReturn # 0.1 is the 4 week dispersion
Log("Long Candidates: %s", paste(sort(LongCandidates$Symbol), collapse=", "))

#Download Short Candidates
ShortCandidates <-
  dbGetQuery(MDC,
             paste(sep="\n",
                   sprintf("select top %i R.EntityId, E.Symbol, ", NoOfCandidatesToDownload / 2),
                   sprintf("0 as CurrentQuantity, PC.AdjClose as Price, 0 as CurrentAmount,"),
                   sprintf("-coalesce(R.%sShortPred, 0) as AlphaPrediction, ", Divination$ShortName),
                   sprintf("PCR.ChangeVolPrediction as VolatilityPrediction, ", Divination$ShortName),
                   sprintf("coalesce(PC.BetaPrediction, 0.75) as BetaPrediction,"),
                   sprintf("coalesce(exp(PC.ActualNext1WLogChange)-1, -999) as ActualReturn1W,"),
                   sprintf("coalesce(exp(PC.ActualNext4WLogChange)-1, -999) as ActualReturn4W,"),
                   sprintf("2*dbo.CalcTransCost(TCPriceTrading, 20000/nullif(TCPriceTrading, 0), TCAvgVolumn13W_WMavg * TCSplitFactor, TCVolatility, null) as Penalty,"),
                   sprintf("TCPriceTrading, TCAvgVolumn13W_WMavg * TCSplitFactor as TCAvgVolume, TCVolatility"),
                   sprintf("from %sRankings R", Divination$SourceView),
                   sprintf("join PriceChangesRankings PCR"),
                   sprintf("on PCR.EntityId = R.EntityId"),
                   sprintf("and PCR.Date = R.Date"),
                   sprintf("join Entities E"),
                   sprintf("on E.EntityId = R.EntityId"),
                   sprintf("left outer join PriceChanges PC"),
                   sprintf("on PC.EntityId = R.EntityId"),
                   sprintf("and PC.Date = R.Date"),
                   sprintf("left outer loop join ("),
                   sprintf("  select EntityID as TCEntityId, Date as TCDate, [Close] as TCPriceTrading,"),
                   sprintf("  convert(real, AdjVolume13W_WMAvg) as TCAvgVolumn13W_WMavg, "),
                   sprintf("  convert(real, Volume) / nullif(AdjVolume, 0) as TCSplitFactor,"),
                   sprintf("  Prev52WAnnualStdevLogChange as TCVolatility"),
                   sprintf("  from PriceChanges"),
                   sprintf(") as TC"),
                   sprintf("on TC.TCEntityId = R.EntityId"),
                   sprintf("and TC.TCDate = (select max(Date) from PriceChanges where EntityId = R.EntityId and Date <= R.Date)"),
                   sprintf("where R.PassesFilter = 1"),
                   sprintf("and R.Passes%sFilter = 1", Divination$Name),
                   sprintf("and R.Date = '%s'", Date),
                   sprintf("and R.%sShortPred is not null", Divination$ShortName),
                   sprintf("and R.%sShrtStdDev > 0", Divination$ShortName),
                   if (nrow(PortfolioPositions) > 0)
                     sprintf("and R.EntityId not in (%s)", paste(PortfolioPositions$EntityId, collapse=", ")),
                   sprintf("order by R.%sShrtRskRwd desc", Divination$ShortName)
             ))
ShortCandidates$ActualReturn1W[ShortCandidates$ActualReturn1W == -999] <- NA_real_
ShortCandidates$ActualReturn4W[ShortCandidates$ActualReturn4W == -999] <- NA_real_
ShortCandidates$ReturnPrediction <-
  (ShortCandidates$AlphaPrediction - ShortCandidates$Penalty / 0.1) * PredictedMarketDispersion + PredictedMarketReturn # 0.1 is the 4 week dispersion
Log("Short Candidates: %s", paste(sort(ShortCandidates$Symbol), collapse=", "))

#Download SPY
IndexCandidates <-
  dbGetQuery(MDC,
             paste(sep="\n",
                   sprintf("select PCR.EntityId, E.Symbol, "),
                   sprintf("0 as CurrentQuantity, PC.AdjClose as Price, 0 as CurrentAmount,"),
                   sprintf("0 as AlphaPrediction, "),
                   sprintf("PCR.ChangeVolPrediction as VolatilityPrediction, "),
                   sprintf("coalesce(PC.BetaPrediction, 0.8) as BetaPrediction,"),
                   sprintf("coalesce(exp(PC.ActualNext1WLogChange)-1, -999) as ActualReturn1W,"),
                   sprintf("coalesce(exp(PC.ActualNext4WLogChange)-1, -999) as ActualReturn4W,"),
                   sprintf("2*dbo.CalcTransCost(TCPriceTrading, 20000/nullif(TCPriceTrading, 0), TCAvgVolumn13W_WMavg * TCSplitFactor, TCVolatility, null) as Penalty,"),
                   sprintf("TCPriceTrading, TCAvgVolumn13W_WMavg * TCSplitFactor as TCAvgVolume, TCVolatility"),
                   sprintf("from PriceChangesRankings PCR"),
                   sprintf("join Entities E"),
                   sprintf("on E.EntityId = PCR.EntityId"),
                   sprintf("left outer join PriceChanges PC"),
                   sprintf("on PC.EntityId = PCR.EntityId"),
                   sprintf("and PC.Date = PCR.Date"),
                   sprintf("left outer loop join ("),
                   sprintf("  select EntityID as TCEntityId, Date as TCDate, [Close] as TCPriceTrading,"),
                   sprintf("  convert(real, AdjVolume13W_WMAvg) as TCAvgVolumn13W_WMavg, "),
                   sprintf("  convert(real, Volume) / nullif(AdjVolume, 0) as TCSplitFactor,"),
                   sprintf("  Prev52WAnnualStdevLogChange as TCVolatility"),
                   sprintf("  from PriceChanges"),
                   sprintf(") as TC"),
                   sprintf("on TC.TCEntityId = PCR.EntityId"),
                   sprintf("and TC.TCDate = (select max(Date) from PriceChanges where EntityId = PCR.EntityId and Date <= PCR.Date)"),
                   sprintf("where PCR.EntityId = dbo.GetTypedEntityId('SPY', 'E')"),
                   sprintf("and PCR.Date = '%s'", Date),
                   if (nrow(PortfolioPositions) > 0)
                     sprintf("and PCR.EntityId not in (%s)", paste(PortfolioPositions$EntityId, collapse=", "))
             ))
IndexCandidates$ActualReturn1W[IndexCandidates$ActualReturn1W == -999] <- NA_real_
IndexCandidates$ActualReturn4W[IndexCandidates$ActualReturn4W == -999] <- NA_real_
IndexCandidates$ReturnPrediction <- PredictedMarketReturn
Log("Index Candidates: %s", paste(sort(IndexCandidates$Symbol), collapse=", "))

# Create Candidates Table
Candidates <- rbind(PortfolioPositions, LongCandidates, ShortCandidates, IndexCandidates)
Candidates <- Candidates[order(Candidates$Symbol), ]

# Download Price Changes
Returns <-
  dbGetQuery(MDC,
             paste(sep="\n",
                   sprintf("select D.Date,"),
                   paste0(paste0("coalesce(avg(iif(P.EntityId = ", Candidates$EntityId, ", P.AdjLogChange, null)), -999) as \"",
                                Candidates$Symbol), "\"", collapse=",\n"),
                   sprintf("from DailyDates D"),
                   sprintf("left outer join Prices P"),
                   sprintf("on P.Date = D.Date"),
                   sprintf("where D.Date between DateAdd(year, -3, '%s') and '%s'", Date, Date),
                   sprintf("and P.EntityId in (%s)", paste(Candidates$EntityId, collapse=", ")),
                   sprintf("group by D.Date"),
                   sprintf("order by D.Date desc")
             ),
             commandTimeout=600)
for (Symbol in names(Returns[-1]))
  Returns[Returns[, Symbol] == -999, Symbol] <- NA_real_
rownames(Returns) <- Returns$Date
Returns$Date <- NULL
Returns <- Returns[sapply(rownames(Returns), function(Date) sum(!is.na(Returns[Date, ]))) > length(colnames(Returns)) / 2, ]
Returns <- Returns[, sapply(colnames(Returns), function(Symbol) sum(!is.na(Returns[, Symbol]))) > length(rownames(Returns)) / 2]
Log("Returns downloaded for the past 3 years: %s", prettyNum(nrow(Returns), digits=0, big.mark=","))

# Set Symbols and Dates
Symbols <- colnames(Returns)
Dates <- rownames(Returns)
Log("No of Symbols: %s", prettyNum(length(Symbols), digits=0, big.mark=","))
Log("No of Dates: %s", prettyNum(length(Dates), digits=0, big.mark=","))

# Reduce the Candidates List to those with Returns
Candidates <- Candidates[Candidates$Symbol %in% Symbols, ]

# Get the Portfolio Value
PortfolioValue <- dbGetQuery(MDC, sprintf("select NetValue from PortfolioValues where PortfolioEntityId = %i and Date = '%s'",
                                          PortfolioEntityId, Date))[1, 1]
Log("PortfolioValue: %s", formatC(PortfolioValue, format="d", big.mark=","))

# Close Connection
dbDisconnect(MDC)

# Fill in missing Values and convert to xts
NoOfMissingValues <- sum(is.na(Returns))
ImputeModel <- preProcess(Returns, method=c("center", "scale", "knnImpute"))
Returns <- predict(ImputeModel, Returns)
for (Symbol in Symbols)
  Returns[, Symbol][abs(Returns[, Symbol]) > 3] <- sign(Returns[, Symbol]) * 3
Returns <- sapply(Symbols, function(Symbol) Returns[, Symbol] * ImputeModel$std[Symbol] + ImputeModel$mean[Symbol])
Returns <- xts(Returns, order.by=as.Date(Dates))
RemainingMissingValues <- sum(is.na(Returns))
Log("Imputed %s missing values, %s remain", prettyNum(NoOfMissingValues - RemainingMissingValues, digits=0, big.mark=","),
    prettyNum(RemainingMissingValues, digits=0, big.mark=","))

# Create PCAs
PCAModel <- preProcess(Returns, method=c("center", "scale", "pca"), pcaComp=5)
PCAs <- predict(PCAModel, Returns) * PCAModel$std + PCAModel$mean
Log("Created PCA's")
