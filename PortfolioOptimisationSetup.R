rm(list=ls())

ObjectDir <- 'F:/MindlessInvesting/R/Objects'
ScriptDir <- 'D:/MindlessInvesting/R/Scripts'
ConnectionString <- 'Server=CICERO\\SQLPROD;Database=MindlessInvesting;Trusted_Connection=True;Connection Timeout=600'

DivinationEntityId <- 1326696 # General Fundamentals
PortfolioEntityId <- 324893 # Portfolio A
Date <- '2014-08-08'
NoOfCandidatesToDownload <- 100
NoOfPositionsToKeep <- 100
TargetBeta <- 0.5
TargetVolatility <- 0.20
MaxPositionSize <- 0.05
MaxLeverage <- 4
PredictedMarketReturn <- 0.001
PredictedMarketDispersion <- 0.03 * 0.06 # 0.06 is the expected correlation.  In SH2 this might not be needed (unless I use override).
PredictedMarketVolatility <- 0.18/sqrt(252)
ReturnDamper <- 0.25
VolatilityDamper <- 0.25

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
                   sprintf("avg(coalesce(R.%sPrediction * %f, 0)) as ReturnPrediction, ", Divination$ShortName, 1-ReturnDamper),
                   sprintf("avg(coalesce(exp(log(R.%sStdDev) * %f), 1)) as VolatilityPrediction, ", Divination$ShortName, 1-VolatilityDamper),
                   sprintf("coalesce(exp(PC.ActualNext1WLogChange)-1, -999) as ActualReturn", Divination$ShortName),
                   sprintf("from PortfolioPositionsByDatePlus PP"),
                   sprintf("join Entities E"),
                   sprintf("on E.EntityId = PP.EntityId"),
                   sprintf("and E.EntityType <> 'X'"),
                   sprintf("left outer join %sRankings R", Divination$SourceView),
                   sprintf("on R.EntityId = PP.EntityId"),
                   sprintf("and R.Date = PP.Date", Date),
                   sprintf("and R.%sPrediction is not null", Divination$ShortName),
                   sprintf("and R.%sStdDev > 0", Divination$ShortName),
                   sprintf("left outer join PriceChanges PC"),
                   sprintf("on PC.EntityId = PP.EntityId"),
                   sprintf("and PC.Date = PP.Date"),
                   sprintf("where PP.PortfolioEntityId = %i", PortfolioEntityId),
                   sprintf("and PP.Date = '%s'", Date),
                   sprintf("group by PP.EntityId, E.Symbol, PC.ActualNext1WLogChange")
             ))
PortfolioPositions$ActualReturn[PortfolioPositions$ActualReturn==-999] <- NA_real_
Log("Portfolio Positions: %s", paste(sort(PortfolioPositions$Symbol), collapse=", "))

#Download Long Candidates
LongCandidates <-
  dbGetQuery(MDC,
             paste(sep="\n",
                   sprintf("select top %i R.EntityId, E.Symbol, ", NoOfCandidatesToDownload / 2),
                   sprintf("0 as CurrentQuantity, PC.AdjClose as Price, 0 as CurrentAmount,"),
                   sprintf("coalesce(R.%sPrediction * %f, 0) as ReturnPrediction, ", Divination$ShortName, 1-ReturnDamper),
                   sprintf("coalesce(exp(log(R.%sStdDev) * %f), 1) as VolatilityPrediction, ", Divination$ShortName, 1-VolatilityDamper),
                   sprintf("coalesce(exp(PC.ActualNext1WLogChange)-1, -999) as ActualReturn", Divination$ShortName),
                   sprintf("from %sRankings R", Divination$SourceView),
                   sprintf("join Entities E"),
                   sprintf("on E.EntityId = R.EntityId"),
                   sprintf("left outer join PriceChanges PC"),
                   sprintf("on PC.EntityId = R.EntityId"),
                   sprintf("and PC.Date = R.Date"),
                   sprintf("where R.PassesFilter = 1"),
                   sprintf("and R.Passes%sFilter = 1", Divination$Name),
                   sprintf("and R.Date = '%s'", Date),
                   sprintf("and R.%sPrediction is not null", Divination$ShortName),
                   sprintf("and R.%sStdDev > 0", Divination$ShortName),
                   sprintf("and R.EntityId not in (%s)", paste(PortfolioPositions$EntityId, collapse=", ")),
                   sprintf("order by R.%sPrediction * %f / exp(log(R.%sStdDev) * %f) desc",
                           Divination$ShortName, 1-ReturnDamper, Divination$ShortName, 1-VolatilityDamper)
             ))
Log("Long Candidates: %s", paste(sort(LongCandidates$Symbol), collapse=", "))
LongCandidates$ActualReturn[LongCandidates$ActualReturn==-999] <- NA_real_

#Download Long Candidates
ShortCandidates <-
  dbGetQuery(MDC,
             paste(sep="\n",
                   sprintf("select top %i R.EntityId, E.Symbol, ", NoOfCandidatesToDownload / 2),
                   sprintf("0 as CurrentQuantity, PC.AdjClose as Price, 0 as CurrentAmount,"),
                   sprintf("coalesce(R.%sPrediction * %f, 0) as ReturnPrediction, ", Divination$ShortName, 1-ReturnDamper),
                   sprintf("coalesce(R.%sStdDev, 1) as VolatilityPrediction, ", Divination$ShortName, 1-VolatilityDamper),
                   sprintf("coalesce(exp(PC.ActualNext1WLogChange)-1, -999) as ActualReturn", Divination$ShortName),
                   sprintf("from %sRankings R", Divination$SourceView),
                   sprintf("join Entities E"),
                   sprintf("on E.EntityId = R.EntityId"),
                   sprintf("left outer join PriceChanges PC"),
                   sprintf("on PC.EntityId = R.EntityId"),
                   sprintf("and PC.Date = R.Date"),
                   sprintf("where R.PassesFilter = 1"),
                   sprintf("and R.Passes%sFilter = 1", Divination$Name),
                   sprintf("and R.Date = '%s'", Date),
                   sprintf("and R.%sPrediction is not null", Divination$ShortName),
                   sprintf("and R.%sStdDev > 0", Divination$ShortName),
                   sprintf("and R.EntityId not in (%s)", paste(PortfolioPositions$EntityId, collapse=", ")),
                   sprintf("order by R.%sPrediction * %f / exp(log(R.%sStdDev) * %f)",
                           Divination$ShortName, 1-ReturnDamper, Divination$ShortName, 1-VolatilityDamper)
             ))
Log("Short Candidates: %s", paste(sort(ShortCandidates$Symbol), collapse=", "))
ShortCandidates$ActualReturn[ShortCandidates$ActualReturn==-999] <- NA_real_

# Create Candidates Table
Candidates <- rbind(PortfolioPositions, LongCandidates, ShortCandidates)
Candidates <- Candidates[order(Candidates$Symbol), ]

# Download Price Changes
Returns <-
  dbGetQuery(MDC,
             paste(sep="\n",
                   sprintf("select D.Date,"),
                   paste(paste0("coalesce(avg(iif(P.EntityId = ", Candidates$EntityId, ", P.AdjLogChange, null)), -999) as \"",
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
Log("Returns downloaded for the past 3 years: %s", prettyNum(nrow(Returns), digits=0, big.mark=","))


# Get the Portfolio Value
PortfolioValue <- dbGetQuery(MDC, sprintf("select NetValue from PortfolioValues where PortfolioEntityId = %i and Date = '%s'",
                                          PortfolioEntityId, Date))[1, 1]
Log("PortfolioValue: %s", formatC(PortfolioValue, format="d", big.mark=","))

# Close Connection
dbDisconnect(MDC)