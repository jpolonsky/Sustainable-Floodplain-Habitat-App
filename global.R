# Title: global.R 
# Author: 
# Description: A set of R global functions ti be used in app.R

# library imports 
library(readr)

# Data Imports ==================================================================

# Lat Long imports
gwlLatLong <- read_rds("gwlLatLong.rds")
screwtrapLatLong <- read_rds("screwtrapLatLong.rds")
flowLatLong <- read_rds("flowLatLong.rds")

# full data imports 
gwlData <- read_rds("groundWaterLevels.rds")
screwTrapData <- read_rds("screwTrapData.rds")
flowData <- read_rds("flowData_BND_1993_2016.rds")
predictedFlowValues <- read_rds("predictedFlowValues.rds")
shastaPreds <- read_rds("shasta_preds_gathered_by_ci.rds")
salmonRunsAggs <- read_csv("salmonRunsAggs.csv")
salmonRunsAggs$day.month.year <- as.Date(salmonRunsAggs$day.month.year, format = "%m/%d/%Y")
shasta_preds_v <- read_csv("shasta_preds_by_ci.csv")
shasta_preds_v$start <- as.Date(shasta_preds_v$start, format = "%m/%d/%Y")

# Infobox Metrics ===============================================================
# =================================================================================

# Q flow Metric -----------------------------------------------------------------

# given a date get the mean daily for the flow data 
GetMeanDaily <- function(d) {
  # date ranges do not match across datasets
  # change date is new date is obtained!!!!
  if (as.Date(d) > as.Date("2016-12-05")) {
    return(NULL)
  }
  flowData$mean_daily[which(flowData$date == d)]
}

# get the number of days until threshold is achieved
GetDaysUntilThreshold <- function(d, threshold=20000, ciLevel="p10") {
  
  # create a dates to search through
  dateLookups <- shastaPreds %>%
    filter(seed == d, key == ciLevel, value >= threshold)

  return(as.Date(dateLookups[1,1]) - as.Date(d))
}

# fow showcase leave threshold at a default of 20000
Qmetric <- function(d, threshold=20000) {
  # allocate memory to output 
  results <- vector(length = 2, mode = "list")
  
  # compute the amount needed today
  todayNeed <- threshold - GetMeanDaily(d)
  
  # compute the days required until we reach the defined threshold
  
  
  list("Threshold" = threshold, 
       "NeedToday" = todayNeed)
  
}

# Juvenile Fish Metrics ---------------------------------------------------------
GetSalmonAgg <- function(d) {
  yearData <- screwTrapData %>%
    filter(reading_year == format(as.Date(d), "%Y")) %>%
    select(SampleDate_formated, Unmarked) %>%
    group_by(SampleDate_formated) %>%
    summarise(
      daily_total = sum(Unmarked, na.rm = TRUE)
    )
  
  sum(yearData$daily_total[yearData$SampleDate_formated < as.Date(d)], na.rm = TRUE)
}

SalmonDifference <- function(d) {
  selected <- GetSalmonAgg(d)
  
  lookUpValue <- paste0("2016-",format(as.Date(d), "%m-%d"))
  historicalAgg <- salmonRunsAggs$Running.Sum.of.Unmarked[which(salmonRunsAggs$day.month.year == lookUpValue)]
  historicalAgg <- parse_number(historicalAgg)
  
  list(
    "HistoricalValue" = historicalAgg, 
    "Difference" = (historicalAgg - selected)
  )
}

GetHistoricalPercent <- function(d) {
  lookupData <- paste0("2016-",format(as.Date(d), "%m-%d"))
  proportion <- salmonRunsAggs$Percent.of.Run[which(salmonRunsAggs$day.month.year == lookupData)]
  parse_number(proportion)
}


# Infobox Helpers 

# change the color of the infobox depending on the needs of the day 
TodaysNeedColor <- function(d) {
  qM <- Qmetric(d)
  if (is.null(qM$NeedToday))
    "green"
  else if (qM$NeedToday <= 100)
    "green"
  else if (qM$NeedToday > 100 & qM$NeedToday < 10000)
    "yellow"
  else 
    "red"
}

# Global Objects =====================================================================
# ====================================================================================

# list smoothers that can be used 
smoothersList <- list(
  "Linear Regression Fit"="lm", 
  "Generalized Linear Fit"="glm", 
  "Local Regression Fit (loess)"="loess",
  "More Under Development"="more"
)






















