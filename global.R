# Title: global.R 
# Author: 
# Description:

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

# Infobox Metrics ===============================================================

# Q flow Metric -----------------------------------------------------------------

# given a date get the mean daily for the flow data 
GetMeanDaily <- function(d) {
  flowData$mean_daily[which(flowData$date == d)]
}

# get the number of days until threshold is achieved
GetDaysUntilThreshold <- function(d, threshold, ciLevel="p10") {
  # create a dates to search through
  dateLookups <- seq(as.Date(d), by = "day", length.out = 100)
  valueLookups <- predictedFlowValues %>%
    filter(date > as.Date(d), key == ciLevel)
  
  resultDate <-valueLookups$date[which(valueLookups$value > 20000)][1]
  return(as.numeric(as.Date(resultDate) - as.Date(d)))
}

# fow showcase leave threshold at a default of 20000
Qmetric <- function(d, threshold=20000) {
  # allocate memory to output 
  results <- vector(length = 4, mode = "list")
  
  # compute the amount needed today
  todayNeed <- threshold - GetMeanDaily(d)
  
  # compute the days required until we reach the defined threshold
  
  
  list("Threshold" = threshold, 
       "NeedToday" = todayNeed)
  
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