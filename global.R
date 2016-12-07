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

# Infobox Metrics ===============================================================

# Q flow Metric
q_metric <- function(d) {
  # allocate memory to output 
  results <- vector(length = 4, mode = "list")
  
  tresh <- 20000, 
  today_need <- tresh - flowData[d]
  
  }
