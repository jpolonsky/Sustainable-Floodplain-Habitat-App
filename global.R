# Title: global.R 
# Author: 
# Description:

# library imports 
library(readr)

# Data Imports -----------------------------------------------------------------

# Lat Long imports
gwlLatLong <- read_rds("gwlLatLong.rds")
screwtrapLatLong <- read_rds("screwtrapLatLong.rds")
flowLatLong <- read_rds("flowLatLong.rds")

# full data imports 
gwlData <- read_rds("groundWaterLevels.rds")
screwTrapData <- read_rds("screwTrapData.rds")
flowData <- read_rds("flowData_BND_1993_2016.rds")
