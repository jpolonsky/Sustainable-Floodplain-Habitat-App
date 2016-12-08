# parse shasta prediction data 

rawData <- readLines("CNRFC_date/DLTC1_20161126to20170930.txt", n = 20)

ParseLine <- function(line) {
  line <- unlist(strsplit(line, split = " "))
  if (line[1] == "#") {
    return()
  }
  
  line <- line[line != ""]
  line <- line[c(1, 6, 7, 8, 9, 10)]
  return(line)
}


ParseFile <- function(filename) {
  rawData <- readLines(filename)
  
  rawDF <- matrix(unlist(lapply(rawData, ParseLine)), ncol=6, byrow=TRUE)
  
  DF <- as.data.frame(rawDF)
  colnames(DF) <- c("date", "p90", "p75","p50", "p25", "p10")
  DF$date <- as.Date(DF$date, format = "%m/%d/%Y")
  
  return(DF)
} 


ParseDir <- function(dir) {
  currentWd <- getwd()
  tempWD <- setwd(dir)
  listOfFiles <- list.files(".")
  print(listOfFiles)
  
  for (item in listOfFiles) {
    print(item)
    tempDF <- ParseFile(item)
    write.csv(tempDF, paste0(item,".csv"), row.names = FALSE)
  }
}

setwd("csv_version/")
csvList <- list.files(".")

for (csv in csvList) {
  temp <- read.csv(csv, stringsAsFactors = FALSE)
  temp$seed <- rep(temp[1,1], nrow(temp))
  
  write.csv(temp, paste(csv), row.names=FALSE)
  rm(temp)
}


df <- read.csv("DLTC1_20161123to20170930.txt.csv", stringsAsFactors = FALSE)
for (csv in csvList[-1]) {
  temp <- read.csv(csv, stringsAsFactors = FALSE)
  df <- rbind(temp, df)
  rm(temp)
}