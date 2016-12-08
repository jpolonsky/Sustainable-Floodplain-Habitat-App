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
  
  # TODO: make this functional with lappy for purrr
  DF$p90 <- as.numeric(DF$p90)
  DF$p75 <- as.numeric(DF$p75)
  DF$p50 <- as.numeric(DF$p50)
  DF$p25 <- as.numeric(DF$p25)
  DF$p10 <- as.numeric(DF$p10)
  
  return(DF)
} 

