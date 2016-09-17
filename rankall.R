## rankall.R script

## The rankall.R function takes two arguments: an outcome name (outcome) and a hospital ranking (num).

## The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame containing the hospital
## in each state that has the ranking specified in num.

## 0. This function relies on the 'dplyr' and the 'stringr' packages, which must be installed and called 
## prior to execution

library(dplyr)

library(stringr)


##------------------------------------------------------------------------------------------------------
## 1. Read relevant outcome data from *.csv file

rankall <- function(outcome, num = "best") {
  
  HospData <- read.csv("outcome-of-care-measures.csv", 
                       colClasses = "character",
                       stringsAsFactors = FALSE,
                       na.strings = "Not Available")
  
  
## 2. Simplify outcome column names in HospData data structure
  
  names(HospData)[names(HospData) == 
                    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"] <- "Heart Attack"
  names(HospData)[names(HospData) == 
                    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"] <- "Heart Failure"
  names(HospData)[names(HospData) == 
                    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"] <- "Pneumonia"
  
  
## 3. Validate outcome argument
  
  if(!(outcome %in% names(HospData)))
    stop("invalid outcome")
  
  
## 4. Select the relevant columns of data from the HospData data structure {dplyr}
  
  outcomes <- c("Heart Attack" = 11, "Heart Failure" = 17, "Pneumonia" = 23)
  
  cleanData <- na.omit(select(HospData, c(2, 7, outcomes[outcome])))

  names(cleanData) = c("Hospital", "State", "Outcome")

  
## 5. Standardize/clean outcome data for sorting {stringr}
  
  cleanData$Outcome <- lapply(cleanData$Outcome, 
                              function(x) {str_pad(x, width = 4, pad = "0")})
  
  
## 6. Sort by outcome by Hospital within State {dplyr}
  
  sortData <- arrange(cleanData, State, Outcome, Hospital)   
  
  
## 7. Sort realData data structure by state
  
  sortData <- split(sortData, sortData$State)
  
  
## 8. Assign numerical values to string-based inputs to the "num" parameter
  
#  num <- ifelse(num == "best", 1, ifelse(num == "worst", length(sortData), as.numeric(num)))

  for(State in levels(sortData$State)) {

      stateHospitals <- sortData[sortData$State == State, ]
    
      ifelse(num == "best", 1, ifelse(num == "worst", nrow(stateHospitals), num))
}


## 9. subset row within each state by rank
  
  Ranked <- lapply(sortData, function(x) x[num, 1])
  
  
## 10. Break down the components of the rankResults list 
  
  states <- c(names(Ranked))
  
  hospValues <- unlist(Ranked)

  
## 11. Create dataframe to organize and report results
  
  endResult <- data.frame("Hospital" = hospValues, "State" = states, row.names = states)
  
endResult

  
}

## End of function