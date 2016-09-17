## This function takes two arguments: the 2-character abbreviated name of a state and
## an outcome name. The function reads the outcome-of-care-measures.csv ???le and
## returns a character vector with the name of the hospital that has the best
## (i.e. lowest) 30-day mortality for the speci???ed outcome in that state

## There are only 3 valid outcome names: "Heart Attack", "Heart Failure", and "Pneumonia"

## This function relies on the 'dplyr' and the 'stringr' packages, which must be installed and called 
## prior to execution

library(dplyr)

library(stringr)

## Read Outcome data

best <- function(state, outcome) {
  
  bestData <- read.csv("outcome-of-care-measures.csv",
                       colClasses = c("NULL", "character", rep("NULL",4), "character", rep("NULL", 3), "character", 
                                      rep("NULL", 5), "character", rep("NULL", 5), "character", rep("NULL",23)),
                       na.strings = "Not Available",
                       stringsAsFactors = FALSE)


## Simplify relevant column names to match valid outcome names
  
  names(bestData)[names(bestData) == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"] <- "Heart Attack"
  names(bestData)[names(bestData) == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"] <- "Heart Failure"
  names(bestData)[names(bestData) == "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"] <- "Pneumonia"


## Validate state and outcome inputs
  
  if(!(state %in% bestData$State))
    stop("invalid state")
  
  if(!(outcome %in% names(bestData)))
    stop("invalid outcome")

  
## subset and then filter dataframe to select related columns and rows respectively

outcomes <- c("Heart Attack" = 3, "Heart Failure" = 4, "Pneumonia"= 5)

sub1 <- na.omit(select(bestData, c(1, 2, outcomes[outcome])))

sub2 <- filter(sub1, sub1$State == state)


## Standardize/clean outcome column prior to sorting it. {stringr}

sub2[outcome] <- lapply(sub2[outcome], function(x) {str_pad(x, width = 4, pad = "0")})


## Sort dataframe by state, outcome, and hospital name

arr1 <- arrange(sub2, State, sub2[outcome], Hospital.Name)

## Return the Hospital Name with the lowest mortality rate for a selected outcome in
## a selected state

result <- arr1[1, ]

result


}
## End of Function