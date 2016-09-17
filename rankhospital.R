
## The following function (rankhospital) takes three arguments: the 2-character abbreviated name of a 
## state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).

## The function reads the outcome-of-care-measures.csv file and returns a character vector
## with the name of the hospital that has the ranking specified by the 'num' argument

## There are only 3 valid outcome arguments utilized  by the function: "Heart Attack", "Heart Failure", 
## and "Pneumonia"

## The hospital ranking argument (num) can be either the character-based inputs "best" and "worst", 
## or any integer

##------------------------------------------------------------------------------------------------------

## This function relies on the 'dplyr' and the 'stringr' packages, which must be installed and called 
## prior to execution

library(dplyr)

library(stringr)

##------------------------------------------------------------------------------------------------------
## 1. Read relevant outcome data

rankhospital <- function(state, outcome, num) {
  
  outcomes <- read.csv("outcome-of-care-measures.csv",
                       colClasses = "character",
                       stringsAsFactors = FALSE,
                       na.strings = "Not Available",
                       nrows = 10000)


## 2. Simplify mortality rate column names
  
  names(outcomes)[names(outcomes) == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"] <- "Heart Attack"
  names(outcomes)[names(outcomes) == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"] <- "Heart Failure"
  names(outcomes)[names(outcomes) == "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"] <- "Pneumonia"
  

## 3. Validate state and outcome inputs
  
  if(!(state %in% outcomes$State))
    stop("invalid state")
  
  if(!(outcome %in% names(outcomes)))
    stop("invalid outcome")


## 4. Subset dataframe to select relevant columns and rows respectively {dplr}

  col_outcome <- c("Heart Attack" = 11, "Heart Failure" = 17, "Pneumonia"= 23)

  sub1 <- na.omit(select(outcomes, c(2, 7, col_outcome[outcome])))
  
  sub2 <- filter(sub1, sub1$State == state)


## 5. Standardize/clean outcome column prior to sorting it. {stringr}
  
  sub2[outcome] <- lapply(sub2[outcome], function(x) {str_pad(x, width = 4, pad = "0")})
  

## 6. Sort dataframe by state, outcome, and hospital name {dplyr}

  sub2sort <- arrange(sub2, State, sub2[outcome], Hospital.Name)


## 7. Create ranking assignment if the 'num' argument is a valid character-based input

  if(is.character(num) == TRUE) {
    if(num == "best") {
      num = 1
    } else if(num == "worst") {
      num = nrow(sub2sort)
    } 
  }


## 8. Return the Hospital Name for a selected state and selected rank
##    by 30-day mortality rate

rankedResult <- sub2sort[num, ]

rankedResult

}
## End of function