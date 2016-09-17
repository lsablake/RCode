## This function takes a directory of files and returns the number of complete cases 
## (i.e, rows without any 'NA' values) in each selected file of the directory. 
## Files are selected by file ID.

complete <- function(directory, id) {
      
      myfiles <- list.files(directory, full.names = TRUE) ## creates a list of the filenames in the directory
      
      idx <- 1 ##creates counter to index number files in directory
      
      v2 <- numeric() ##creates numeric vector in which to place counter

      allresults <- data.frame() ## initialize dataframe to aggregrate for-loop results
      
      for (value in id) {
        
              v2[idx] <- value  ##places the counter as subsetted value to select a specific file 
        
              mydata <- read.csv(myfiles[value]) ## loads the specific file into the 'mydata' dataframe
        
              nobs <- sum(complete.cases(mydata)) ## counts the rows in which complete.cases = T

              result <- c(value, nobs) ## creates vector of total complete cases by id
        
              allresults <- rbind(allresults, result) ##aggregate results all selected files

              idx <- idx + 1 ## add to the counter to move on to next file in directory
      }

      names(allresults) <- c("id", "nobs") ##assign column names to allresults dataframe
      
      allresults
}