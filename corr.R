## This function takes a directory of data files and a threshold level of complete cases of data and
## calculates the correlation between sulfate and nitrate monitor locations where the 
## number of completely observed cases (on all observed variables) is greater than the threshold.
## This function returns a vector of correlations for the monitors that meet the threshold 
## requirement. If no monitors meet the threshold requirement, the function returns a numeric
## vector of length 0. 

source("complete.R")

corr <- function(directory, threshold=0) {

  meet_thrshld <- subset(complete(directory, id=1:332), nobs > threshold)
  
  thrshld_files <- list.files(directory, full.names = T)[meet_thrshld$id]
  
  cor_results <- numeric()
  
  idx <- 1
  
  v2 = numeric()
  
  for (value in seq_along(thrshld_files)) {
    
    v2[idx] <- value
    
    cor_data <- read.csv(thrshld_files[value])
    
    cr <- cor(cor_data$nitrate,cor_data$sulfate, use = "complete.obs")
    
    cor_results <- append(cor_results, cr)
    
    idx <- idx + 1
        
  }
      cor_results
}