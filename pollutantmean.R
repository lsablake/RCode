pollutantmean <- function(directory, pollutant, ID=1:332) {
      all_files <- list.files(directory, full.names = TRUE) ## creates a list of all the individual files in the specdata directory

      all_data <- data.frame() ## creates empty dataframe to utilize in forthcoming 'rbind' argument
              for (i in ID) {
                all_data <- rbind(all_data, read.csv(all_files[i]))
              } ## This "For-loop" combines all the rows of each individual *.csv file in all_files list to create one large dataframe

      mean(all_data[, pollutant], na.rm = TRUE) ## subsets the all_data by column and returns mean value, excluding NA values
}