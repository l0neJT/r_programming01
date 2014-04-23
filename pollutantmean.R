pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    ## initialize character vector for file names
    files <- as.character()
    
    ## initialize numeric vector for pullutant readings
    readings <- as.numeric()
    
    ## populate 'files' from 'id'
    ## assumes 3 digit file name as CSV
    for(i in id) {
        iFile <- formatC(i, width = 3, format = "d", flag = "0")
        files <- c(files, paste(iFile, ".csv", sep = ""))
    }
    
    ## populate 'readings' from 'files' in 'directory
    ## reads all data from 'pollutatnt' column
    for(f in files) {
        fData <- read.csv(paste(directory, f, sep = "/"))
        readings <- c(readings, fData[[pollutant]])
    }
    
    ## return mean (ignoring NA values)
    mean(readings, na.rm = TRUE)
}