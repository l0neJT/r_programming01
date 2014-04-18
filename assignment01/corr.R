corr <- function(directory, threshold = 1) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    ## initialze numeric vector for storing correlation values
    correlations <- numeric()
    
    ## call 'complete' function on all monitors
    monitors <- complete("specdata")
    
    ## restrict monitors to 'threshold'
    monitors <- monitors[monitors$nobs >= threshold, ]
    
    ## populate 'correlations'
    ## reads data for each file in 'monitors$id' to 'dataTemp'
    ## appends correlation of complete nitrate-sulfate
    ## observations to 'correlations'
    for(i in monitors$id) {
        fileID <- formatC(i, width = 3, format = "d", flag = "0")
        file <- paste(fileID, ".csv", sep = "")
        dataTemp <- read.csv(paste(directory, file, sep = "/"))
        corTemp <- cor(dataTemp$nitrate, dataTemp$sulfate, use = "complete.obs")
        correlations <- c(correlations, corTemp)
    }
    
    ## return 'correlations'
    correlations
}