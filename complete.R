complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases

    
    ## initalize data frame for file-id-observation tuples
    monitors <- data.frame(id, nobs = numeric(length(id)))

    ## populate 'monitors$nobs'
    ## reads data for each file in 'monitors$id' to 'dataTemp'
    ## creates a logical vector 'isComplete' where TRUE means
    ## both sulfate and nitrate are not NA for a row in 'dataTemp'
    ## updates 'monitor$nobs' with sum('isComplete')
    for(i in monitors$id) {
        fileID <- formatC(i, width = 3, format = "d", flag = "0")
        file <- paste(fileID, ".csv", sep = "")
        dataTemp <- read.csv(paste(directory, file, sep = "/"))
        isComplete <- !is.na(dataTemp$nitrate) & !is.na(dataTemp$sulfate)
        monitors[monitors$id == i, "nobs"] <- sum(isComplete)
    }
    
    ## return 'monitors'
    monitors
}