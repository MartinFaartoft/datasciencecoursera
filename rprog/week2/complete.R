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
    n <- length(id)
    data <- data.frame(id=integer(n), nobs=integer(n))
    
    for(index in 1:n) {
        val <- id[index]
        filename <- paste(c(directory, "/", str_pad(val, 3, pad = "0"), ".csv"), collapse = "") #create filename
        df <- read.csv(filename)
        data$id[index] <- val
        data$nobs[index] <- sum(complete.cases(df))
    }
    
    data
}