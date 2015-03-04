library(stringr)

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
    
    
    data <- NULL
    
    for(val in id) {
        filename <- paste(c(directory, "/", str_pad(val, 3, pad = "0"), ".csv"), collapse = "") #create filename
        df <- read.csv(filename)
        if(is.null(data)) {
            data <- df
        } else {
            data <- rbind(data, df) #concat dataframes
        }
    }
    
    vals <- data[[pollutant]] #extract relevant column
    
    filtered <- vals[!is.na(vals)] #remove NA entries
    
    mean(filtered) #return mean
}