corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    result <- numeric(0)
    
    files <- list.files(path = directory, full.names = TRUE)
    
    for(file in files) {
        df <- read.csv(file)
        nobs <- sum(complete.cases(df))
        if(nobs > threshold) {
            cor <- cor(df$nitrate, df$sulfate, use = "complete.obs")
            result <- c(result, cor)
        }
    }
    
    result
}