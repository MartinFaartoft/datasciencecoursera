rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    
    ##convert columns to numeric
    data[, 11] <- as.numeric(data[, 11])
    data[, 17] <- as.numeric(data[, 17])
    data[, 23] <- as.numeric(data[, 23])
    
    ## Check that outcome is valid
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    if (!(outcome %in% valid_outcomes)) {
        stop("invalid outcome")
    }
    
    ##Find column that matches outcome (11 = heart attack, 17 = heart failure, 23 = pneumonia)
    cols <- c(11, 17, 23)
    ix <- match(outcome, valid_outcomes)
    col <- cols[ix]
    
    ## Return hospital name in that state with lowest 30-day death rate
    
    ##filter out hospitals with no data for outcome
    data <- data[!is.na(data[col]), ]
    
    ##split on state
    s <- split(data, data$State)
    rank_and_select <- function(dat) {
        ##determine index of row to return
        row_count <- nrow(dat)
        
        row_index <- if(num == "best") 1 else if (num == "worst") row_count else as.integer(num)
        #print(row_count)
        #print(row_index)
        if (row_index > row_count) {
            return(data.frame(Hospital.Name = NA, State = data$State[1]))
        }
        
        ix <- order(dat[col], dat[2])
        
        ##sort remaining by col,name ascending
        sorted <- dat[ix, ]
        #print(head(sorted[, c(7,2,col)]))
        ##return name of hospital with lowest mortality rate    
        sorted[row_index, c(2, 7)]
    }
    
    l <- lapply(s, rank_and_select)
    
    df <- do.call(rbind, l)
    names(df)[names(df) == "Hospital.Name"] <- "hospital"
    names(df)[names(df) == "State"] <- "state"
    
    df
}