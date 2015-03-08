rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    
    ##convert columns to numeric
    data[, 11] <- as.numeric(data[, 11])
    data[, 17] <- as.numeric(data[, 17])
    data[, 23] <- as.numeric(data[, 23])
    
    ## Check that state and outcome are valid
    valid_states <- unique(data$State)
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    if (!(state %in% valid_states)) {
        stop("invalid state")
    }
    
    if (!(outcome %in% valid_outcomes)) {
        stop("invalid outcome")
    }
    
    ##Find column that matches outcome (11 = heart attack, 17 = heart failure, 23 = pneumonia)
    cols <- c(11, 17, 23)
    ix <- match(outcome, valid_outcomes)
    col <- cols[ix]
    
    ## Return hospital name in that state with lowest 30-day death rate
    
    ##filter out hospitals in other states
    state_hospitals <- data[data$State == state, ]
    
    ##filter out hospitals with no data for outcome
    state_hospitals <- state_hospitals[!is.na(state_hospitals[col]), ]
    
    ##determine index of row to return
    row_count <- nrow(state_hospitals)
    
    row_index <- if(num == "best") 1 else if (num == "worst") row_count else as.integer(num)
    
    if (row_index > row_count) {
        return(NA)
    }
    
    ix <- order(state_hospitals[col], state_hospitals[2])
    
    ##sort remaining by col,name ascending
    sorted <- state_hospitals[ix, ]
    print(head(sorted[, c(7,2,col)]))
    ##return name of hospital with lowest mortality rate
    sorted$Hospital.Name[row_index]
}