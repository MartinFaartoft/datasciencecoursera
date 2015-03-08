ass3 <- function() {
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    outcome[, 11] <- as.numeric(outcome[, 11])
    
    ## You may get a warning about NAs being introduced; that is okay
    ## hist(outcome[, 11])
    
    outcome
}

best <- function(state, outcome) {
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
    
    ##sort remaining by col,name ascending
    sorted <- state_hospitals[order(state_hospitals[, col, 2]), ]
    
    ##return name of hospital with lowest mortality rate
    sorted$Hospital.Name[1]
}