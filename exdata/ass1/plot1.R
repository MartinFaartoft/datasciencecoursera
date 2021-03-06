plot1 <- function() {
    df <- read.csv("household_power_consumption.txt", 
                   header = TRUE, 
                   colClasses = c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric"),
                   sep = ";",
                   na.strings = c("?")) #read file

    #combine Date and Time columns into a single POSIXlt column (Datetime)
    df <- within(df, Datetime <- as.POSIXlt(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S")) 
    
    #remove observations from dates outside desired (2007-02-01 and 2007-02-02)
    good_dates <- c("1/2/2007", "2/2/2007")
    
    ix_good <- df$Date %in% good_dates
    
    filtered <- df[ix_good, ]
    
    #prepare graphical device
    png("plot1.png")
    
    #create plot1
    hist(filtered$Global_active_power, 
         col="red", 
         main="Global Active Power", 
         xlab="Global Active Power (kilowatts)")
    
    #close device
    dev.off()
    
}