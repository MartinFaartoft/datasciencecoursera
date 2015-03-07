plot3 <- function() {
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
    
    #change to US locale to prevent non-english weekday names
    Sys.setlocale("LC_TIME", "en_US")
    
    #prepare graphical device
    png("plot3.png")
    
    #create plot3
    plot(filtered$Datetime, filtered$Sub_metering_1, 
         main="",
         col="black",
         type="l",
         ylab="Energy sub metering",
         xlab="")
    lines(filtered$Datetime, filtered$Sub_metering_2, col="red")
    lines(filtered$Datetime, filtered$Sub_metering_3, col="blue")
    legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = c(1,1,1), col=c("black", "red", "blue"))
    #close device
    dev.off()
    
}