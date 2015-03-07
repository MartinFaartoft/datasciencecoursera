plot4 <- function() {
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
    png("plot4.png")
    
    #set parameters for 2x2 subplots
    par(mfrow=c(2,2))
    
    #create plot4
    
    #top left plot (same as plot2)
    plot(filtered$Datetime, filtered$Global_active_power, 
         main="",
         type="l",
         ylab="Global Active Power",
         xlab="")
    
    #top right plot - Voltage over time
    plot(filtered$Datetime, filtered$Voltage,
         main="",
         type="l",
         xlab="datetime",
         ylab="Voltage")
    
    
    #bottom left plot (same as plot3, but with tweaked legend)
    plot(filtered$Datetime, filtered$Sub_metering_1, 
         main="",
         col="black",
         type="l",
         ylab="Energy sub metering",
         xlab="")
    lines(filtered$Datetime, filtered$Sub_metering_2, col="red")
    lines(filtered$Datetime, filtered$Sub_metering_3, col="blue")
    legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
           lty = c(1,1,1),
           col = c("black", "red", "blue"),
           bty = "n")
    
    #bottom right plot - Global reactive power over time
    plot(filtered$Datetime, filtered$Global_reactive_power,
         main="",
         type="l",
         xlab="datetime",
         ylab="Global_reactive_power")
    
    
    #close device
    dev.off()
    
}