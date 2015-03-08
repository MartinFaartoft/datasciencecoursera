q1 <- function() {
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
    dest <- "idaho_housing_2006.csv"
    
    if(!file.exists(dest)) {
        download.file(url, dest, method="curl")
    }
    
    df <- read.csv(dest)
    
    #codebook: https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf
    #question: how many properties are worth $1.000.000 or more?
    
    #answer: VAL column contains value in different brackets, where 24 is $1.000.000 and more
    sum(df$VAL == 24, na.rm = TRUE)
}

q3 <- function() {
    library(xlsx)
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
    dest <- "natural_gas_acquisition.xslx"
    
    if(!file.exists(dest)) {
        download.file(url, dest, method="curl")
    }
    
    dat <- read.xlsx(dest, 
                     sheetIndex = 1,
                     colIndex = 7:15,
                     rowIndex = 18:23)
    
    #question: What is the value of: sum(dat$Zip*dat$Ext,na.rm=T)
    sum(dat$Zip*dat$Ext,na.rm=T)
}

q4 <- function() {
    library(XML)
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
    dest <- "restaurants.xml"
    
    if(!file.exists(dest)) {
        download.file(url, dest, method="curl")
    }
    
    doc <- xmlTreeParse(dest, useInternal = TRUE)
    
    #question: how many restaurants have the zipcode 21231
    
    #answer:
    zips <- as.integer(xpathSApply(doc, "//zipcode", xmlValue))
    
    sum(zips == 21231)
    
}