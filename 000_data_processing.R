#----------#
#--AUTHOR--#
#----------#
# Jason Schatz 
# created: 05/13
# last modified: 10/16


#--------------------#
#--FILE DESCRIPTION--#
#--------------------#
# This script merges raw sensor files, performs meteorological conversions for each year,
# and performs date/time conversions.


#--------------------------------------------------#
#-- install packages (only need to do this once) --#
#--------------------------------------------------#
install.packages(c("plyr", "stringr", "data.table", "tidyverse", "RAtmosphere", "reshape2"))


#----------------------#
#-- import libraries --#
#----------------------#
library(plyr)
library(stringr)
library(data.table)
library(tidyverse)
library(reshape2)


#-----------------------------------#
#-------  1. File merging  ---------#
#-----------------------------------#
## specify season (e.g. F16 for fall 2016; S17 for spring 2017)
season <- "F16"


#----------------------------------------------------------#
#--  the raw datafiles contain temp, RH, and dew point.  --#
#--  this script will pull out each and merge the files. --#
#----------------------------------------------------------#
path1 <- "F:/Users/Jason/Desktop/UHI_data_processing/raw_CSVs"
path2 <- "F:/Users/Jason/Desktop/UHI_data_processing/"		
filenames <- list.files(path = path1, full.names = TRUE, pattern = "*.csv")
import.list <- llply(filenames, read.csv, header = F)

### true sensor names
all_colnames <- read.csv(paste(path2, "!!column_names.csv", sep = ""), header = T)


#-------------------#
#--  Temperature  --#
#-------------------#
data <- lapply(import.list, function(x) x[, -c(1, 4:20)])
data <- lapply(data, function(x) x[-c(1:2), ] )

data <- lapply(seq(data), function(i) {
	y <- data.frame(data[[i]])
	sid1 <- gsub(path1, "", filenames[i]); sid1 = gsub("/", "", sid1); sid1 = gsub(".csv", "", sid1)
	names(y) <- c("Timestamp", sid1)
	return(y)
	})

merge.all <- function(x, y) merge(x, y, all = TRUE, by = "Timestamp")
data <- Reduce(merge.all, data)	
for (i in 2:ncol(data)){
   data[, i] <- as.numeric(as.character(data[, i]))
}
data$Timestamp <- as.character(data$Timestamp)

final <- rbind.fill(all_colnames, data)
write.csv(final, paste(season, "TempC.csv", sep = "_"), row.names = F)


#----------#
#--  RH  --#
#----------#
data <- lapply(import.list, function(x) x[, -c(1, 3, 5:20)])
data <- lapply(data, function(x) x[-c(1:2), ] )

data <- lapply(seq(data), function(i) {
	y <- data.frame(data[[i]])
	sid1 <- gsub(path1, "", filenames[i]); sid1 = gsub("/", "", sid1); sid1 = gsub(".csv", "", sid1)
	names(y) <- c("Timestamp", sid1)
	return(y)
	})

merge.all <- function(x, y) merge(x, y, all = TRUE, by = "Timestamp")
data <- Reduce(merge.all, data)	
for (i in 2:ncol(data)){
   data[, i] <- as.numeric(as.character(data[, i]))
}
data$Timestamp <- as.character(data$Timestamp)

final <- rbind.fill(all_colnames, data)
write.csv(final, paste(season, "RH.csv", sep = "_"), row.names = F)


#----------#
#--  DP  --#
#----------#
data <- lapply(import.list, function(x) x[, -c(1, 3:4, 6:20)])
data <- lapply(data, function(x) x[-c(1:2), ] )

data <- lapply(seq(data), function(i) {
	y <- data.frame(data[[i]])
	sid1 <- gsub(path1, "", filenames[i]); sid1 = gsub("/", "", sid1); sid1 = gsub(".csv", "", sid1)
	names(y) <- c("Timestamp", sid1)
	return(y)
	})

merge.all <- function(x, y) merge(x, y, all = TRUE, by = "Timestamp")
data <- Reduce(merge.all, data)	
for (i in 2:ncol(data)){
   data[, i] <- as.numeric(as.character(data[, i]))
}
data$Timestamp <- as.character(data$Timestamp)

final <- rbind.fill(all_colnames, data)
write.csv(final, paste(season, "DP.csv", sep = "_"), row.names = F)


#--------------------------#
#--  2. Date/time setup  --#
#--------------------------#
T.C <- read_csv(paste(season, "TempC.csv", sep = "_"))
RH  <- read_csv(paste(season, "RH.csv", sep = "_"))
DP  <- read_csv(paste(season, "DP.csv", sep = "_"))

list1  <- list(T.C, RH, DP)
names1 <- c("TempC", "RH", "DP")

for (i in 1:length(list1)){
   DF <- list1[[i]]
   Date <- data.frame(DF$Timestamp)
   colnames(Date) <- c("Date")
   Date$Date <- strptime(Date$Date,"%m/%d/%y %I:%M:%S %p")
   DF$Date <- Date$Date
   dt <- DF$Timestamp
   Date$Time   <- paste(format(Date$Date, format = '%H'), format(Date$Date, format = '%M'))
   Date$T2     <- sapply(strsplit(Date$Time, " "), function(x) {x <- as.numeric(x); x[1] + x[2] / 60})
   Date$D.temp <- format(Date$Date, "%m-%d-%y %H%M")
   Date$doy    <- yday(Date$Date)

   lat <-  43.13
   lon <- -89.33
   Date$now          <- Date$doy+Date$T2/24
   Date$sunrise      <- Date$doy+(sapply(Date$doy,   function(x) suncalc(x, lat, lon, UTC = FALSE)[[1]]) / 24)
   Date$sunset       <- Date$doy+(sapply(Date$doy,   function(x) suncalc(x, lat, lon, UTC = FALSE)[[2]]) / 24)
   Date$prev.sunrise <- Date$doy-1+(sapply(Date$doy, function(x) suncalc(x-1, lat, lon, UTC = FALSE)[[1]]) / 24)
   Date$prev.sunset  <- Date$doy-1+(sapply(Date$doy, function(x) suncalc(x-1, lat, lon, UTC = FALSE)[[2]]) / 24)
   Date$next.sunrise <- Date$doy+1+(sapply(Date$doy, function(x) suncalc(x+1, lat, lon, UTC = FALSE)[[1]]) / 24)
   Date$next.sunset  <- Date$doy+1+(sapply(Date$doy, function(x) suncalc(x+1, lat, lon, UTC = FALSE)[[2]]) / 24)
   Date$H.from.rise  <- ifelse((Date$now - Date$prev.sunrise) * 24 > 24, 
                               (Date$now - Date$prev.sunrise - 1) * 24,
                               (Date$now - Date$prev.sunrise) * 24)
   Date$H.from.set   <- ifelse((Date$now - Date$prev.sunset) * 24 > 24,
                               (Date$now - Date$prev.sunset - 1) * 24,
                               (Date$now - Date$prev.sunset) * 24)
   Date$H.to.rise    <- ifelse((Date$now - Date$next.sunrise) * -24 > 24,
                               (Date$now - Date$next.sunrise + 1) * -24,
                               (Date$now - Date$next.sunrise) * -24)
   Date$H.to.set     <- ifelse((Date$now - Date$next.sunset) * -24 > 24,
                               (Date$now - Date$next.sunset + 1) * -24, (Date$now - Date$next.sunset) * -24)
   Date$mins         <- apply(Date[, 13:16], 1, min)
   Date$mins2        <- apply(Date[, 13:14], 1, min)
   Date$Time2        <- ifelse(Date$mins == Date$H.from.rise, Date$H.from.rise,
       	                ifelse(Date$mins == Date$H.to.rise, -Date$H.to.rise,
	                ifelse(Date$mins == Date$H.from.set, -Date$H.from.set,
	                ifelse(Date$mins == Date$H.to.set, Date$H.to.set, 0))))
   Date$Time2        <- ifelse(Date$Time2 < (-9), 0, Date$Time2)
   Date$Time2        <- ifelse(Date$Time2 > 9, 0, Date$Time2)
   Date$Time3        <- ifelse(Date$mins2 == Date$H.from.rise, Date$H.from.rise,
		        ifelse(Date$mins2 == Date$H.from.set, -Date$H.from.set, 0))
   Date$Date2        <- format(Date$Date, format = "%m/%d/%y")
   Date$Month        <- as.numeric(format(Date$Date, format = "%m"))
   Date$Year         <- as.numeric(format(Date$Date, format = "%y"))
   Date$Month.year   <- Date$Month + Date$Year / 100
   Date$Time1        <-format(Date$Date, format="%H%M")

   DT.DF <- Date[, c(4, 21:24, 25, 19, 20)]
   colnames(DT.DF)[1:2] <- c("Timestamp", "Date")
   final <- cbind(DT.DF, DF)
   final <- final[, -c(9, 161)] 

   ### filter out values that are not on the quarter hour
   x <- c("0000", "0015", "0030", "0045", "0100", "0115", "0130", "0145", "0200", "0215", "0230", "0245", "0300", "0315", "0330",
  	  "0345", "0400", "0415", "0430", "0445", "0500", "0515", "0530", "0545", "0600", "0615", "0630", "0645", "0700", "0715",
	  "0730", "0745", "0800", "0815", "0830", "0845", "0900", "0915", "0930", "0945", "1000", "1015", "1030", "1045", "1100",
	  "1115", "1130", "1145", "1200", "1215", "1230", "1245", "1300", "1315", "1330", "1345", "1400", "1415", "1430", "1445", 
	  "1500", "1515", "1530", "1545", "1600", "1615", "1630", "1645", "1700", "1715", "1730", "1745", "1800", "1815", "1830", 
	  "1845", "1900", "1915", "1930", "1945", "2000", "2015", "2030", "2045", "2100", "2115", "2130", "2145", "2200", "2215", 
	  "2230", "2245", "2300", "2315", "2330", "2345")
   final <- final[final$Time1 %in% x, ]
   write.csv(final, paste(season, "-", names1[i], ".csv", sep = ""), row.names = F)
}
