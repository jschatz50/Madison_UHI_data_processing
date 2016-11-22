#----------#
#--AUTHOR--#
#----------#
# Jason Schatz 
# created: 09/16
# last modified: 10/16


#--------------------#
#--FILE DESCRIPTION--#
#--------------------#
# this script is not beautiful, but it gets the job done.  first, it
# pulls the serial numbers for each bi-annually downloaded file.  Then,
# it calculates the average deviation from mean temperature (across all 
# sensors) at each sensor location.  each time the sensors are swapped
# between locations (every 6 months), a new deviation from the mean is
# calculated.  if the deviation changes by a given threshold for a 
# particular location, that sensor is flagged for further inspection
# to see if particular anomolies follow sensors around (indicating a
# faulty sensor), or if it's random, and everything's gonna be okay.


#----------------------#
#-- import libraries --#
#----------------------#
library(plyr)
library(stringr)
library(data.table)
library(tidyverse)
library(reshape2)


#-----------------------#
#--  extract serials  --#
#-----------------------#
path1      <- "F:/Users/Jason/Desktop/UHI_data_processing/raw_CSVs"   ##set path to folder containing CSVs
filenames1 <- list.files(path=path1, full.names=TRUE)
filenames2 <- list.files(path=path1, full.names=FALSE)

import.list <- llply(filenames1, read.csv, header = F, nrows = 3)

df1 = data.frame(matrix(ncol = 2, nrow = length(filenames1)))
colnames(df1) = c("SID", "SERIAL")

for (i in 1:length(filenames1)){
   tryCatch({
      df <- import.list[[i]]
      df1$SID[i]    <- gsub(".csv", "", filenames2[i])
      df1$SERIAL[i] <- str_sub(df[2, 3], -4, -2)
   }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

df1$SERIAL <- as.numeric(df1$SERIAL)
write.csv(df1, "S16_serials.csv", row.names = F)


#----------------------------#
#--  calculate deviations  --#
#----------------------------#
df1 <- read.csv(file.choose(), header = T)   ##open daily Tmins
a   <- melt(df1, id = c("Date", "Month", "Year", "Month.year"))
a   <- aggregate(value ~ Year + Month + variable, data = a, FUN = 'mean', na.rm = T)
colnames(a) <- c("YEAR", "MONTH", "SID", "temperature")

ser1 <- read.csv("serials_locations.csv", header = T)
ser1$POLEID <- NULL
ser1 <- melt(ser1,id="SID")

key1 <- data.frame(variable <- c("SERIAL_S12.F12", "SERIAL_F12.S13", "SERIAL_S13.F13", "SERIAL_F13.S14",
                                 "SERIAL_S14.F14", "SERIAL_F14.S15", "SERIAL_S15.F15", "SERIAL_F15.S16",
                                 "SERIAL_S16.F16"),
                   YEAR     <- c(2012, 2013, 2013, 2014, 2014, 2015, 2015, 2016, 2016),
		   MONTH    <- c(7, 1, 7, 1, 7, 1, 7, 1, 7))

b <- merge(key1, b, all.x = T)
colnames(b)[5] <- "serial"

c <- merge(b, a, by = c("YEAR", "MONTH", "SID"), all.x = T)
c$variable <- NULL
period.means <- aggregate(temperature ~ YEAR + MONTH, data = c, FUN = 'mean')
colnames(period.means)[3] <- "mean"

c <- merge(c, period.means)
c$DELTA <- c$temperature - c$mean

summer <- subset(c, MONTH == 7)
winter <- subset(c, MONTH == 1)
summer.ser <- dcast(summer, SID ~ YEAR, value.var = "serial")
winter.ser <- dcast(winter, SID ~ YEAR, value.var = "DELTA")

summer <- dcast(summer, SID ~ YEAR, value.var = "DELTA")
colnames(summer) <- c("SID", "y12", "y13", "y14", "y15")
winter <- dcast(winter, SID ~ YEAR, value.var = "DELTA")
colnames(winter) <- c("SID", "y12", "y13", "y14", "y15")

summer$y12_13 <- summer$y12 - summer$y13
summer$y13_14 <- summer$y13 - summer$y14
summer$y14_15 <- summer$y14 - summer$y15

winter$y12_13 <- winter$y12 - winter$y13
winter$y13_14 <- winter$y13 - winter$y14
winter$y14_15 <- winter$y14 - winter$y15


#--------------------------------#
#-- test to see if temperature --# 
#-- relative to mean changes   --#
#-- by a significant amount    --#
#--------------------------------#
flagging_threshold <- 0.5   #set temperature threshold at which to flag values as potentially suspect

which(abs(summer$y14_15) >= flagging_threshold)
which(abs(winter$y14_15) >= flagging_threshold)



