#----------#
#--AUTHOR--#
#----------#
# Jason Schatz 
# created: 05/13
# last modified: 10/16


#--------------------#
#--FILE DESCRIPTION--#
#--------------------#
# this script calculates various meterological indices and daily tmax/tmin/tavg
# simply open the specified files, run script, and files will be generated     


#----------------------#
#-- IMPORT LIBRARIES --#
#----------------------#
library(plyr)
library(stringr)
library(data.table)
library(tidyverse)
library(reshape2)


#-----------------------#
#--  open data files  --#
#-----------------------#
## open TempC data from year of interest, and RH/DP from corresponding year
T.C <- read.csv(file.choose(), header = T)
RH  <- read.csv(file.choose(), header = T)	
DP  <- read.csv(file.choose(), header = T)			
for (i in 1:8) {T.C[, i] <- as.factor(as.factor(T.C[, i])) }
for (i in 1:8) {RH[, i] <- as.factor(as.factor(RH[, i])) }
for (i in 1:8) {DP[, i] <- as.factor(as.factor(DP[, i])) }
all_colnames <- read.csv("!!column_names.csv", header = T)   #open column header names
yr <- T.C$Year[1]

#--------------------#
#--  Celsius to F  --#
#--------------------#
T.F <- T.C
for (i in 9:159) {T.F[, i] <- T.F[, i] * (9/5) + 32}
outname <- paste("!", "TempF-20", yr, ".csv", sep = "")
write.csv(T.F, outname, row.names = F)


#----------#
#--  VP  --#
#----------#
## From Campbell, Norman; Environmental Biophysics eqn 3.11
VP <- (0.611 * exp(17.502 * T.C / (T.C + 240.97))) * (RH / 100)
VP <- cbind(T.C[, 1:8], round(VP[9:ncol(T.C)], 3))
outname <- paste("!", "VP-20", yr, ".csv", sep = "")
write.csv(VP, outname, row.names=F)


#--------------------------#
#-- Apparent temperature --#
#--------------------------#
## Steadman's method:  A(°C) = -1.3 + 0.92T.C + 2.2e
AT <- -1.3 + 0.92 * T.C + 2.2 * VP
AT <- cbind(T.C[, 1:8], round(AT[9:ncol(AT)], 3))
outname <- paste("!", "AT-20", yr, ".csv", sep = "")
write.csv(AT, outname, row.names = F)


#------------------#
#--  Heat Index  --#
#------------------#
## From Stull, 2000, Meteorology for Scientists & Engineers
F <- function(T.F, RH)
          (16.923 + ((1.85212E-1) * T.F)
        + (5.37941 * RH)
        - ((1.00254E-1)  * T.F     * RH)
        + ((9.41695E-3)  * T.F ^ 2)
        + ((7.28898E-3)  * RH  ^ 2)
        + ((3.45372E-4)  * T.F ^ 2 * RH)
        - ((8.14971E-4)  * T.F     * RH ^ 2)
        + ((1.02102E-5)  * T.F ^ 2 * RH^2)
        - ((3.8646E-5)   * T.F ^ 3)
        + ((2.91583E-5)  * RH  ^ 3)
        + ((1.42721E-6)  * T.F ^ 3 * RH)
        + ((1.97483E-7)  * T.F     * RH^3)
        - ((2.18429E-8)  * T.F ^ 3 * RH^2)
        + ((8.43296E-10) * T.F ^ 2 * RH^3)
        - ((4.81975E-11) * T.F ^ 3 * RH^3))

HI  <- F(T.F, RH)
T3  <- data.frame(T.F >= 80)
HI2 <- HI * T3
temp1 <- HI2 / T.F
temp1[temp1 == 0] <- 1
HI.final <- temp1 * T.F
HI.final <- cbind(T.F[, 1:8], round(HI.final[9:ncol(T.F)], 3))
outname  <- paste("!", "HI-20", yr, ".csv", sep = "")
write.csv(HI.final, outname)


#-----------#
#--  VPD  --#
#-----------#
## From Campbell, Norman; Environmental Biophysics
VPD <- (0.611 * exp(17.502 * T.C / (T.C + 240.97))) * (1 - RH / 100)
VPD <- cbind(T.C[, 1:8], round(VPD[9:159], 3))
outname <- paste("!", "VPD-20", yr, ".csv", sep = "")
write.csv(VPD, outname, row.names = F)


#------------#
#--  GDDs  --#
#------------#
## using 15-min measurements rather than daily max/min
data2 <- melt(T.C,id=c("Timestamp", "Date", "Month", "Year", "Month.year", "Time1", "Time2", "Time3"), variable_name = "Sensor")
data2$value <- as.numeric(data2$value)

GDD.A <- data2
Tbase <- 10
Tmax  <- 30
GDD.A[, 10] <- ifelse(GDD.A[, 10] < Tbase, Tbase, GDD.A[, 10])
GDD.A[, 10] <- ifelse(GDD.A[, 10] > Tmax, Tmax, GDD.A[, 10])
GDD.A[, 10] <- GDD.A[, 10] - Tbase
GDD.A <- aggregate(value ~ Date + variable, FUN = 'mean', data = GDD.A)
GDD.A <- dcast(GDD.A, Date ~ variable)

outname = paste("!", "GDDs-by-day-20", yr, ".csv", sep = "")
write.csv(GDD.A, outname, row.names = F)


#-----------------#
#--  HDDs/CDDs  --#
#-----------------#
data2 <- melt(T.C, id = c("Timestamp", "Date", "Month", "Year", "Month.year", "Time1", "Time2", "Time3"), variable_name = "Sensor")
data2$value <- as.numeric(data2$value)
Tbase <- 55/3   #equal to 18.33C = 65F

CDD       <- data2
CDD$value <- ifelse(CDD$value > Tbase, CDD$value - Tbase, 0)
CDD       <- aggregate(value ~ Date + variable, FUN = "mean", data = CDD)
CDD       <- dcast(CDD, Date ~ variable)
outname   <- paste("!", "CDDs-by-day-20", yr, ".csv", sep = "")
write.csv(CDD, outname, row.names = F)

HDD       <- data2
HDD$value <- ifelse(HDD$value < Tbase, Tbase - HDD$value, 0)
HDD       <- aggregate(value ~ Date + variable, FUN = "mean", data = HDD)
HDD       <- dcast(HDD, Date ~ variable)
outname   <- paste("!", "HDDs-by-day-20", yr, ".csv", sep = "")
write.csv(HDD, outname, row.names = F)


#--------------------------------------#
#--  Daily min/max/mean temperature  --#
#--------------------------------------#
data2 <- melt(T.C, id = c("Timestamp", "Date", "Month", "Year", "Month.year", "Time1", "Time2", "Time3"))
data2$value <- as.numeric(data2$value)

## eliminate days with partial sets of observations (<90)
len1 <- aggregate(value ~ Date + variable, FUN = "length", data = data2)
len1$value <- ifelse(len1$value >= 90, 1, NA)
len1 <- dcast(len1, Date ~ variable)

## Tavg
result       <- aggregate(value ~ Date + variable, FUN = "mean", data = data2); colnames(result)[1] <- "DATE"
result$value <- round(result$value, 3)
result       <- dcast(result, DATE ~ variable)
final        <- result * len1
final$DATE   <- as.character(len1$Date)
final        <- rbind.fill(all_colnames, final)
outname      <- paste("!", "Tavg-20", yr, ".csv", sep = "")
write.csv(final, outname, row.names = F)

## Tmin
result     <- aggregate(value ~ Date + variable, FUN = "min", data = data2); colnames(result)[1] <- "DATE"
result     <- dcast(result, DATE ~ variable)
final      <- result * len1
final$DATE <- as.character(len1$Date)
final      <- rbind.fill(all_colnames,final)
outname    <- paste("!", "Tmin-20", yr, ".csv", sep = "")
write.csv(final, outname, row.names = F)

## Tmax
result     <- aggregate(value ~ Date + variable, FUN = "max", data = data2); colnames(result)[1] <- "DATE"
result     <- dcast(result, DATE ~ variable)
final      <- result * len1
final$DATE <- as.character(len1$Date)
final      <- rbind.fill(all_colnames, final)
outname    <- paste("!", "Tmax-20", yr, ".csv", sep = "")
write.csv(final, outname, row.names = F)
