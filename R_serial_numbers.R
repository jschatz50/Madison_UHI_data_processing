### open packages ###
library(plyr)
library(stringr)
library(data.table)
library(tidyverse)
library(reshape2)

#########################
###  extract serials  ###
#########################
path1 = "F:/Users/Jason/Desktop/UHI_data_processing/raw_CSVs"	##set path to folder containing CSVs
filenames = list.files(path=path1, full.names=TRUE)
filenames2 = list.files(path=path1, full.names=FALSE)

import.list = llply(filenames, read.csv, header=F, nrows=3)

df1 = data.frame(matrix(ncol=2, nrow=length(filenames)))
colnames(df1) = c("SID","SERIAL")

for (i in 1:length(filenames)){
   tryCatch({
	df = import.list[[i]]
	df1$SID[i] = gsub(".csv","",filenames2[i])
	df1$SERIAL[i] = str_sub(df[2,3], -4, -2)
   }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

df1$SERIAL = as.numeric(df1$SERIAL)
write.csv(df1, "S16_serials.csv", row.names=F)


##############################
###  calculate deviations  ###
##############################
df1 = read.csv(file.choose(),header=T)	##open daily Tmins
a = melt(df1,id=c("Date","Month","Year","Month.year"))
a = aggregate(value~Year+Month+variable,data=a,FUN='mean',na.rm=T)
colnames(a) = c("YEAR","MONTH","SID","temperature")

ser1 = read.csv("serials_locations.csv",header=T)
ser1$POLEID = NULL
ser1 = melt(ser1,id="SID")

key1 = data.frame(variable = c("SERIAL_S12.F12","SERIAL_F12.S13","SERIAL_S13.F13","SERIAL_F13.S14","SERIAL_S14.F14","SERIAL_F14.S15","SERIAL_S15.F15","SERIAL_F15.S16","SERIAL_S16.F16"),
			YEAR = c(2012,2013,2013,2014,2014,2015,2015,2016,2016),
			MONTH = c(7,1,7,1,7,1,7,1,7))

b = merge(key1,b,all.x=T)
colnames(b)[5] = "serial"

c = merge(b, a, by=c("YEAR","MONTH","SID"), all.x=T)
c$variable = NULL
period.means = aggregate(temperature~YEAR+MONTH, data=c, FUN='mean')
colnames(period.means)[3]="mean"

c = merge(c, period.means)
c$DELTA = c$temperature - c$mean

summer = subset(c, MONTH==7)
winter = subset(c, MONTH==1)
summer.ser = dcast(summer, SID~YEAR, value.var="serial")
winter.ser = dcast(winter, SID~YEAR, value.var="DELTA")


summer = dcast(summer, SID~YEAR, value.var="DELTA")
colnames(summer)=c("SID","y12","y13","y14","y15")

summer$y12_13 = summer$y12 - summer$y13
summer$y13_14 = summer$y13 - summer$y14
summer$y14_15 = summer$y14 - summer$y15

which(abs(summer$y14_15) >= 0.5)	##test to see if temperature relative to mean changes by a significant amount

summer
##14-15:	1   2   7   8  10  11  14  22  38  81  82  86 104 111 125 126

winter = dcast(winter, SID~YEAR, value.var="DELTA")
colnames(winter)=c("SID","y12","y13","y14","y15")

winter$y12_13 = winter$y12 - winter$y13
winter$y13_14 = winter$y13 - winter$y14
winter$y14_15 = winter$y14 - winter$y15

which(abs(winter$y14_15) >= 0.5)	##test to see if temperature relative to mean changes by a significant amount

winter
##14-15:	2   7  20  22  23  24  25  83 129



