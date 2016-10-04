#################################
#################################
###      Vertical merging     ###
#################################
#################################
## this combines new data with existing data files e.g. if you have data for the first half of 2015, 
## this will add the second half of 2015.  so for each data type (TempC, RH, DP), open the previously 
## existing file, the new data, and merge

install.packages(c("foreach","doMC")

### open packages ###
library(plyr)
library(stringr)
library(data.table)
library(tidyverse)
library(RAtmosphere)
library(reshape2)
library(foreach)
library(doMC)

all_colnames2 = read.csv("!!column_names2.csv",header=T)

### previously existing file (partial year) ###
TC.old = read_csv(file.choose())
RH.old = read_csv(file.choose())
DP.old = read_csv(file.choose())
list1=list(TC.old,RH.old,DP.old)

### new data (e.g. collected in Fall 2016) ###
TC  = read_csv(paste(season,"TempC.csv",sep="-"))
RH  = read_csv(paste(season,"RH.csv",sep="-"))
DP  = read_csv(paste(season,"DP.csv",sep="-"))
list2=list(TC,RH,DP)

names1 = c("TempC","RH","DP")

registerDoMC(3)	#for parallelizing; specify number of cores
foreach(i=1:length(list1)) %dopar% {
	d1 = data.frame(list1[[i]])
	d2 = data.frame(list2[[i]])
	d1[,9:ncol(d1)] = apply(d1[,9:ncol(d1)], 2, function(x) as.numeric(x))
	d2[,9:ncol(d2)] = apply(d2[,9:ncol(d1)], 2, function(x) as.numeric(x))
	d1 = d1[rowSums(is.na(d1[,9:ncol(d1)]))!=length(9:ncol(d1)), ]
	d2 = d2[rowSums(is.na(d2[,9:ncol(d2)))!=length(9:ncol(d2)), ]

	d1=melt(d1, na.rm=TRUE, id=c("Timestamp","Date","Month","Year","Month.year","Time1","Time2","Time3"))
	d2=melt(d2, na.rm=TRUE, id=c("Timestamp","Date","Month","Year","Month.year","Time1","Time2","Time3"))
	d2$Time1 = as.integer(d2$Time1)
	d1$value = as.numeric(d1$value)
	d2$value = as.numeric(d2$value)
	d1$variable = as.factor(d1$variable)
	d2$variable = as.factor(d2$variable)

	d1DT=setDT(d1,key=c("Timestamp","Date","Month","Year","Month.year","Time1","Time2","Time3","variable"))
	d2DT=setDT(d2,key=c("Timestamp","Date","Month","Year","Month.year","Time1","Time2","Time3","variable"))
	outDT=merge(d1DT,d2DT,by=c("Timestamp","Date","Month","Year","Month.year","Time1","Time2","Time3","variable"),all=T)
	outDT[is.na(value.x), value.x:=value.y]
	outDT$value.y = NULL

	out = dcast(outDT, Timestamp+Date+Month+Year+Month.year+Time1+Time2+Time3 ~ variable, value.var="value.x", fun.aggregate = mean, na.rm = TRUE)
	setDT(out)
	DF_new <- out[, names(out)[9:ncol(out)] := lapply(.SD, max, na.rm=TRUE), by=list(Timestamp), .SDcols=9:ncol(out)][,unique(.SD)]
	DF_new = rbind.fill(all_colnames2,DF_new)
	is.na(DF_new) <- sapply(DF_new, is.infinite)
	yrs = levels(factor(DF_new$Year))
	
	for (f in 1:2){
		yr = as.numeric(yrs[f])
		data = subset(DF_new, Year==yr)
		data=data[!duplicated(data[,c('Timestamp')]),]
		outname = paste("!",names1[i],"-20",yr,".csv",sep="")
		write.csv(data,outname,row.names=F)
	}
}

