
chr.dir <- paste0(dirname(getwd()),"/data")

chr.files <- list.files(chr.dir, pattern="^DSN.*exp$")
chr.files

##  two precip files
## process first
ii<-1
chr.prec <- scan(file=paste0(chr.dir,"/",chr.files[ii]),what="character",sep="\n")
chr.prec <- chr.prec[(grep("^Date",chr.prec) + 1):length(chr.prec)] 
# ts.prec <- ts(as.numeric(do.call(rbind,lapply(chr.prec,function(x) substr(x,start=15,stop=nchar(x))))), start=c(1995,10),frequency=24)
# tmp.chr.dt <- paste0(format(as.Date(substr(chr.prec,start=1,stop=11)),fmt="%Y-%m-%d")," ",
#                             sprintf(as.numeric(substr(chr.prec,start=12,stop=13)),fmt="%02i"),":00")
#  tmp.dt <- paste0(format(as.Date(substr(chr.prec,start=1,stop=11)),fmt="%Y-%m-%d")," ",
#         sprintf(as.numeric(substr(chr.prec,start=12,stop=13)),fmt="%02i"),
#         ":00")
# tmp.dt <- as.POSIXlt(tmp.dt,format="%Y-%m-%d %H:%M", tz="America/Los_Angeles")
## there is a problem with daylight savings time which causes the hour when 
## clocks are move forward (2 AM) to not exists. This causes the length of the 
## date data to be shorter than the length of the precip data. I am goin to use the
## daily total precipitation. I will get the daily sum by using the date as a factor
# length(tmp.dt)
# length(unique(tmp.dt))
# length(ts.prec)
## daily total precip
tmp.df.precip <- data.frame(date=format(as.Date(substr(chr.prec,start=1,stop=11)),fmt="%Y-%m-%d"),
                        prec11=as.numeric(do.call(rbind,lapply(chr.prec,function(x) substr(x,start=15,stop=nchar(x))))),
                        stringsAsFactors=TRUE)
library(doBy)
tmp.df.daily.precip <- summaryBy(.~date,data=tmp.df.precip,FUN=sum)
## process second
ii<-2
chr.prec <- scan(file=paste0(chr.dir,"/",chr.files[ii]),what="character",sep="\n")
chr.prec <- chr.prec[(grep("^Date",chr.prec) + 1):length(chr.prec)] 


## daily total precip
tmp.df.precip <- data.frame(date=format(as.Date(substr(chr.prec,start=1,stop=11)),fmt="%Y-%m-%d"),
                        prec11=as.numeric(do.call(rbind,lapply(chr.prec,function(x) substr(x,start=15,stop=nchar(x))))),
                        stringsAsFactors=TRUE)
## compare dates to make sure the same for both precip files
if(length(grep("FALSE",as.character(levels(tmp.df.precip$date) == levels(tmp.df.daily.precip$date)))) == 0) paste0("Dates mathch!")
if(length(grep("FALSE",as.character(levels(tmp.df.precip$date) == levels(tmp.df.daily.precip$date)))) != 0) paste0("Dates DON'T mathch!")
## add data if the dates are the same
tmp.df <- summaryBy(.~date,data=tmp.df.precip,FUN=sum)
df.daily.precip <- data.frame(date=as.Date(tmp.df.daily.precip$date),
                              prec11=tmp.df.daily.precip$prec, 
                              prec31=tmp.df$prec)
# clean up
rm(list=ls()[-grep("^df",ls())])
