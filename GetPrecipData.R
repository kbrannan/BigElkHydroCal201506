
chr.dir <- paste0(dirname(getwd()),"/data")

chr.files <- list.files(chr.dir, pattern="^DSN.*exp$")

ii<-1
chr.prec <- scan(file=paste0(chr.dir,"/",chr.files[ii]),what="character",sep="\n")

chr.prec <- chr.prec[(grep("^Date",chr.prec) + 1):length(chr.prec)] 

tmp.prec <- as.numeric(substr(chr.prec,start=15,stop=length(tmp.prec)))

tmp.chr.dt <- paste0(format(as.Date(substr(chr.prec,start=1,stop=11)),fmt="%Y-%m-%d")," ",
                            sprintf(as.numeric(substr(chr.prec,start=12,stop=13)),fmt="%02i"),":00")
tmp.dt <- paste0(format(as.Date(substr(chr.prec,start=1,stop=11)),fmt="%Y-%m-%d")," ",
       sprintf(as.numeric(substr(chr.prec,start=12,stop=13)),fmt="%02i"),
       ":00")
tmp.dt <- as.POSIXlt(tmp.dt,format="%Y-%m-%d %H:%M", tz="America/Los_Angeles")

tmp.dte <- unique(as.Date(substr(chr.prec,start=1,stop=11),fmt="%Y-%m-%d"))
tmp.dt <- as.POSIXlt(tmp.dt,format="%Y-%m-%d %H:%M", tz="America/Los_Angeles")


as.POSIXct("1996-04-07 01:00",format="%Y-%m-%d %H:%M", tz="America/Los_Angeles")
as.POSIXct("1996-04-07 02:00",format="%Y-%m-%d %H:%M", tz="America/Los_Angeles")
as.POSIXct("1996-04-07 03:00",format="%Y-%m-%d %H:%M", tz="America/Los_Angeles")

as.POSIXlt("1996-04-07 02:00",format="%Y-%m-%d %H:%M", tz="America/Los_Angeles")

length(tmp.dt)
length(unique(tmp.dt))

tmp.rows <- grep("TRUE",as.character(is.na(tmp.dt)))
tmp.chr.dt[tmp.rows]

df.prec <- data.frame(datefield=tmp.dt,prec=tmp.prec)
tmp.zoo <- with(df.prec, zoo(prec, order.by=tmp.dt))

tmp.zoo[1:10]

tmp.dt[1:10]

str(as.POSIXct(tmp.dt[1:10],format="%Y/%m/%d %H"))

str(rbind(as.POSIXct(strptime(tmp.dt[1],format="%Y/%m/%d %H")),as.POSIXct(strptime(tmp.dt[2],format="%Y/%m/%d %H"))))

str(tmp.dt)

ts.prec <- ts(as.numeric(do.call(rbind,lapply(chr.prec,function(x) substr(x,start=15,stop=nchar(x))))), start=c(1995,10),frequency=24)
tsp(ts.prec)
str(ts.prec)

head(ts.prec)
