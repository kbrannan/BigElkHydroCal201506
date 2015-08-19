source(file="plotPotStormByYear.R")

## set inital pars
spn <- 5 ## span of 5 days to find mins and maxs
dt.max.p <- max(df.daily.precip$date)
df.est.clp <- df.est[as.Date(df.est$date) <= dt.max.p,]

## get boundaries of potential storms, see function for rules to get boudaries
lst.pot.strm <- getPotentialStormData(spn=spn,dates=df.est.clp$date,flow=df.est.clp$mean_daily_flow_cfs)

## get plots of the potential storms
plotToFile(as.numeric(unique(format(df.est.clp$date,format="%Y"))),
           lst.pot.strm,df.est.clp,df.daily.precip,"strmPlots.pdf")

## get storm summary table
df.strm.sum <- table.me(yr.b=NULL,z=lst.pot.strm)

## only use storms with duration greater than 5 days, this lower limit from HSPFEXP Guidence
lst.pot.strm.5 <- lst.pot.strm
df.strm.sum.5 <- lst.pot.strm$pot.strm[df.strm.sum$length.days >= 5,]
lst.pot.strm.5$pot.strm <- lst.pot.strm$pot.strm[lst.pot.strm$pot.strm$strm.num %in% df.strm.sum.5$strm.num, ]
plotToFile(as.numeric(unique(format(df.est.clp$date,format="%Y"))),
           lst.pot.strm.5,df.est.clp,df.daily.precip,"strmPlots5day.pdf")

## check for multuiple peaks with storms and exclude sorms with multiple peaks 
countPeak <- function(flow) sum(peaks(flow,span=3)*1)
df.peak.counts <- summaryBy(flow ~ strm.num,data=lst.pot.strm.5$pot.strm, FUN=countPeak)
keep.strm.nums <- as.numeric(df.peak.counts[df.peak.counts$flow.countPeak == 1,"strm.num"])
df.pot.strm.5.single <- lst.pot.strm.5$pot.strm[lst.pot.strm.5$pot.strm$strm.num %in% keep.strm.nums, ]
lst.pot.strm.5.single <- lst.pot.strm.5
lst.pot.strm.5.single$pot.strm <- df.pot.strm.5.single
plotToFile(as.numeric(unique(format(df.est.clp$date,format="%Y"))),lst.pot.strm.5.single,df.est.clp,df.daily.precip,"strmPlots5Single.pdf")

## only use storms with precip > 0
df.pot.00 <- merge(x=lst.pot.strm.5.single$pot.strm,y=df.daily.precip, by="date")
df.pot.01 <- data.frame(df.pot.00,p=pmax(df.pot.00$prec11,df.pot.00$prec31))
df.pot.01.sum <- summaryBy(p ~ strm.num,data=df.pot.01, FUN=sum)
keep.strm.nums <- as.numeric(df.pot.01.sum$strm.num[df.pot.01.sum$p.sum > 0])
lst.pot.strm.5.single.gt0 <- lst.pot.strm.5.single
lst.pot.strm.5.single.gt0$pot.strm <- lst.pot.strm.5.single$pot.strm[as.numeric(lst.pot.strm.5.single$pot.strm$strm.num) %in% keep.strm.nums, ]
plotToFile(as.numeric(unique(format(df.est.clp$date,format="%Y"))),lst.pot.strm.5.single.gt0,df.est.clp,df.daily.precip,"strmPlots5Singlegt0.pdf")
rm(list=ls(pattern="df.pot"))
## check max precip timing relative to peak flow
plotIndvToFile(tmp.lst.pot.strm=lst.pot.strm.5.single.gt0,df.est=df.est.clp,df.daily.precip=df.daily.precip,
                           out.file="strmInvdPlots5singlegt0.pdf")
df.pot.00 <- merge(x=lst.pot.strm.5.single.gt0$pot.strm,y=df.daily.precip, by="date")
df.pot.01 <- data.frame(df.pot.00,p=pmax(df.pot.00$prec11,df.pot.00$prec31))
dtpeak <- function(x.date,x.val) x.date[x.val == max(x.val)]
df.pot.00.p.max <- summaryBy(p ~ strm.num,data=df.pot.01, FUN=max)
by(df.pot.01,df.pot.01$strm.num,max)
df.pot.00.f.max <- summaryBy(flow ~ strm.num,data=df.pot.01, FUN=max)
df.pot.01$date[df.pot.01$flow %in% ]
junk <- data.frame(strm.num=unique(df.pot.00$strm.num),date.p=as.Date("1967-07-02"),date.f=as.Date("1967-07-02"),diff=0)
for( ii in junk$strm.num) {
  tmp.d <- df.pot.01[df.pot.01$strm.num == ii,]
  junk$date.p[junk$strm.num == ii] <- tmp.d$date[tmp.d$p == max(tmp.d$p)]
  junk$date.f[junk$strm.num == ii] <- tmp.d$date[tmp.d$flow == max(tmp.d$flow)]
  junk$diff[junk$strm.num == ii] <- tmp.d$date[tmp.d$p == max(tmp.d$p)] - tmp.d$date[tmp.d$flow == max(tmp.d$flow)]
}
keep.strm.nums <- as.numeric(junk$strm.num[junk$diff <= 0])
df.pot.02 <- df.pot.01[df.pot.01$strm.num %in% keep.strm.nums, ]
lst.pot.strm.5.single.gt0.fafp <- lst.pot.strm.5.single.gt0
lst.pot.strm.5.single.gt0.fafp$pot.strm <- df.pot.02
plotToFile(as.numeric(unique(format(df.est.clp$date,format="%Y"))),lst.pot.strm.5.single.gt0.fafp,df.est.clp,df.daily.precip,"strmPlots5Singlegt0fafp.pdf")
plotIndvToFile(tmp.lst.pot.strm=lst.pot.strm.5.single.gt0.fafp,df.est=df.est.clp,df.daily.precip=df.daily.precip,
               out.file="strmInvdPlots5singlegt0fafp.pdf")
## check storms by seasons
## seasons as defined by HSPEXP
## Summer Jun-Aug
## Winter Dec-Feb
## remaing seasons defined by me
## Fall Sep-Nov
## Spring Mar-May
## I am excluding storms that occur accross the boundaries of the seasons for this part of 
## the investigation, but I will include these storms in the selection process later
##
##
## monthly information
rm(df.strm.sum)
df.strm.sum <- table.me(yr.b=NULL,z=lst.pot.strm.5.single.gt0.fafp)
df.strm.months <- data.frame(strm.num=df.strm.sum$strm.num,month.bgn=format(df.strm.sum$date.bgn,format="%b"),month.end=format(df.strm.sum$date.end,format="%b"))
## summer strorms (Months- Jun, Jul, Aug)
df.months.00 <- df.strm.months[df.strm.months$month.bgn %in% c("Jun","Jul","Aug"),]
df.months.01 <- df.months.00[df.months.00$month.end %in% c("Jun","Jul","Aug"),]
lst.pot.strm.sum <- lst.pot.strm.5.single.gt0.fafp
lst.pot.strm.sum$pot.strm <- lst.pot.strm.5.single.gt0.fafp$pot.strm[lst.pot.strm.5.single.gt0.fafp$pot.strm$strm.num %in% df.months.01$strm.num,]
plotToFile(as.numeric(unique(format(df.est.clp$date,format="%Y"))),lst.pot.strm.sum,df.est.clp,df.daily.precip,"strmPlotsSummer.pdf")
plotIndvToFile(tmp.lst.pot.strm=lst.pot.strm.sum,df.est=df.est.clp,df.daily.precip=df.daily.precip,
               out.file="strmInvdPlotsSummer.pdf")
rm(df.months.00,df.months.01)
## fall strorms (Months- Sep, Oct, Nov)
df.months.00 <- df.strm.months[df.strm.months$month.bgn %in% c("Sep","Oct","Nov"),]
df.months.01 <- df.months.00[df.months.00$month.end %in% c("Sep","Oct","Nov"),]
lst.pot.strm.fal <- lst.pot.strm.5.single.gt0.fafp
lst.pot.strm.fal$pot.strm <- lst.pot.strm.5.single.gt0.fafp$pot.strm[lst.pot.strm.5.single.gt0.fafp$pot.strm$strm.num %in% df.months.01$strm.num,]
plotToFile(as.numeric(unique(format(df.est.clp$date,format="%Y"))),lst.pot.strm.fal,df.est.clp,df.daily.precip,"strmPlotsFall.pdf")
plotIndvToFile(tmp.lst.pot.strm=lst.pot.strm.fal,df.est=df.est.clp,df.daily.precip=df.daily.precip,
               out.file="strmInvdPlotsFall.pdf")
rm(df.months.00,df.months.01)
