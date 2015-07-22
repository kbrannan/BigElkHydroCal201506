## USGS DVstats package
## to install package from GitHub use this command:
## install.packages(c("DVstats"),repos=c("http://usgs-r.github.com","http://cran.us.r-project.org"), dependencies=TRUE)
library(DVstats)
library(ggplot2)

## seperate hydrograph
## using "hysep" because the "part" function is not documented enough to understand the output
bf.est <- hysep(Flow=df.est$mean_daily_flow_cfs,Dates=as.Date(df.est$record_date,format="%m-%d-%Y"),da=88.8,select="sliding",STAID="BigElk")

## plot baseflow and total flow
plot.bf.est <- ggplot() + xlab("") +
  geom_line(data=bf.est,stat="identity",aes(x=Dates,y=Flow,colour="blue")) +
  geom_line(data=bf.est,stat="identity",aes(x=Dates,y=BaseQ,colour="red"))+
  scale_y_log10("Mean Daily Flow (cfs)")
plot(plot.bf.est)

## use R command "peaks to get inflection points
library(smwrBase)
spn <- 5
yr.b <- 1999

tmp.peaks.R <- df.est[peaks(df.est$mean_daily_flow_cfs,span=spn) == TRUE,]
tmp.peaks.R <- cbind(tmp.peaks.R,Dates=as.Date(tmp.peaks.R$record_date,format="%m-%d-%Y"))
tmp.rises.R <- df.est[peaks(-1*df.est$mean_daily_flow_cfs,span=spn) == TRUE,]
tmp.rises.R <- cbind(tmp.rises.R,Dates=as.Date(tmp.rises.R$record_date,format="%m-%d-%Y"))
## plot baseflow and total flow for a water year

dt.b <- as.Date(paste0(yr.b,"/10/01"))
dt.e <- as.Date(paste0(as.numeric(format(dt.b,"%Y")) + 1,"/09/30"))
if(dt.e > max(bf.est$Dates)) dt.e <- max(bf.est$Dates)
df.yr <- bf.est[bf.est$Dates >= dt.b & bf.est$Dates <= dt.e, ]
df.peak <- tmp.peaks.R[tmp.peaks.R$Dates >= dt.b & tmp.peaks.R$Dates <= dt.e, ]
df.rise <- tmp.rises.R[tmp.rises.R$Dates >= dt.b & tmp.rises.R$Dates <= dt.e, ]
plot.peak.rise.yr <- ggplot(data=df.yr) + xlab("") +
  geom_line(stat="identity",aes(x=Dates,y=Flow,colour="blue")) +
  geom_line(stat="identity",aes(x=Dates,y=BaseQ,colour="red"))+
  geom_point(data=df.peak,aes(x=Dates,y=mean_daily_flow_cfs)) +
  geom_point(data=df.rise,aes(x=Dates,y=mean_daily_flow_cfs)) +
  scale_y_log10("Mean Daily Flow (cfs)") + theme(legend.position="none")
plot(plot.peak.rise.yr)


## shiny
library(shiny)
