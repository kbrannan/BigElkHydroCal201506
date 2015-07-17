## USGS DVstats package
## to install package from GitHub use this command:
## install.packages(c("DVstats"),repos=c("http://usgs-r.github.com","http://cran.us.r-project.org"), dependencies=TRUE)
library(DVstats)
library(ggplot2)

## seperate hydrograph
## using "hysep" because the "part" function is not documented enough to understand the output
bf.est <- hysep(Flow=df.est$mean_daily_flow_cfs,Dates=as.Date(df.est$record_date,format="%m-%d-%Y"),da=88.8,select="sliding",STAID="BigElk")
suro.intflw.est <- data.frame(Dates=bf.est$Dates,Flow=with(bf.est,(Flow-BaseQ)))
summary(suro.intflw.est$Flow)
ecdf.flow.est <- ecdf(df.est$mean_daily_flow_cfs)
ecdf.suro.intflw.est <- ecdf(suro.intflw.est$Flow)

plot(ecdf.suro.intflw.est)

flow.max <- 10^(round(log10(max(df.est$mean_daily_flow_cfs,suro.intflw.est$Flow)))+1)

flow.min <- 10^(round(log10(min(df.est$mean_daily_flow_cfs[df.est$mean_daily_flow_cfs > 0],suro.intflw.est$Flow[suro.intflw.est$Flow > 0])))-1)

x.flow <- c(0,c(1:10 %o%  10^(log10(flow.min):(log10(flow.max)-1))))
y.p <- ecdf.flow.est(x.flow)
y.r <- 1 - y.p

df.ecdf.flow.est <- data.frame(flow=x.flow,p=y.r)

df.fdc.est <- data.frame(flow=df.est$mean_daily_flow_cfs, rank=rank(df.est$mean_daily_flow_cfs, ties.method="average"))
df.fdc.est <- df.fdc.est[ order(df.fdc.est$rank,decreasing=TRUE),]
head(df.fdc.est,10)
df.fdc.est <- data.frame(df.fdc.est,p.greater=100*(df.fdc.est$rank/length(df.fdc.est$rank)))

## plot the digression rates of measured and observed
png(filename = paste0(file="edfflowest.png"), width = 11, height = 8.5,units = "in",res=300,bg = "white")
plot.ecdf.flow.est <- ggplot() + xlab("% Greater than") +
  geom_line(data=df.ecdf.flow.est,stat="identity",aes(x=100*p,y=flow,colour="red"))+
  geom_line(data=df.fdc.est,stat="identity",aes(x=100-p.greater,y=flow,colour="blue")) +
  scale_y_log10("Mean Daily Flow (cfs)")
  

plot(plot.ecdf.flow.est)
dev.off()

y.max <- 10^(round(log10(max(df.data$obs,df.data$mod.max)))+1)
x <- as.Date(df.data$date)

x.min <- as.Date(paste0(format(min(x),"%Y"),"-01-01"))
x.max <- as.Date(paste0(format(max(x),"%Y"),"-12-31"))
df.data <- data.frame(x=x,df.data)
dur.est <- baseDur(x=suro.intflw.est$Flow,Dates=suro.intflw.est$Dates,base=0,test="<=")
dur.est.95per <- baseDur(x=suro.intflw.est$Flow,Dates=suro.intflw.est$Dates,base=ecdf.suro.intflw.est(0.05),test="<=")
dur.est.90per <- baseDur(x=suro.intflw.est$Flow,Dates=suro.intflw.est$Dates,base=ecdf.suro.intflw.est(0.10),test="<=")
dur.est.1 <- baseDur(x=suro.intflw.est$Flow,Dates=suro.intflw.est$Dates,base=1,test="<=")
dur.est.7Q10 <- baseDur(x=suro.intflw.est$Flow,Dates=suro.intflw.est$Dates,base=df.values[df.values$statistic=="7Q10" & df.values$reg.time=="Annual","value"],test="<=")

summary(dur.est$Duration)
summary(dur.est.95per$Duration)
summary(dur.est.90per$Duration)
summary(dur.est.1$Duration)
summary(dur.est.7Q10$Duration)

## freq analysis
frqa.est <- freqAnal(x=df.est$mean_daily_flow_cfs)
plot(frqa.est)
prd.frqa.est <- predict(frqa.est)

library(manipulate)

manipulate(plot(x=df.est$date,y=df.est$mean_daily_flow_cfs, 
                 xlim=c(x.min,x.max),xlab="Date", ylab="Flow"), 
            x.min = slider(as.numeric(min(df.est$date)),as.numeric(max(df.est$date)), label = "Minimum Date"),
            x.max = slider(as.numeric(min(df.est$date)),as.numeric(max(df.est$date)), label = "Maximum Date") )

manipulate(  
{ggplot(df,aes(x=time,y=y))+  
   geom_line()+  
   scale_x_datetime(limits=c(as.POSIXct(x.min,origin = "1970-01-01"),
                             as.POSIXct(x.max,origin = "1970-01-01")))},  
x.min=slider(as.numeric(min(df$time)),as.numeric(max(df$time))),  
x.max=slider(as.numeric(min(df$time))+2,as.numeric(max(df$time)))
)