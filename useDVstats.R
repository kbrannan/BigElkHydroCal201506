## USGS DVstats package
## to install package from GitHub use this command:
## install.packages(c("DVstats"),repos=c("http://usgs-r.github.com","http://cran.us.r-project.org"), dependencies=TRUE)
library(DVstats)

## seperate hydrograph
## using "hysep" because the "part" function is not documented enough to understand the output
bf.est <- hysep(Flow=df.est$mean_daily_flow_cfs,Dates=as.Date(df.est$record_date,format="%m-%d-%Y"),da=88.8,select="sliding",STAID="BigElk")
suro.intflw.est <- data.frame(Dates=bf.est$Dates,Flow=with(bf.est,(Flow-BaseQ)))

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