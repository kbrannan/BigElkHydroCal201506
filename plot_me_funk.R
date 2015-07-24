plot.me <- function(spn,yr.b,dates,flow) {
  require(smwrBase)
  require(ggplot2)
  
  df.tmp <- data.frame(dates=as.Date(dates),flow=flow)
  
  tmp.peaks <- df.tmp[peaks(df.tmp$flow,span=spn) == TRUE,]
  tmp.rises <- df.tmp[peaks(-1*df.tmp$flow,span=spn) == TRUE,]

  head(tmp.rises)
  
  tmp.diff <- diff(tmp.rises$flow,lag=1)
  tmp.rises.sel <- tmp.rises[tmp.diff >= 0,]
  head(tmp.rises.sel)
  
  tmp.sel <- df.tmp[as.numeric(row.names(head(tmp.rises.sel)[1:2,]))[1]:as.numeric(row.names(head(tmp.rises.sel)[1:2,]))[2],]
  
  dt.b <- as.Date(paste0(yr.b,"/10/01"))
  dt.e <- as.Date(paste0(as.numeric(format(dt.b,"%Y")) + 1,"/09/30"))
  
  df.yr <- df.tmp[df.tmp$date >= dt.b & df.tmp$date <= dt.e, ]
  df.peak <- tmp.peaks[tmp.peaks$date >= dt.b & tmp.peaks$date <= dt.e, ]
  df.rise <- tmp.rises[tmp.rises$date >= dt.b & tmp.rises$date <= dt.e, ]
  plot.peak.rise.yr <- ggplot() + xlab("") + ggtitle(paste0("Span = ",spn," days")) +
    geom_line(data=df.yr,stat="identity",aes(x=dates,y=flow, colour="blue")) +
    geom_point(data=df.peak,aes(x=dates,y=flow,colour="red",size=1)) +
    geom_point(data=df.rise,aes(x=dates,y=flow,colour="green",size=1)) +
    scale_y_log10("Mean Daily Flow (cfs)") + theme(legend.position="none")

  return(plot.peak.rise.yr)
}
