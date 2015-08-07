plot.me <- function(spn,yr.b,dates,flow,lst.pot.strm) {
  require(ggplot2)
  
  df.tmp <- data.frame(dates=as.Date(dates),flow=flow)
  
  tmp.peaks <- lst.pot.strm$peaks
  tmp.rises <- lst.pot.strm$rises
  tmp.rises.sel <- lst.pot.strm$rises.sel
  tmp.pot.strms <- lst.pot.strm$pot.strm
 
  dt.b <- as.Date(paste0(yr.b,"/10/01"))
  dt.e <- as.Date(paste0(as.numeric(format(dt.b,"%Y")) + 1,"/09/30"))
  
  df.yr <- df.tmp[df.tmp$date >= dt.b & df.tmp$date <= dt.e, ]
  df.yr.ylims <- c(10^(floor(log10(min(df.yr$flow))-1)),10^(ceiling(log10(max(df.yr$flow))+1)))
  df.yr.xlims <- c(dt.b,dt.e)
  df.peak <- tmp.peaks[tmp.peaks$date >= dt.b & tmp.peaks$date <= dt.e, ]
  df.rise <- tmp.rises[tmp.rises$date >= dt.b & tmp.rises$date <= dt.e, ]
  df.pot.strms <- tmp.pot.strms[tmp.pot.strms$date >= dt.b & tmp.pot.strms$date <= dt.e, ]
  plot.peak.rise.yr <- ggplot(data=df.pot.strms) + xlab("") + ggtitle(paste0("Span = ",spn," days")) +
    geom_polygon(data=df.pot.strms,mapping=aes(x=date,y=flow,group=strm.num,fill="gray")) +
    geom_line(data=df.yr,stat="identity",aes(x=dates,y=flow, colour="blue")) +
    geom_point(data=df.peak,aes(x=dates,y=flow,colour="red",size=1)) +
    geom_point(data=df.rise,aes(x=dates,y=flow,colour="green",size=1)) +
    scale_y_log10("Mean Daily Flow (cfs)",limits=df.yr.ylims) + 
    coord_cartesian(xlim = df.yr.xlims) + 
    theme(legend.position="none")

  return(plot.peak.rise.yr)
}
