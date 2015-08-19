
##y.b<-as.numeric(unique(format(df.est$date,format="%Y")))
##plotToFile(y.b,lst.pot.strm,df.est,df.daily.precip,"strmPlots.pdf")

plotToFile <- function(y.b=NULL,lst.pot.strm=lst.pot.strm,df.est=df.est,df.daily.precip=df.daily.precip,
                       out.file="strmPlots.pdf") {
  df.tmp <- data.frame(dates=as.Date(df.est$date),flow=df.est$mean_daily_flow_cfs)
  df.p <- data.frame(date=df.daily.precip$date,p=df.daily.precip$prec11)
  df.tmp <- data.frame(dates=as.Date(df.est$date),flow=df.est$mean_daily_flow_cfs)
  df.p <- data.frame(date=df.daily.precip$date,p=df.daily.precip$prec11)
  tmp.peaks <- lst.pot.strm$peaks
  tmp.rises <- lst.pot.strm$rises
  tmp.rises.sel <- lst.pot.strm$rises.sel
  tmp.pot.strms <- lst.pot.strm$pot.strm
  pdf(file=out.file,width=11,height=8.5,onefile=TRUE)
  for(ii in 1:(length(y.b)-1)) {
    dt.b <- as.Date(paste0(y.b[ii],"/10/01"))
    dt.e <- as.Date(paste0(as.numeric(format(dt.b,"%Y")) + 1,"/09/30"))
    df.p.yr <- df.p[df.p$date >= dt.b & df.p$date <= dt.e & df.p$p > 0,]
    df.yr <- df.tmp[df.tmp$date >= dt.b & df.tmp$date <= dt.e, ]
    df.yr.ylims <- c(10^(floor(log10(min(df.yr$flow))-1)),10^(ceiling(log10(max(df.yr$flow))+1)))
    df.yr.xlims <- c(dt.b,dt.e)
    df.peak <- tmp.peaks[tmp.peaks$date >= dt.b & tmp.peaks$date <= dt.e, ]
    df.rise <- tmp.rises[tmp.rises$date >= dt.b & tmp.rises$date <= dt.e, ]
    df.pot.strms <- tmp.pot.strms[tmp.pot.strms$date >= dt.b & tmp.pot.strms$date <= dt.e, ]
    par(mfrow=c(2,1), tck=0.01,  mar=c(0,1,0,0.5),oma=c(7,5,7,2))
    layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE), heights=c(1,3),widths=c(1,1))
    plot(x=df.p.yr$date,y=df.p.yr$p,xlab="",pch="",
         xlim=df.yr.xlims,xaxt="n")
    title(xlab="",ylab="",main=paste0("for year ",y.b[ii]), outer=TRUE, line=3)
    lines(x=df.p.yr$date,y=df.p.yr$p,type="h")
    grid(nx=30,ny=NULL)
    plot(x=df.yr$date,y=df.yr$flow, type="l", log="y", lty="blank",
         xlim=df.yr.xlims, ylim=df.yr.ylims)
    for(ii in as.numeric(unique(df.pot.strms$strm.num))) polygon(x=df.pot.strms[df.pot.strms$strm.num == ii,"date"],y=df.pot.strms[df.pot.strms$strm.num == ii,"flow"],
                                                                 col="yellow", lty="blank")
    lines(x=df.yr$date,y=df.yr$flow, type="l",col="blue")
    points(x=df.rise$date,y=df.rise$flow)
    points(x=df.peak$date,y=df.peak$flow)
    grid(nx=30,ny=NULL)    
  }
  dev.off()
}

