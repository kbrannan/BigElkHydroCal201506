
##y.b<-as.numeric(unique(format(df.est$date,format="%Y")))
##plotToFile(y.b,lst.pot.strm,df.est,df.daily.precip,"strmPlots.pdf")

plotIndvToFile <- function(tmp.lst.pot.strm=lst.pot.strm,df.est=df.est,df.daily.precip=df.daily.precip,
                       out.file="strmInvdPlots.pdf") {
  df.tmp <- data.frame(dates=as.Date(df.est$date),flow=df.est$mean_daily_flow_cfs)
  df.p <- data.frame(date=df.daily.precip$date,p=df.daily.precip$prec11)
  tmp.peaks <- tmp.lst.pot.strm$peaks
  tmp.rises <- tmp.lst.pot.strm$rises
  tmp.rises.sel <- tmp.lst.pot.strm$rises.sel
  tmp.pot.strms <- tmp.lst.pot.strm$pot.strm
  strm.nums <- as.numeric(unique(as.character(tmp.pot.strms$strm.num)))
  pdf(file=out.file,width=11,height=8.5,onefile=TRUE)
  for(ii in 1:(length(strm.nums))) {
    x <- tmp.pot.strms[tmp.pot.strms$strm.num == strm.nums[ii], ]
    tmp.ylims <- c(10^(floor(log10(min(x$flow))-1)),10^(ceiling(log10(max(x$flow))+1)))
    tmp.xlims <- c(min(x$date)-1,max(x$date)+1)
    tmp.p <- df.p[df.p$date >= tmp.xlims[1] & df.p$date <= tmp.xlims[2], ]
    
    tmp.f <- df.tmp[df.tmp$date >= tmp.xlims[1] & df.tmp$date <= tmp.xlims[2], ]

    par(mfrow=c(2,1), tck=0.01,  mar=c(0,1,0,0.5),oma=c(7,5,7,2))
    layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE), heights=c(1,3),widths=c(1,1))

    plot(x=tmp.p$date,y=tmp.p$p,xlab="",pch="",xlim=tmp.xlims,
         ylim=c(0,max(c(tmp.p$p,0.1))), xaxt="n")
    title(xlab="",ylab="",main=paste0("storm num ",strm.nums[ii]), outer=TRUE, line=3)
    lines(x=tmp.p$date,y=tmp.p$p,type="h")
    grid(nx=30,ny=NULL)

    plot(x=tmp.f$date,y=tmp.f$flow, type="l", log="y", lty="blank",
         xlim=tmp.xlims, ylim=tmp.ylims, xaxt="n")
    polygon(x=x$date,y=x$flow,col="yellow", lty="blank")
    lines(x=tmp.f$date,y=tmp.f$flow, type="l",col="blue")
    points(x=tmp.rises$date,y=tmp.rises$flow)
    points(x=tmp.peaks$date,y=tmp.peaks$flow)
    grid(nx=30,ny=NULL)    
    axis.Date(side=1,x=tmp.f$date,format="%m-%d-%Y")
  }
  dev.off()
}

