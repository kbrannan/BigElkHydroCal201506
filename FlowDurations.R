

fdc <- function(x) {
  y <- x[order(x,decreasing=TRUE)]
  r <- rank(y,ties.method="average")
  pe <- 100*(1 - r/length(y))
  df.fdc <- data.frame(flow=y,per.exc=pe,rank=r)  
  return(df.fdc)
}

plot.fdc <- function(df.fdc) {
  require(ggplot2)
  require(scales)
  ylims <- c(10^floor(log10(min(df.fdc$flow))),
             10^ceiling(log10(max(df.fdc$flow))))
  ybreaks <- 10^(log10(ylims[1]):log10(ylims[2]))
  p <- ggplot(data=df.fdc) + 
    geom_line(aes(x=per.exc,y=flow)) + 
    scale_y_continuous(trans="log10", breaks=ybreaks, limits=ylims, labels=comma) +
    scale_x_continuous(limits=c(0,100)) +
    xlab("Percent Exceedance") + ylab("Flow (cfs)")
  return(p)
}




df.fdc.obs <- fdc(df.obs[,3])

plot(plot.fdc(df.fdc.obs))

df.fdc.est <- fdc(df.est[,3])

plot(plot.fdc(df.fdc.est))
