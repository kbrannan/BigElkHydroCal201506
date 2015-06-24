
library(ggplot2)
library(scales)

fdc <- function(x) {
  y <- x[order(x,decreasing=TRUE)]
  r <- rank(y,ties.method="average")
  pe <- 100*(1 - r/length(y))
  df.fdc <- data.frame(flow=y,per.exc=pe,rank=r)  
  return(df.fdc)
}

df.fdc.est <- fdc(df.est[,3])

ylims <- c(10^floor(log10(min(df.fdc.est$flow))),10^ceiling(log10(max(df.fdc.est$flow))))

ybreaks <- 10^(log10(ylims[1]):log10(ylims[2]))




p.fdc.est <- ggplot(data=df.fdc.est) + 
  geom_line(aes(x=per.exc,y=flow)) + 
  scale_y_continuous(trans="log10", breaks=ybreaks, limits=ylims, labels=comma) +
  scale_x_continuous(limits=c(0,100)) +
  xlab("Percent Exceedance") + ylab("Flow (cfs)")

plot(p.fdc.est)
