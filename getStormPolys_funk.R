
df.tmp <- data.frame(date=as.Date(df.est[,8]),flow=df.est[,3])

tmp.peaks <- df.tmp[peaks(df.tmp$flow,span=spn) == TRUE,]
tmp.rises <- df.tmp[peaks(-1*df.tmp$flow,span=spn) == TRUE,]

tmp.rises.b <- tmp.rises[order(tmp.rises$date,decreasing=TRUE),]

tmp.diff <- diff(tmp.rises$flow,lag=1)
tmp.rises.sel <- tmp.rises[tmp.diff >= 0,]

tmp.diff.b <- diff(tmp.rises.b$flow,lag=1)
tmp.rises.sel.b <- tmp.rises.b[tmp.diff.b >= 0,]






getStormPolys <- function() {
  tmp.dates <- tmp.rises[(tmp.rises$date - tmp.rises.sel$date[1]) > 0,][1,]
  
  tmp.ends <- sapply(tmp.rises.sel$date,getNextRise,tmp.rises)
  
  tmp.rises.sel.end <-data.frame(date.end=do.call("c",tmp.ends[1,]),flow.end=do.call("c",tmp.ends[2,])) 
  
  tmp.pot.strms <- data.frame(date.bgn=tmp.rises.sel$date, flow.bgn=tmp.rises.sel$flow,
                              date.end=do.call("c",tmp.ends[1,]),
                              flow.end=do.call("c",tmp.ends[2,]))
  
}