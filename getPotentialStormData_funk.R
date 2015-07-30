getPotentialStormData <- function(spn,dates,flow) {
  require(smwrBase)
  source(file="getNextRise_funk.R")
  
  df.tmp <- data.frame(dates=as.Date(dates),flow=flow)
  
  df.peaks <- df.tmp[peaks(df.tmp$flow,span=spn) == TRUE,]
  df.rises <- df.tmp[peaks(-1*df.tmp$flow,span=spn) == TRUE,]
  
  tmp.diff <- diff(df.rises$flow,lag=1)
  df.rises.sel <- df.rises[tmp.diff <= 0,]
  
  df.pot.strms <- getStormPolys(df.tmp,df.rises,df.rises.sel)
  lst.pot.strm <- list(peaks=df.peaks,rises=df.rises,rises.sel=df.rises.sel,pot.strm=df.pot.strms)
}