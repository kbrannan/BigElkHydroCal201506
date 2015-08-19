table.me <- function(yr.b,z) {
  require(doBy)
  y<-z$pot.strm
  if(is.null(yr.b) != TRUE) {
    dt.b <- as.Date(paste0(yr.b,"/10/01"))
    dt.e <- as.Date(paste0(as.numeric(format(dt.b,"%Y")) + 1,"/09/30"))
    x <- y[y$date >= dt.b & y$date <= dt.e, ]
  } else x <- y
  df.table <- data.frame(strm.num=summaryBy(strm.num ~ strm.num,x,FUN=max)[,2],date.bgn=x[firstobs(~strm.num,x),"date"],date.end=x[lastobs(~strm.num,x),"date"])
  df.table <- data.frame(df.table,length.days=as.numeric(df.table$date.end-df.table$date.bgn))
  df.table <- data.frame(df.table,peak=summaryBy(flow ~ strm.num,x,FUN=max)[,2],sum.cuft=summaryBy(flow ~ strm.num,x,FUN=sum)[,2]*(3600*24)*df.table$length.days)
  return(df.table)
}
