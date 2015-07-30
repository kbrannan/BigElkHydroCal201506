table.me <- function(yr.b,z) {
  require(doBy)
  y<-z$pot.strm
  dt.b <- as.Date(paste0(yr.b,"/10/01"))
  dt.e <- as.Date(paste0(as.numeric(format(dt.b,"%Y")) + 1,"/09/30"))
  x <- y[y$date >= dt.b & y$date <= dt.e, ]
  df.table <- data.frame(date.bgn=x[firstobs(~strm.num,x),"date"],date.end=x[lastobs(~strm.num,x),"date"])
  df.table <- data.frame(df.table[,1:2],length.days=as.numeric(df.table[,2]-df.table[,1]))
  df.table <- data.frame(df.table,peak=summaryBy(flow ~ strm.num,x,FUN=max)[,2],sum.cuft=summaryBy(flow ~ strm.num,x,FUN=sum)[,2]*(3600*24)*df.table[,3])
  return(df.table)
}
