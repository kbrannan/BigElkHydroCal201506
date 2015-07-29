getStormPolys <- function(df.flow,df.rises,df.rises.sel) {
  df.ends <- sapply(df.rises.sel$date,getNextRise,df.rises)
  df.pot.strm.bnds <- data.frame(date.bgn=df.rises.sel$date,date.end=do.call("c",df.ends[1,]),
                                 flow.bgn=df.rises.sel$flow)
  df.pot.storms <- do.call(rbind,lapply(seq(1:(length(df.pot.strm.bnds[,1])-1)),getStormFlows,
                          df.flow,df.pot.strm.bnds))
  df.pot.storms$strm.num <- factor(df.pot.storms$strm.num)
  return(df.pot.storms)
}

getStormFlows <- function(lng.strm,df.flow,df.pot.strm.bnds) {
#     tmp.strm <- df.flow[as.Date(df.flow$date) >= df.pot.strm.bnds$date.bgn[lng.strm] 
#                        & as.Date(df.flow$date) <= df.pot.strm.bnds$date.end[lng.strm], ]
     tmp.1 <- df.flow[as.Date(df.flow$date) >= df.pot.strm.bnds$date.bgn[lng.strm] 
                        & as.Date(df.flow$date) <= df.pot.strm.bnds$date.end[lng.strm], ]
     tmp.2 <- tmp.1[tmp.1$flow >= df.pot.strm.bnds$flow.bgn[lng.strm],]
     rw.max <- max(as.numeric(row.names(tmp.1)))
     rw.flow.end <- max(as.numeric(row.names(tmp.2))) + 1
     if(rw.flow.end > rw.max) rw.flow.end <- rw.max
     tmp.date.end <- tmp.1$date[grep(as.character(rw.flow.end),row.names(tmp.1))]
     tmp.strm <- tmp.1[tmp.1$date <= tmp.date.end,]
  df.storm <- data.frame(date=as.Date(tmp.strm$date), flow=tmp.strm$flow, strm.num=lng.strm)
  return(df.storm)  
}
