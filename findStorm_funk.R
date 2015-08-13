findStorm <- function(dt.strm,x) {
  if(is.Date(as.Date(dt.strm)) == TRUE) {
      row.strm <- x[x$date.bgn <= dt.strm & x$date.end >= dt.strm, ]
      if(length(row.strm[,1] > 0)) {
        str.info <- paste0(format(row.strm[1,1], format="%Y-%m-%d"), " to ", 
                           format(row.strm[1,2], format="%Y-%m-%d"), " with duration of ",
                           format(row.strm[1,3], format="%02i"), " days, peak flow of ",
                           format(row.strm[1,4], format="%.2f"), " cfs, and total flow of ",
                           format(row.strm[1,5], format="%.1f"), " cu.ft.")
      } else str.info <- " no storm at this date."
    return(str.info)    
  }
}