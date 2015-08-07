findStorm <- function(dt.strm,z) {
  if(is.Date(as.Date(dt.strm)) == TRUE) {
    if(is.list(z) == TRUE) {
      b.yr <- as.numeric(strftime(dt.strm,"%Y"))
      x <- table.me(b.yr,z)
      row.strm <- x[x$date.bgn <= dt.strm & x$date.end >= dt.strm, ]
      if(is.Date(row.strm[1,1])) {
        str.info <- paste0(format(row.strm[1,1], format="%Y-%m-%d"), " to ", 
                           format(row.strm[1,2], format="%Y-%m-%d"), " with duration of ",
                           format(row.strm[1,3], format="%02i"), " days, peak flow of ",
                           format(row.strm[1,4], format="%.2f"), " cfs, and total flow of ",
                           format(row.strm[1,5], format="%.1f"), " cu.ft.")
      } else str.info <- " no storm at this date."
    } else str.info <- "z is not a list!"
    return(str.info)    
  }
}