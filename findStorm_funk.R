findStorm <- function(dt.strm,z) {
  if(is.Date(as.Date(dt.strm)) == TRUE) {
    b.yr <- as.numeric(strftime(dt.strm,"%Y"))
    x <- table.me(b.yr,z)
    row.strm <- x[x$date.bgn <= dt.strm & x$date.end >= dt.strm, ]
    return(row.strm)    
  }
}