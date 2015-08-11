getPlotDate <- function(dt.bnd,frac) {
  dt.b <- as.Date(dt.bnd[1])
  dt.e <- as.Date(dt.bnd[2])
  theDate <- as.Date(frac * (dt.e - dt.b) + dt.b)
  theStrDate <- format(theDate,"%Y-%m-%d")
  return(theStrDate)
}