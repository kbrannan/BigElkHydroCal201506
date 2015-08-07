getPlotDate <- function(yr.b,frac) {
  dt.b <- as.Date(paste0(yr.b,"/10/01"))
  dt.e <- as.Date(paste0(as.numeric(format(dt.b,"%Y")) + 1,"/09/30"))
  theDate <- as.Date(frac * (dt.e - dt.b) + dt.b)
  theStrDate <- format(theDate,"%Y-%m-%d")
  return(theStrDate)
}