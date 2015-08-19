getNextRise <- function(dt.sel,df.rises){
  df.next <- df.rises[(df.rises$date - dt.sel) > 1,][1,]
  return(df.next)
}    
