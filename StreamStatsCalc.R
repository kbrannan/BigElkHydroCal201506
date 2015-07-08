library(xlsx)

options(stringsAsFactors=FALSE)

## StreamStats Equations
chr.dir <- paste0(dirname(getwd()),"/docs/USGSStreamStats")
chr.file <- "sir20085126_tables.xls"
df.reg <- read.xlsx2(file=paste0(chr.dir,"/",chr.file), sheetName="Table 7",
                    startRow=14,endRow=118,hear=TRUE, colClassess="character",
                    col.index=12)[,1:12]
tmp.rows <- c(as.numeric(row.names(df.reg)[-grep("[0-9aA-zZ]",df.reg[,1])]),(length(df.reg[,1])+1))
chr.reg.eq.name <- df.reg[tmp.rows,3]
df.reg <- cbind(df.reg,reg.time="empty")
junk <- function(lng.rows,chr.name,cur,chr.names) {
  chr.names[lng.rows[cur]:(lng.rows[cur+1]-1)] <- chr.name[cur]
  return(chr.names)
}
for(ii in 1:length(chr.reg.eq.name)) df.reg$reg.time <- junk(tmp.rows,chr.reg.eq.name,ii,df.reg$reg.time)
df.reg <- df.reg[-tmp.rows,]
rm(list=ls()[-grep("df.reg",ls())])

## StreamStats Parameters for Big Elk Creek at outlet from Cadmus
chr.dir <- paste0(dirname(getwd()),"/docs/CadmusHydCal/report")
chr.file <- "Report Data_072312_Figure11_12.xlsx"   ## read.xlsx2 command cannot read from the whole workbook, it runs out of mememory!!! So I extracted the relevant sheet and saved it as a stand-alone workbook
df.pars <- read.xlsx2(file=paste0(chr.dir,"/",chr.file), sheetName="Figure11-12",
                     startRow=597,endRow=605)
rm(list=ls()[-grep("(^df\\.reg$)|(^df\\.pars$)",ls())])

## I need Min January Temp in addition to those that Cadmus used to calculate some of the 7Q# values that Cadmus didn't use
## I am getting that from the shapefile for the WQ station 34453 StreamStats watershed
library(foreign)
chr.dir <- "M:/GIS/BacteriaTMDL/Mult_Basins/StreamStats/FreshwaterBacteriaStations/shapefiles"
chr.file <- "st34453WatershedOR.dbf"
df.st34453.par <- read.dbf(file=paste0(chr.dir,"/",chr.file),as.is=FALSE)
df.pars <- rbind(df.pars,c("Min Jan Temp (JNT) in deg F", df.st34453.par$JANMIN))
rm(list=ls()[-grep("(^df\\.reg$)|(^df\\.pars$)",ls())])
df.pars <- cbind(df.pars,par.names=gsub("ft","E",gsub("^.*\\(","",gsub("\\).*$","",df.pars[,1]))))

### calculate the flow characteristics
junk <- do.call(rbind,strsplit(gsub("(^ )|( $)","",gsub("[^aA-zZ]{1,}"," ", df.reg[1,3])),split=" "))
df.pars[grep(junk[2],df.pars[,3]),2]
paste0(df.reg[5,2],gsub("\\)","\\)\\^", df.reg[5,3]))
gsub("(\\*\\()", "\\)\\*\\(",gsub("^\\*10","\\*10\\^\\(",df.reg[5,3]))
gsub("(\\*\\()", "\\)\\*\\(",gsub("^\\*10","\\*10\\^\\(",gsub("\\)","\\)\\^", df.reg[5,3])))


gsub("(?:\\*10)+","\\^\\(",df.reg[5,3])

ii <- 22
j0 <- df.reg[ii,3]
j.pars <- do.call(rbind,strsplit(gsub("(^ )|( $)","",gsub("[^aA-zZ]{1,}"," ", j0)),split=" "))

lng.rows <- sapply(paste0("^",j.pars,"$"),grep,df.pars[,3])

cur.pars <- df.pars[lng.rows,2:3]
j1 <- gsub("\\)","\\)\\^",j0)
j2 <- gsub("\\*10","\\*10\\^\\(",j1)
j3 <- sub("(\\*\\()","\\)\\*\\(",j2)

j4 <- paste0(df.reg[ii,2],j3)

j5 <- gsub(cur.pars[1,2],cur.pars[1,1],j4)
for(ii in 2:length(cur.pars[,2])) j5 <- gsub(cur.pars[ii,2],cur.pars[ii,1],j5)

j6 <- as.expression(j5)

1.0138825118910877*10^(-0.8519)*(88.9)^1.0184*(79.9)^0.7811*(18.6)^0.4333*(0.36)^-0.0297

gsub("(?:10)","sub",df.reg[5,3])


gsub("\\*\\(?<=","\\*\\)\\(",df.reg[5,3])

