if(length(ls()) > 0) tmp.hold <- paste0("(",ls(),")",collapse="|")

library(xlsx)

options(stringsAsFactors=FALSE)

## StreamStats Equations
chr.dir <- paste0(dirname(getwd()),"/docs/USGSStreamStats")
chr.file <- "sir20085126_tables.xls"
df.reg <- read.xlsx2(file=paste0(chr.dir,"/",chr.file), sheetName="Table 7",
                    startRow=14,endRow=118,hear=TRUE, colClassess="character",
                    col.index=12)[,1:12]
##tmp.rows <- c(as.numeric(row.names(df.reg)[-grep("[0-9aA-zZ]",df.reg[,1])]),(length(df.reg[,1])+1))
tmp.rows <- as.numeric(row.names(df.reg)[-grep("[0-9aA-zZ]",df.reg[,1])])
chr.reg.eq.name <- df.reg[tmp.rows,3]
df.reg <- cbind(df.reg,reg.time="empty")
junk <- function(lng.rows,chr.name,cur,chr.names) {
  if(cur+1 < length(lng.rows)) chr.names[lng.rows[cur]:(lng.rows[cur+1]-1)] <- chr.name[cur]
  if(cur+1 >= length(lng.rows)) chr.names[lng.rows[cur]:(length(chr.names))] <- chr.name[cur]
  return(chr.names)
}
for(ii in 1:length(chr.reg.eq.name)) df.reg$reg.time <- junk(tmp.rows,chr.reg.eq.name,ii,df.reg$reg.time)
df.reg <- df.reg[-tmp.rows,]
## StreamStats Parameters for Big Elk Creek at outlet from Cadmus
chr.dir <- paste0(dirname(getwd()),"/docs/CadmusHydCal/report")
chr.file <- "Report Data_072312_Figure11_12.xlsx"   ## read.xlsx2 command cannot read from the whole workbook, it runs out of mememory!!! So I extracted the relevant sheet and saved it as a stand-alone workbook
df.pars <- read.xlsx2(file=paste0(chr.dir,"/",chr.file), sheetName="Figure11-12",
                     startRow=597,endRow=605)
## I need Min January Temp in addition to those that Cadmus used to calculate some of the 7Q# values that Cadmus didn't use
## I am getting that from the shapefile for the WQ station 34453 StreamStats watershed
library(foreign)
chr.dir <- "M:/GIS/BacteriaTMDL/Mult_Basins/StreamStats/FreshwaterBacteriaStations/shapefiles"
chr.file <- "st34453WatershedOR.dbf"
df.st34453.par <- read.dbf(file=paste0(chr.dir,"/",chr.file),as.is=FALSE)
df.pars <- rbind(df.pars,c("Min Jan Temp (JNT) in deg F", df.st34453.par$JANMIN))
df.pars <- cbind(df.pars,par.names=gsub("ft","E",gsub("^.*\\(","",gsub("\\).*$","",df.pars[,1]))))
### calculate the flow characteristics
## output data.frame
df.values <- data.frame(statistic=gsub("( )|(\\=)","",df.reg[,1]),reg.time=gsub(" ","",df.reg$reg.time),value=as.numeric(NA),UCL=as.numeric(NA),LCL=as.numeric(NA))
##input
##df.reg - data.frame of regression eqaution information
## df.pars - data.frame of all the parameters and repsective values used in the regression equations
for(ii in 1:length(df.values[,1])) {
  ## start calculations
  ## get regression equation
  j0 <- df.reg[ii,3]
  ## get the parameters used in the equation
  j.pars <- do.call(rbind,strsplit(gsub("(^ )|( $)","",gsub("[^aA-zZ]{1,}"," ", j0)),split=" "))
  ## get the rows in the parameter data.frame for the parameters used in the equation
  lng.rows <- sapply(paste0("^",j.pars,"$"),grep,df.pars[,3])
  ## get the paramerters and respcetive values for the parameters used in the equation
  cur.pars <- df.pars[lng.rows,2:3]
  ## modify equation to be evaluated
  j1 <- gsub("\\)","\\)\\^",j0) ## add exponent notation to variables
  j2 <- gsub("\\*10","\\*10\\^\\(",j1) ## add exponent notation to "10" at begining of eqution
  j3 <- sub("(\\*\\()","\\)\\*\\(",j2) ## add closing parenthesis for the exponent of "10"
  j4 <- paste0(df.reg[ii,2],j3) ## adding BCF to begining of the eqaution
  ## put parameter values in the equation
  j5 <- gsub(paste0("\\(",cur.pars[1,2],"\\)"),paste0("(",cur.pars[1,1],")"),j4)
  for(jj in 2:length(cur.pars[,2])) j5 <- gsub(paste0("\\(",cur.pars[jj,2],"\\)"),paste0("(",cur.pars[jj,1],")"),j5)
  ## calculate the eqution
  df.values[ii,"value"] <- eval(parse(text=j5))
  ## calculate error bounds
  if(toupper(df.reg$SEE..Percent[ii]) != "NA") j6 <- df.values[ii,"value"] * (as.numeric(df.reg$SEE..Percent[ii])/100)
  if(toupper(df.reg$SEP..Percent[ii]) != "NA") j6 <- df.values[ii,"value"] * (as.numeric(df.reg$SEP..Percent[ii])/100)
  df.values[ii,"UCL"] <- df.values[ii,"value"] + j6
  df.values[ii,"LCL"] <- df.values[ii,"value"] - j6
  
}
## clean up
if(1*exists("tmp.hold")==0) rm(list=ls()[-grep("(df.reg)|(df.pars)|(df.values)",ls())])
if(1*exists("tmp.hold")==1) rm(list=ls()[-grep(paste0("(df.reg)|(df.pars)|(df.values)|",tmp.hold),ls())])

