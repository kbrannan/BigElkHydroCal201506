library(xlsx)

options(stringsAsFactors=FALSE)

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
  print(cur)
  return(chr.names)
}
for(ii in 1:length(chr.reg.eq.name)) df.reg$reg.time <- junk(tmp.rows,chr.reg.eq.name,ii,df.reg$reg.time)
  
df.reg <- df.reg[-tmp.rows[1:(length(tmp.rows)-1)],]
