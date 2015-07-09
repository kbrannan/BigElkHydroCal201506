install.packages(c("DVstats"), 
                 repos=c("http://usgs-r.github.com","http://cran.us.r-project.org"), 
                 dependencies=TRUE)
library(DVstats)

df.hydsep.est <- hysep(Flow=df.est[,3],Dates=as.Date(df.est[,8]),da=88.8,select="sliding")

ls.recess.est <- recess(Flow=df.est[,3],Dates=as.Date(df.est[,8]))

ls.recess.est$Recessions

