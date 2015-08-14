# server.R

require(shiny)
require(Rcpp)
require(DT)


shinyServer(function(input,output) {
  
  datasetInput <- reactive(
    {getPotentialStormData(spn=input$spn,
                            dates=df.est$date,
                            flow=df.est$mean_daily_flow_cfs
                            )
    }
    )
  output$plot_p <- renderPlot( {
    yr.b <- input$yr.b
    df.p <- data.frame(date=df.daily.precip$date,p=df.daily.precip$prec11)
    
  }
    )
    output$plot <- renderPlot( 
      {
        spn <- input$spn
        lst.pot.strm <-getPotentialStormData(spn=input$spn,dates=df.est$date,flow=df.est$mean_daily_flow_cfs)
        yr.b <- input$yr.b
        df.tmp <- data.frame(dates=as.Date(df.est$date),flow=df.est$mean_daily_flow_cfs)
        df.p <- data.frame(date=df.daily.precip$date,p=df.daily.precip$prec11)
        df.tmp <- data.frame(dates=as.Date(df.est$date),flow=df.est$mean_daily_flow_cfs)
        df.p <- data.frame(date=df.daily.precip$date,p=df.daily.precip$prec11)
        tmp.peaks <- lst.pot.strm$peaks
        tmp.rises <- lst.pot.strm$rises
        tmp.rises.sel <- lst.pot.strm$rises.sel
        tmp.pot.strms <- lst.pot.strm$pot.strm
        dt.b <- as.Date(paste0(yr.b,"/10/01"))
        dt.e <- as.Date(paste0(as.numeric(format(dt.b,"%Y")) + 1,"/09/30"))
        df.p.yr <- df.p[df.p$date >= dt.b & df.p$date <= dt.e & df.p$p > 0,]
        df.yr <- df.tmp[df.tmp$date >= dt.b & df.tmp$date <= dt.e, ]
        df.yr.ylims <- c(10^(floor(log10(min(df.yr$flow))-1)),10^(ceiling(log10(max(df.yr$flow))+1)))
        df.yr.xlims <- c(dt.b,dt.e)
        df.peak <- tmp.peaks[tmp.peaks$date >= dt.b & tmp.peaks$date <= dt.e, ]
        df.rise <- tmp.rises[tmp.rises$date >= dt.b & tmp.rises$date <= dt.e, ]
        df.pot.strms <- tmp.pot.strms[tmp.pot.strms$date >= dt.b & tmp.pot.strms$date <= dt.e, ]
         par(mfrow=c(2,1),oma=c(5,4,0,0),mar=c(0,0,0,1))
         layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE), heights=c(1,2),widths=c(1,1))
         plot(x=df.p.yr$date,y=df.p.yr$p,xaxt="n",xlab="",pch="",
              xlim=df.yr.xlims)
         lines(x=df.p.yr$date,y=df.p.yr$p,type="h")
        plot(x=df.yr$date,y=df.yr$flow, type="l", log="y", lty="blank",
             xlim=df.yr.xlims, ylim=df.yr.ylims)
        for(ii in as.numeric(unique(df.pot.strms$strm.num))) polygon(x=df.pot.strms[df.pot.strms$strm.num == ii,"date"],y=df.pot.strms[df.pot.strms$strm.num == ii,"flow"],
                                                                     col="yellow", lty="blank")
        lines(x=df.yr$date,y=df.yr$flow, type="l",col="blue")
        points(x=df.rise$date,y=df.rise$flow)
        points(x=df.peak$date,y=df.peak$flow)
      }
      )
  output$quick <- renderPrint({
    if(length(input$plot_click$x) != 0) {
       findStorm(dt.strm=format(as.Date(floor(input$plot_click$x)),format="%Y-%m-%d"),
                 x=table.me(yr.b=input$yr.b,z=datasetInput()))
    }
    })
    output$table <- DT::renderDataTable(table.me(yr.b=input$yr.b,
                                            z=datasetInput()))

}
)
