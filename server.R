# server.R

library(shiny)
library(Rcpp)

shinyServer(function(input,output) {
  
  datasetInput <- reactive(
    {getPotentialStormData(spn=input$spn,
                            dates=df.est$date,
                            flow=df.est$mean_daily_flow_cfs
                            )
    }
    )
    output$plot <- renderPlot( 
      {
#       plot(plot.me(spn=input$spn,
#                    yr.b=input$yr.b,
#                    dates=df.est$date,
#                    flow=df.est$mean_daily_flow_cfs,
#                    lst.pot.strm=getPotentialStormData(spn=input$spn,
#                                                       dates=df.est$date,
#                                                       flow=df.est$mean_daily_flow_cfs)
#                    )
#            )
        spn <- input$spn
        
        lst.pot.strm <-getPotentialStormData(spn=input$spn,dates=df.est$date,flow=df.est$mean_daily_flow_cfs)
        
        yr.b <- input$yr.b
        df.tmp <- data.frame(dates=as.Date(df.est$date),flow=df.est$mean_daily_flow_cfs)
        
        tmp.peaks <- lst.pot.strm$peaks
        tmp.rises <- lst.pot.strm$rises
        tmp.rises.sel <- lst.pot.strm$rises.sel
        tmp.pot.strms <- lst.pot.strm$pot.strm
        
        dt.b <- as.Date(paste0(yr.b,"/10/01"))
        dt.e <- as.Date(paste0(as.numeric(format(dt.b,"%Y")) + 1,"/09/30"))
        
        df.yr <- df.tmp[df.tmp$date >= dt.b & df.tmp$date <= dt.e, ]
        df.yr.ylims <- c(10^(floor(log10(min(df.yr$flow))-1)),10^(ceiling(log10(max(df.yr$flow))+1)))
        df.yr.xlims <- c(dt.b,dt.e)
        df.peak <- tmp.peaks[tmp.peaks$date >= dt.b & tmp.peaks$date <= dt.e, ]
        df.rise <- tmp.rises[tmp.rises$date >= dt.b & tmp.rises$date <= dt.e, ]
        df.pot.strms <- tmp.pot.strms[tmp.pot.strms$date >= dt.b & tmp.pot.strms$date <= dt.e, ]
        
        plot(x=df.yr$date,y=df.yr$flow, type="l", log="y", lty="blank")
        for(ii in as.numeric(unique(df.pot.strms$strm.num))) polygon(x=df.pot.strms[df.pot.strms$strm.num == ii,"date"],y=df.pot.strms[df.pot.strms$strm.num == ii,"flow"],
                col="yellow", lty="blank")
        lines(x=df.yr$date,y=df.yr$flow, type="l",col="blue")
        points(x=df.rise$date,y=df.rise$flow)
        points(x=df.peak$date,y=df.peak$flow)
      }
      )
  output$quick <- renderPrint({
    
    format(as.Date(input$plot_click$x),format="%Y-%m-%d")
    })
#   output$quick <- renderPrint({nearPoints(df.est, input$plot_click, xvar = "date", yvar = "mean_daily_flow_cfs")
#   })
#  )

#output$quick <- renderText(input$plot_click$x)
#output$quick <- renderText(if(is.numeric(input$plot_click$x) == TRUE) findStorm(dt.strm=getPlotDate(input$yr.b,input$plot_click$x),
#                                     z=datasetInput()))
# output$quick <- renderPrint(if(is.numeric(input$plot_click$x) == TRUE) getPlotDate(dt.bnd=format(plot.me(spn=input$spn,
#                                   yr.b=input$yr.b,
#                                   dates=df.est$date,
#                                   flow=df.est$mean_daily_flow_cfs,
#                                   lst.pot.strm=datasetInput()
#                                   )$coordinates$limits, format="%Y-%m-%d"),
#                                   frac=input$plot_click$x)  
#                            )



    output$table <- renderDataTable(table.me(yr.b=input$yr.b,
                                            z=datasetInput()))

}
)
