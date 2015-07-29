# server.R

library(shiny)
library(Rcpp)

shinyServer(function(input,output) {
    
    
    output$plot <- renderPlot( {
      plot(plot.me(input$spn,input$yr.b,df.est$date,df.est$mean_daily_flow_cfs))
    }
    )
  }
  )
