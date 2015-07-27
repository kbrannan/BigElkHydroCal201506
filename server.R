# server.R

library(shiny)
library(Rcpp)
if(! exists("df.est")) source(file="GeneralSteps.R")
source(file="plot_me_funk.R")

shinyServer(function(input,output) {
    
    
    output$plot <- renderPlot( {
      print(plot.me(input$spn,input$yr.b,df.est$date,df.est$mean_daily_flow_cfs))
    }
    )
  }
  )
