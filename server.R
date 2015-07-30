# server.R

library(shiny)
library(Rcpp)

shinyServer(function(input,output) {
  
  datasetInput <- reactive(
    {
    getPotentialStormData(spn=input$spn,
                          dates=df.est$date,
                          flow=df.est$mean_daily_flow_cfs
                          )
    }
  )
    output$plot <- renderPlot( 
      {
      plot(
        plot.me(spn=input$spn,
                yr.b=input$yr.b,
                dates=df.est$date,
                flow=df.est$mean_daily_flow_cfs,
                lst.pot.strm=datasetInput()
                )
        )
      }
      )
  output$table <- renderDataTable(table.me(yr.b=input$yr.b
                                           ,z=datasetInput()))
}
)
