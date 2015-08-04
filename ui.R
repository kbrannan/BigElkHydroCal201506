# ui.R

if(! exists("df.est")) source(file="GeneralSteps.R")
source(file="plot_me_funk.R")
source(file="getNextRise_funk.R")
source(file="getStormPolys_funk.R")
source(file="getPotentialStormData_funk.R")
source(file="table_me_funk.R")
source(file="getPlotDate_funk.R")
source(file="findStorm_funk.R")

shinyUI(fluidPage(
##  titlePanel("Select Storm"),
  fluidRow(
      helpText("Select the span and what water year"),
      column(6,
      selectInput("yr.b", 
                  label = "Select water year from below",
                  choices = unique(strftime(df.est$date,format="%Y")),
                  selected = unique(strftime(df.est$date,format="%Y"))[1])
      ),
      column(6,
      sliderInput("spn",
                  label = "Span length",
                  min = 3, max = 25, value = 5, step = 2)
      )
    ),
  br(),
  fluidRow(verbatimTextOutput("quick")),
  br(),
  fluidRow(plotOutput("plot", height="600px", click = "plot_click")),
  br(),
  fluidRow(dataTableOutput("table"))
  )
)