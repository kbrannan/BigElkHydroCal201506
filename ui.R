# ui.R

shinyUI(fluidPage(
  titlePanel("Select Storm"),
  fluidRow(plotOutput("plot")),
  br(),
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
    )
  )
)