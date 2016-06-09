library(shiny)

shinyUI(fluidPage(
        headerPanel("MLB Batting record_example plot"), 
        titlePanel("test"),
        sidebarPanel(
                h4("Filter"), 
                sliderInput("year", "Year",
                            min = 1871, max = 2014, value = c(1990, 2014)),
                sliderInput("age", "Age",
                            min = 15, max = 60, value = c(25, 40))
                
        ),
        
    mainPanel(
                h4("Batting average vs. Times At bat"),
                plotOutput("newplot")
                #plotOutput("plot2")
                
                )
))
