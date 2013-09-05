source('phillips.r')
library(shiny)

shinyServer(function(input, output) {
  output$phillipsPlot <- renderPlot({
    phillips(input$yrs[1], input$yrs[2], clusters=input$clusters,
             lag=input$lag)
  })
})