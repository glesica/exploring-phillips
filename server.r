shinyServer(function(input, output) {
  customDataset <- reactive({
    load.phillips(input$yrs[1], input$yrs[2],
                  clusters=input$clusters, df=full.df)
  })
  output$phillipsPlot <- renderPlot({
    plot.phillips(customDataset(), lag=input$lag, labs=input$labs)
  })
  output$inflationHist <- renderPlot({
    data <- customDataset()
    hist(data$Inflation, xlab="Inflation Rate",
         main="Histogram of Inflation Rate")
  })
  output$unemploymentHist <- renderPlot({
    data <- customDataset()
    hist(data$Unemployment, xlab="Unemployment Rate",
         main="Histogram of Unemployment Rate")
  })
  output$downloadData <- downloadHandler(
    filename="exploring_phillips.csv",
    content=function(file) { write.csv(customDataset(), file) }
  )
})