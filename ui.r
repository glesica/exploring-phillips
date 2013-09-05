source('phillips.r')
library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Exploring the Phillips Curve"),
  sidebarPanel(
    h4("Parameters"),
    sliderInput("yrs", "Years:", min=1948,
                max=2012, value=c(1948, 2012),
                format="####"),
    sliderInput("clusters", "Number of clusters:", min=1,
                max=10, value=1,
                format="##"),
    sliderInput("lag", "Time lag (months):", min=-36, max=36, value=12,
                format="##"),
    h4("Help"),
    helpText("Set the parameters above to update the plot and trend line.",
             "Lags are specified by the number of months between the",
             "unemployment and inflation observations used. So, for",
             "example, a value of -12 means unemployment at month t",
             "will be paired with inflation at month t-12."),
    h4("About"),
    helpText("Note that the overall data do not exhibit a",
             "downward-sloping trend as would be expected based on the",
             "theory behind the Phillips Curve. However, several contiguous",
             "time periods do exhibit such a trend, suggesting that the",
             "curve may have shifted over time, possibly due to change",
             "in economic policies."),
    h4("Credits"),
    helpText("Created by George Lesica (george@lesica.com) using R",
             "(http://r-project.org) and Shiny",
             "(http://www.rstudio.com/shiny/).",
             "Data source: Bureau of Labor Statistics (http://bls.gov)."),
    helpText("Download the source at https://github.com/glesica/exploring-phillips")
  ),
  mainPanel(
    h4("Visualization"),
    plotOutput("phillipsPlot")
  )
))
