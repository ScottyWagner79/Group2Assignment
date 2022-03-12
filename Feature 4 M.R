library(shiny)
library(quantmod)
library(shinydashboard)
library(ggplot2)
library(tidyquant)
library(fpp3)
library(ggthemes)

AAPL <- getSymbols(Symbols = "AAPL", src = "yahoo", from= start, to= end, auto.assign = FALSE)
AAPL <- Cl(AAPL)
TopAAPL <- log(AAPL)
head(AAPL, 10)

SPG <- getSymbols(Symbols = "SPG", src = "yahoo", from= start, to= end, auto.assign = FALSE)
SPG <- Cl(SPG)
TopSPG <- log(SPG)
head(SPG, 10)

GOOG <- getSymbols(Symbols = "GOOG", src = "yahoo", from= start, to= end, auto.assign = FALSE)
GOOG <- Cl(GOOG)
TopGOOG <- log(GOOG)
head(GOOG, 10)

STOCKS <- cbind(TopAAPL, TopSPG, TopGOOG)

ui <- dashboardPage(
  
  dashboardHeader(title="Stock Price Forecasting"),
  dashboardSidebar(),
  
  dashboardBody(
    selectInput("select", 
                label= h3("Select Company"), 
                choices = list("AAPL"= "AAPL", 
                               "SPG" = "SPG", 
                               "GOOG" = "GOOG"), 
                selected= "AAPL"),
    numericInput("ahead", "Years to Forecast Ahead:", 2),
    submitButton("Update View"),
    
    hr(), 
    plotOutput("op"))
)

server <- function(input, output) {
  
  Dataset <- reactive({
    if (input$STOCKS=="AAPL.Close")
    {
      return(AAPL)
    }
    else if (input$STOCKS=="SPG.Close")
    {
      return(SPG)
    }
    else (input$STOCKS=="GOOG.Close")
    {
      return(GOOG)
    }
  })

output$op <- renderPlot({
    
    Dataset() %>%
      model(ETS= ETS(price),
            ARIMA = ARIMA(price)) %>%
      forecast(h = paste0(input$ahead," years")) %>%
      autoplot(Dataset()) +
      labs(title = paste0(input$ahead, " year forecasts for price of ", input$Dataset))

})

output$selection <- DT::renderDataTable({
  
  fit <- Dataset() %>%
    filter(DATE < yearmonth("2022 March")) %>%
    model(
      ETS(price),
      ARIMA(price)) %>%
    forecast(h= "10 years")
  
  selection <- fit %>% accuracy(Dataset())
  
  selection
})
}
shinyApp(ui, server)

