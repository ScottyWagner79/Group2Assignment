library(shiny)
library(tidyquant)
library(quantmod)
start<- as.Date("2017-01-01")
end <- as.Date("2022-03-01")
STOCK10 <- c("BRK-A", "LDSVF", "NXGPF", "AMZN", "SEB", "NVR", "BKNg", "MKL", "MRF", "GOOGL")
TOP10STOCKS <- tq_get(STOCK10, from = start, to = end)


ui <- fluidPage( 
  theme = bslib::bs_theme(bootswatch = "darkly"),
  sidebarPanel(
      
    radioButtons("radio", label = h3("Select Top Company"),
                 choices = 
                   c("BRK-A", "LDSVF", "NXGPF", "AMZN", "SEB", "NVR", "BKNg", "MKL", "MRF", "GOOGL")),
    dateInput("date", label = h3("Stock Date"), value = "2021-01-01")
  ),
  titlePanel("Stock Close and Open"),
  
  hr(),
  
  mainPanel( "Closing",
  fluidRow(column(3, verbatimTextOutput( "value")))
  ),
  mainPanel( "Opening",
  fluidRow(column(3, verbatimTextOutput( "values"))))
)

server <- function(input, output, session) {
  output$value <- renderPrint({ 
      TOP10STOCKS$close[which(TOP10STOCKS$date == input$date &
                                 TOP10STOCKS$symbol == input$radio)]})
      
      
  output$values <- renderPrint( 
        ({  TOP10STOCKS$open[which(TOP10STOCKS$date == input$date &
                                      TOP10STOCKS$symbol == input$radio)]})
      )
      
  
}

shinyApp(ui = ui, server = server)
