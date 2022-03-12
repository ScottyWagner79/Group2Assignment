library(quantmod)
library(shiny)
library(shinydashboard)

SYMBOLS <- stockSymbols()

ui <- dashboardPage(
  dashboardHeader(title= "Stock Dashboard"), 
  dashboardSidebar(), 
  dashboardBody(
    box(   title= "Company Selection",
           selectInput("select",
                       label = h3("Select A Stock Name"), 
                       choices = names(table(SYMBOLS$Name)), 
                       selected = 1),
           hr(),
           fluidRow(column(3, textOutput("value")))),
        box(
            
            helpText("Please select a symbol."),
            
            textInput("input1", "Enter Valid Stock Symbol:", value = "", width = NULL, placeholder = "AAPL"),
            
            sliderInput("input2", "Last Months Data:", 
                        min=1, max=100, value=1)
          ),
          
          box(textOutput("text1"),textOutput("text2"),plotOutput("fig5"), tableOutput("view"), width = 10, height=10)
      )
  )

server <- function(input, output, session) {
  
  output$value <- renderText({ SYMBOLS$Symbol[which(SYMBOLS$Name == input$select)] })
  output$fig5 <- renderPlot({
    output$text1 <- renderText({paste("Output: ", input$input1)})
    validate(
      need(input$input1 != "", "Please enter Valid Stock Symbol")
    )
    
    
    tryCatch({
      data <- getSymbols(
        input$input1,
        src = "yahoo",
        to = Sys.Date(),
        auto.assign = FALSE
      )
    },
    error=function(e) {
      output$text1 <- renderText({paste(input$input1, " is not a valid symbol.")})
      return(NULL) 
    }
    )
    
    output$view <- renderTable({
      head(data, n = 5)
    }, include.rownames = TRUE)
    
    m <- paste0("last ",input$input2, " months")
    
    output$text2 <- renderText({paste("Currenty Showing: " , m)})
    tryCatch({
      chartSeries(data,
                  theme = chartTheme("white"),
                  type = "line",
                  subset = m ,
                  TA = NULL)
    },
    error=function(e) {
      output$text2 <- renderText({paste("")})
      return(NULL) 
    }
    )
  }
  ) 
}


shinyApp(ui, server)
