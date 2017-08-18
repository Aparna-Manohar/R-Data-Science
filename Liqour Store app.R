#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages("shinythemes")
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)

# Liquor store prices
bcl <- read.csv("/Users/aparnamanohar/Desktop/BC_Liqour_Store/bcl-data.csv", stringsAsFactors = FALSE)
#View(bcl)
nrow(bcl)
str(bcl)

range(bcl$Price, na.rm = TRUE)
#Highest price is 30250

?sliderInput
#Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("BC Liquor Store prices") , theme = shinytheme("cosmo"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("priceInput", "Price", min=0, max=100, value=c(25, 40), pre = "$"),
            radioButtons("typeInput", "Product type",
                         choices =c("BEER", "REFRESHMENT", "SPIRITS", "WINE"), selected = "WINE"),
            uiOutput("countryOutput")
        ),
        mainPanel(
            plotOutput("coolplot"),
            br(), br(),
            h4(textOutput("text")),
            br(),
            tableOutput("results")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$countryOutput <- renderUI({
        selectInput("countryInput", "Country", sort(unique(bcl$Country)), selected = "CANADA")
    })  
    
    filtered <- reactive({
        if (is.null(input$countryInput)) {
            return(NULL)
        }    
        bcl %>%
            filter(Price >= input$priceInput[1],
                   Price <= input$priceInput[2],
                   Type == input$typeInput,
                   Country == input$countryInput
            )
    })
    
    output$text <- renderText({
        x = nrow(filtered())
        print(paste("The number of products are",x))
        })
    
    output$coolplot <- renderPlot({
        if (is.null(filtered())) {
            return()
        }
        ggplot(filtered(), aes(Alcohol_Content), fill = blue) + geom_histogram() 
    })
    
    output$results <- renderTable({
        filtered()
    })

}

# Run the application 
shinyApp(ui = ui, server = server)

