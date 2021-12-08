library(shiny)
library(magick)
library(tidyverse) 
library(colourpicker)
library(rsconnect)
library(shinythemes)

bcl <- read.csv("bcl-data.csv")

options(shiny.autoreload = TRUE)
#max_price <- max(bcl$Price, na.rm = TRUE)

price_filter <- function(lwb,upb) {
  bcl %>% filter(lwb < Price & Price < upb) %>% nrow()
}

ui <- fluidPage(
   theme=shinytheme("slate"), 
   themeSelector(),
   #this step changed the theme of the r shiny and also allow user to choose different themes from 17 options. 
   
   titlePanel("BCL app"),
   "This is an app that can help you with exploring the alcohol content at BC Liquor stores between two price points.", 
   tags$br(), 
   tags$br(), 
   sidebarLayout(
     sidebarPanel(
       sliderInput("my_slider", "Select a price range", 
                   min = 0, max = 200, value = c(10,30)),
       radioButtons(
         "my_radio", "Select beverage type.", 
         choices = unique(bcl$Type)
       ), 
       img(src='images.jpeg', align = "top left",
           tags$br(),
           tags$br(), 
           tags$br(),
           style="width: 100%; height: 100%")
       #this step modified the layout of the picture and increase user experience 
     ), 
     mainPanel(
       verbatimTextOutput("result"), 
       colourInput("col", "Choose colour", "#3D4152"),
       #this step changed the color to match the default theme of the r shiny
       plotOutput("my_plot"), 
       tableOutput("my_table")
     )
   )
)

server <- function(input, output) {
  filtered <- reactive({
    #print(input$my_slider)
    #print(input$my_radio)
    bcl %>%
    filter(Price < input$my_slider[2],
           Price > input$my_slider[1],
           Type == input$my_radio)
  })
  
  output$my_plot <- renderPlot(
    filtered() %>%
    ggplot(aes(Alcohol_Content)) +
      geom_histogram(fill=input$col)
  )
  output$result <- renderText(
      paste("we have found",
            price_filter(input$my_slider[1],input$my_slider[2]) %>% as.character(), 
      "results for you"))
  #this step rendered the text and replaced the previous sentence of code and increase user experience
  output$my_table <- renderTable(
    filtered()
    
  )
}



shinyApp(ui = ui, server = server)

