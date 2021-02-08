# This is Dani and Meghan's shiny app!

library(tidyverse)
library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("cerulean"),
  
  navbarPage("US National Park Biological Diversity App",
             tabPanel("US National Park Information",
                      sidebarLayout(
                        sidebarPanel("widget1",
                                     radioButtons("radio", label = h3("US National Parks:"),
                                                  choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                                                  selected = 1),
                                     
                                     hr(),
                                     fluidRow(column(3, verbatimTextOutput("value")))
                                     ),
                        mainPanel("output1")
                      )
                      ),
             tabPanel("Park Locations",
                      sidebarLayout(
                        sidebarPanel("widget2",
                                     fileInput("View Park", label = h3("File input")),
                                     
                                     hr(),
                                     fluidRow(column(4, verbatimTextOutput("value")))
                                     ),
                        mainPanel("output2")
                      )
                      ),
             tabPanel("Park Animal Species",
                      sidebarLayout(
                        sidebarPanel("widget3",
                                     textInput("text", label = h3("Animal Look-Up"), value = "Enter text..."),
                                     
                                     hr(),
                                     fluidRow(column(3, verbatimTextOutput("value")))
                                     ),
                        mainPanel("output3")
                      )
                      ),
             tabPanel("Park Biological Diversity Index",
                      sidebarLayout(
                        sidebarPanel("widget4",
                                     sliderInput("slider1", label = h3("Slider"), min = 0, 
                                                 max = 100, value = 50),
                                     hr(),
                                     
                                     fluidRow(
                                       column(4, verbatimTextOutput("value")),
                                       column(4, verbatimTextOutput("range"))
                                     )
                                     ),
                        mainPanel("output4")
                      )
                      )
             
    
  )
  
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)
