# This is Dani and Meghan's shiny app!

library(tidyverse)
library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("California National Park App"),
                sidebarLayout(
                  sidebarPanel(),
                  mainPanel(
                    p("This application can be used to access information regarding California's National Parks. Each tab provides different information including general park information, park locations, animal species within each park, and the park biological diversity index.")
                  )
                ),
  
  navbarPage("California National Park Biological Diversity App",
             tabPanel("California National Park Information",
                      sidebarLayout(
                        sidebarPanel("widget1",
                                     radioButtons("radio", label = h3("California National Parks:"),
                                                  choices = unique(parks_data$park_name), 
                                                  selected = 1),
                                     
                                     hr(),
                                     fluidRow(column(3, verbatimTextOutput("value")))
                                     ),
                        mainPanel(
                          tabsetPanel(type = "tab",
                                      tabPanel("Park Map", plotOutput("plot")),
                                      tabPanel("Summary", verbatimTextOutput("summary")))
                        )
                      )
                      ),
             tabPanel("Park Locations",
                      sidebarLayout(
                        sidebarPanel("widget2",
                                     fileInput("View Park", label = h3("File input")),
                                     
                                     hr(),
                                     fluidRow(column(4, verbatimTextOutput("value")))
                                     ),
                        mainPanel(tabsetPanel(type = "tab",
                                              tabPanel("Graph", plotOutput("plot")),
                                              tabPanel("Summary", verbatimTextOutput("summary"))))
                      )
                      ),
             tabPanel("Park Species Categories",
                      sidebarLayout(
                        sidebarPanel("widget3",
                                     checkboxGroupInput(inputId = "species_category",
                                                        label = "Select species category:",
                                                        choices = unique(species_data$category))
                                     ),
                        mainPanel("Bar Graph",
                                  plotOutput("category_plot"))
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
                        mainPanel(tabsetPanel(type = "tab",
                                              tabPanel("Species Stats", plotOutput("plot")),
                                              tabPanel("Summary", verbatimTextOutput("summary"))))
                      )
                      )
             
    
  )
  
)

server <- function(input, output) {
  
  # Widget 3 - Species Categories
  category_reactive <- reactive({
    
    species_data %>% 
      group_by(park_name, category) %>% 
      count()
    
  })
  
  output$category_plot <- renderPlot(
    ggplot(data = category_reactive(), aes(x = park_name, y = n)) +
      geom_col(aes(color=category)) +
      facet_wrap(~category, scales = "free")
  )
  
}

shinyApp(ui = ui, server = server)
