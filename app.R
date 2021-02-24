# This is Dani and Meghan's shiny app!

library(tidyverse)
library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("California National Park App"),
                sidebarLayout(
                  sidebarPanel("Home Page"),
                  mainPanel(
                    p("This application can be used to access information regarding California's National Parks. Each tab provides different information including general park information, park locations, animal species within each park, and the park biological diversity index. It is our hope that this app can be used by a wide variety of inviduals and corporations. From people within the general public looking to plan a fun weekend excursion to kids working on schools research projects to organizations needing information on nearby parks, we believe this app has a little something for everyone."),
                    p("The California National Park App was created and produced by Danielle Sclafani and Meghan Fletcher as part of their Advanced Data Analysis graduate course. Progress of this app was overseen by Dr. Allison Horst. All data used in the development of this app was from The National Parks Service.")
                  )
                ),
  
  navbarPage("California National Park Biological Diversity App",
             tabPanel("California National Park Information",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("radio", label = h3("California National Parks:"),
                                                  choices = unique(parks_data$park_name), 
                                                  selected = 1),
                                     
                                     hr(),
                                     fluidRow(column(3, verbatimTextOutput("value")))
                                     ),
                        mainPanel(
                          tabsetPanel(type = "tab",
                                      tabPanel("Park Map",  imageOutput("Image")),
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
                        sidebarPanel(selectInput(inputId = "species_category",
                                                        label = "Select species category:",
                                                        choices = unique(species_data$category)),
                                     hr(),
                                     helpText("Data from the National Park Service")
                                     ),
                        mainPanel(
                          tabsetPanel(type = "tab",
                                              tabPanel("Bar Graph", plotOutput("category_plot")),
                                              tabPanel("Summary", verbatimTextOutput("summary"))))
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
  
  # Widget 1
  output$Image <- renderUI({
    if(input$park_name == "Channel Islands National Park"){
      img(height = 240, width = 300, src = "channel_islands_map.jpg")
    }
    else if(input$park_name == "Death Valley National Park"){
      img(height = 240, width = 300, src = "death_valley_map.jpg")
    }
    else if(input$park_name == "Joshua Tree National Park"){
      img(height = 240, width = 300, src = "joshua_tree_map.jpg")
    }
    else if(input$park_name == "Lassen Volcanic National Park"){
      img(height = 240, width = 300, src = "lassen_volcanic_map.jpg")
    }
    else if(input$park_name == "Pinnacles National Park"){
      img(height = 240, width = 300, src = "pinnacles_map.jpg")
    }
    else if(input$park_name == "Redwood National Park"){
      img(height = 240, width = 300, src = "redwood_map.jpg")
    }
    else if(input$park_name == "Sequoia and Kings Canyon National Parks"){
      img(height = 240, width = 300, src = "sequoia_map.jpg")
    }
    else if(input$park_name == "Yosemite National Park"){
      img(height = 240, width = 300, src = "yosemite_map.jpg")
    }
  })
  
  # Widget 2
  
  # Widget 3 - Species Categories
  category_reactive <- reactive({
    
    species_category_all %>% 
      filter(category == input$species_category)
    
  }) # widget 3 reactive parentheses
  
  output$category_plot <- renderPlot(
    
    ggplot(data = category_reactive(), aes(x = park_name, y = n)) +
      geom_col()
    
  ) # widget 3 output parentheses
  
  # Widget 4
}

shinyApp(ui = ui, server = server)
