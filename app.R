# This is Dani and Meghan's shiny app!

library(tidyverse)
library(shiny)
library(shinythemes)
library(here)
library(janitor)
library(sf)
library(tmap)


####### Read in data for widget 1:
park_boundaries <- 
  st_read(
    here("data", 
         "ne_10m_parks_and_protected_lands" ,
         "ne_10m_parks_and_protected_lands_area.shp")
  ) %>% 
  clean_names()
####### End of data for widget 1

###### Widget 2 Data
#selecting only necessary values for species data
species_data_v2 <- read_csv(here("data", "species.csv")) %>% 
  clean_names() %>%
  filter(occurrence == "Present") %>% 
  filter(park_name %in% c("Sequoia and Kings Canyon National Parks",
                          "Channel Islands National Park",
                          "Redwood National Park",
                          "Death Valley National Park",
                          "Joshua Tree National Park",
                          "Lassen Volcanic National Park",
                          "Yosemite National Park",
                          "Pinnacles National Park")) %>% 
  mutate(name = case_when(
    park_name == "Sequoia and Kings Canyon National Parks" ~ "Sequoia",
    park_name == "Channel Islands National Park" ~ "Channel Islands",
    park_name == "Redwood National Park" ~ "Redwood",
    park_name == "Death Valley National Park" ~ "Death Valley",
    park_name == "Joshua Tree National Park" ~ "Joshua Tree",
    park_name == "Lassen Volcanic National Park" ~ "Lassen Volcanic",
    park_name == "Yosemite National Park" ~ "Yosemite",
    park_name == "Pinnacles National Park" ~ "Pinnacles"))%>% 
  mutate(common_names = tolower(common_names))# so when user types in animal name in lower case it will detect it


#renaming park_code to unit_code in order to match the park boundaries dataset
park_data_v2 <- read_csv(here("data", "parks.csv"))%>% 
  clean_names() %>% 
  rename(unit_code = park_code) %>% 
  filter(str_detect(state, pattern= "CA")) %>% 
  mutate(name = case_when(
    park_name == "Sequoia and Kings Canyon National Parks" ~ "Sequoia",
    park_name == "Channel Islands National Park" ~ "Channel Islands",
    park_name == "Redwood National Park" ~ "Redwood",
    park_name == "Death Valley National Park" ~ "Death Valley",
    park_name == "Joshua Tree National Park" ~ "Joshua Tree",
    park_name == "Lassen Volcanic National Park" ~ "Lassen Volcanic",
    park_name == "Yosemite National Park" ~ "Yosement",
    park_name == "Pinnacles National Park" ~ "Pinnacles"))


#combining park data  with park boundary data (contains all 7 NPs in CA)
park_data_bound <- merge(park_boundaries, park_data_v2, by = "name")


#combining the above combined dataset with the species data set by the name of the park (contains all 7 NPs in CA)
species_park_bound <- merge(park_data_bound, species_data_v2, by = "name")

#map data
ca_county_map <- st_read(here("data", "ca_counties","CA_Counties_TIGER2016.shp"))

###### END OF WIDGET 2 DATA

####### Widget 3 data
parks_data <- read_csv(here("data", "parks.csv")) %>% 
  clean_names() %>% 
  filter(state %in% c("CA", "CA, NV"))


species_data <- read_csv(here("data", "species.csv")) %>% 
  clean_names() %>% 
  filter(park_name %in% c("Channel Islands National Park", "Death Valley National Park", "Joshua Tree National Park", "Lassen Volcanic National Park", "Pinnacles National Park", "Redwood National Park", "Sequoia and Kings National Park", "Yosemite National Park")) %>% 
  mutate(common_names = tolower(common_names))

species_category_all <- species_data %>% 
  group_by(park_name, category) %>% 
  count()
####### End of widget 3 data


ui <- fluidPage(theme = "parks.css",
  
  navbarPage(title = "California National Parks Informational App",
             tabPanel("Home",
                img(src = "yosemite_home.jpg", height = 200, width = 850),
                h1(strong("Welcome to the California National Parks Informational App")),
                h4(strong("Created by: Danielle Sclafani & Meghan Fletcher")),
                hr(),
                h4("The intention of this application is to enable viewers to quickly find basic geographic and species information for California’s major national parks. Each tab provides different information including geographic information on all U.S. national parks, a CA species locator, differences between the species categories of each CA park and maps of the CA parks. It is our hope that this app can be used by a wide variety of individuals and corporations. From people within the general public looking to plan a fun weekend excursion to kids working on school research projects to organizations needing information on nearby parks, we believe this app has a little something for everyone."),
                hr(),
                h3(strong("Widget Information")),
                h4("There are four interactive widgets in this app. Each widget is housed in a separate tab (see below)."),
                h4(strong("Widget 1: U.S. National Parks Interactive Map")),
                h4("This widget allows users to select one or more regions across the U.S. to see where the various national parks are using an interactive tmap.")
               ),
                 
             
             tabPanel("U.S. National Parks Interactive Map",
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput(inputId = "region_selected", 
                                       label = h3("Select region:"),
                                       choices = unique(park_boundaries$nps_region), 
                                                  selected = "Pacific West"),
                                     
                                     hr(),
                                     fluidRow(column(5, verbatimTextOutput("value")))
                                     ),
                        mainPanel(
                          tabsetPanel(type = "tab",
                                      tabPanel("Map of Parks",  tmapOutput("region_plot")),
                                      tabPanel("Summary", verbatimTextOutput("summary")))
                        )
                      )
                      ),
            tabPanel("Species Locator",
                      sidebarLayout(
                        sidebarPanel(
                          textInput(inputId = "species_locator",
                                    label = h3("Species Locator"),
                                   value = "lion"),
                          hr(),
                        ),
                        mainPanel(plotOutput("species_locator_plot"))
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
                        mainPanel(plotOutput("category_plot"))
                      )
                      ),
            tabPanel("CA Park Images",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons(inputId = "park_selected", 
                                       label = h3("California National Parks:"),
                                       choices = list("Channel Islands National Park",
                                                      "Death Valley National Park",
                                                      "Joshua Tree National Park",
                                                      "Lassen Volcanic National Park",
                                                      "Pinnacles National Park",
                                                      "Redwood National Park",
                                                      "Sequoia and Kings Canyon National Parks",
                                                      "Yosemite National Park"), 
                                       selected = "Channel Islands National Park"),
                          
                          hr(),
                        ),
                        mainPanel(
                          (uiOutput("image"))
                        )
                     )
                      )
             
    
 
             )  
)

server <- function(input, output) {
  
  # Widget 1
  region_reactive <- reactive({
    park_boundaries %>% 
      filter(nps_region %in% input$region_selected)
    
  }) # Widget 1 reactive parentheses
  
  output$region_plot <- renderTmap(
    tm_basemap("Esri.WorldTopoMap")+
    tm_shape(region_reactive()) +
      tm_polygons(fill = "nps_region")
    
  ) # widget 1 output parentheses

  
  # Widget 2
  
species_reactive <- reactive({
  species_park_bound %>% 
    filter(str_detect(common_names, pattern = input$species_locator))
})

output$species_locator_plot <- renderPlot(
  ggplot(data = ca_county_map) + geom_sf()+
    geom_sf(data = species_reactive(), fill = "blue")
)
  
  
  # Widget 3 - Species Categories
  category_reactive <- reactive({
    species_category_all %>% 
      filter(category == input$species_category)
    
  }) # widget 3 reactive parentheses
  
  output$category_plot <- renderPlot(
    
    ggplot(data = category_reactive(), aes(x = park_name, y = n)) +
      geom_col(aes(fill = park_name)) +
      coord_flip() +
      theme_bw() +
      theme(legend.position = "none") +
      labs(x = "", y = "Number of Species")
    
  ) # widget 3 output parentheses
  
  # Widget 4
  
  output$image <- renderUI({
    if(input$park_selected == "Channel Islands National Park"){
      img(height = 600, width = 570, src = "images/channel_islands_map.jpg")
    }
    else if(input$park_selected == "Death Valley National Park"){
      img(height = 600, width = 570, src = "images/death_valley_map.jpg")
    }
    else if(input$park_selected == "Joshua Tree National Park"){
      img(height = 600, width = 570, src = "images/joshua_tree_map.jpg")
    }
    else if(input$park_selected == "Lassen Volcanic National Park"){
      img(height = 600, width = 570, src = "images/lassen_volcanic_map.jpg")
    }
    else if(input$park_selected == "Pinnacles National Park"){
      img(height = 600, width = 570, src = "images/pinnacles_map.jpg")
    }
    else if(input$park_selected == "Redwood National Park"){
      img(height = 600, width = 570, src = "images/redwood_map.jpg")
    }
    else if(input$park_selected == "Sequoia and Kings Canyon National Parks"){
      img(height = 600, width = 570, src = "images/sequoia_map.jpg")
    }
    else if(input$park_selected == "Yosemite National Park"){
      img(height = 600, width = 570, src = "images/yosemite_map.jpg")
    }
    
  })
  
}

shinyApp(ui = ui, server = server)
