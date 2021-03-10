# This is Dani and Meghan's shiny app!

library(tidyverse)
library(shiny)
library(shinythemes)
library(here)
library(janitor)
library(sf)
library(tmap)
library(viridis)
library(shinydashboard)


####### Read in data for widget 1:
park_boundaries <- 
  st_read(
    here("data", 
         "ne_10m_parks_and_protected_lands" ,
         "ne_10m_parks_and_protected_lands_area.shp")
  ) %>% 
  clean_names()%>% 
  select("name", "nps_region")
####### End of data for widget 1

###### Widget 2 Data
#selecting only necessary values for species data
species_data_v2 <- read_csv(here("data", "species.csv")) %>% 
  clean_names() %>%
  filter(occurrence == "Present") %>% 
  mutate(name = case_when(
    park_name == "Sequoia and Kings Canyon National Parks" ~ "Sequoia",
    park_name == "Channel Islands National Park" ~ "Channel Islands",
    park_name == "Redwood National Park" ~ "Redwood",
    park_name == "Death Valley National Park" ~ "Death Valley",
    park_name == "Joshua Tree National Park" ~ "Joshua Tree",
    park_name == "Lassen Volcanic National Park" ~ "Lassen Volcanic",
    park_name == "Yosemite National Park" ~ "Yosemite",
    park_name == "Pinnacles National Park" ~ "Pinnacles",
    park_name == "Badlands National Park" ~ "Badlands",
    park_name == "Big Bend National Park" ~ "Big Bend",
    park_name == "Canyonlands National Park" ~ "Canyonlands",
    park_name == "Capitol Reef National Park" ~ "Capital Reef",
    park_name == "Crater Lake National Park" ~ "Crater Lake",
    park_name == "Denali National Park and Preserve" ~ "Denali",
    park_name == "Everglades National Park" ~ "Everglades",
    park_name == "Gates of the Arctic National Park and Preserve" ~ "Gates of the Artic",
    park_name == "Glacier National Park" ~ "Glacier",
    park_name == "Glacier Bay National Park and Preserve" ~ "Glacier Bay",
    park_name == "Grand Canyon National Park" ~"Grand Canyon",
    park_name == "Great Sand Dunes National Park and Preserve" ~ "Great Sand Dunes",
    park_name ==  "Great Smoky Mountations National Park" ~ "Great Smoky Mountains",
    park_name ==  "Grand Teton National Park" ~ "Grand Teton",
    park_name == "Hawaii Volcanoes National Park" ~ "Hawai'i Volcanoes",
    park_name == "Isle Royale National Park" ~ "Isle Royale",
    park_name == "Katmai National Park and Preserve" ~ "Katmai",
    park_name == "Kenai Fjords National Park" ~ "Kenai Fjords",
    park_name == "Kobuk Valley National Park" ~ "Kobuk Valley",
    park_name == "Lake Clark National Park and Preserve" ~ "Lake Clark",
    park_name == "Mount Rainer National Park" ~ "Mount Rainer",
    park_name == "North Cascades National Park" ~ "North Cascades",
    park_name == "Olympic National Park" ~ "Olympic",
    park_name == "Petrified Forest National Park" ~ "Petrified Forest",
    park_name == "Rocky Mountain National Park" ~ "Rocky Mountain",
    park_name == "Shenandoah National Park" ~ "Shenandoah",
    park_name == "Voyageurs National Park" ~ "Voyageurs",
    park_name == "Wrangell - St Elias National Park and Preserve" ~ "Wrangell-St Elias",
    park_name == "Yellowstone National Park" ~ "Yellowstone",
    park_name == "Zion National Park" ~ "Zion"))%>% 
  mutate(common_names = tolower(common_names))# so when user types in animal name in lower case it will detect it


#renaming park_code to unit_code in order to match the park boundaries dataset
park_data_v2 <- read_csv(here("data", "parks.csv"))%>% 
  clean_names() %>% 
  rename(unit_code = park_code) %>% 
  mutate(name = case_when(
    park_name == "Sequoia and Kings Canyon National Parks" ~ "Sequoia",
    park_name == "Channel Islands National Park" ~ "Channel Islands",
    park_name == "Redwood National Park" ~ "Redwood",
    park_name == "Death Valley National Park" ~ "Death Valley",
    park_name == "Joshua Tree National Park" ~ "Joshua Tree",
    park_name == "Lassen Volcanic National Park" ~ "Lassen Volcanic",
    park_name == "Yosemite National Park" ~ "Yosement",
    park_name == "Pinnacles National Park" ~ "Pinnacles",
    park_name == "Badlands National Park" ~ "Badlands",
    park_name == "Big Bend National Park" ~ "Big Bend",
    park_name == "Canyonlands National Park" ~ "Canyonlands",
    park_name == "Capitol Reef National Park" ~ "Capital Reef",
    park_name == "Crater Lake National Park" ~ "Crater Lake",
    park_name == "Denali National Park and Preserve" ~ "Denali",
    park_name == "Everglades National Park" ~ "Everglades",
    park_name == "Gates of the Arctic National Park and Preserve" ~ "Gates of the Artic",
    park_name == "Glacier National Park" ~ "Glacier",
    park_name == "Glacier Bay National Park and Preserve" ~ "Glacier Bay",
    park_name == "Grand Canyon National Park" ~"Grand Canyon",
    park_name == "Great Sand Dunes National Park and Preserve" ~ "Great Sand Dunes",
    park_name ==  "Great Smoky Mountations National Park" ~ "Great Smoky Mountains",
    park_name ==  "Grand Teton National Park" ~ "Grand Teton",
    park_name == "Hawaii Volcanoes National Park" ~ "Hawai'i Volcanoes",
    park_name == "Isle Royale National Park" ~ "Isle Royale",
    park_name == "Katmai National Park and Preserve" ~ "Katmai",
    park_name == "Kenai Fjords National Park" ~ "Kenai Fjords",
    park_name == "Kobuk Valley National Park" ~ "Kobuk Valley",
    park_name == "Lake Clark National Park and Preserve" ~ "Lake Clark",
    park_name == "Mount Rainer National Park" ~ "Mount Rainer",
    park_name == "North Cascades National Park" ~ "North Cascades",
    park_name == "Olympic National Park" ~ "Olympic",
    park_name == "Petrified Forest National Park" ~ "Petrified Forest",
    park_name == "Rocky Mountain National Park" ~ "Rocky Mountain",
    park_name == "Shenandoah National Park" ~ "Shenandoah",
    park_name == "Voyageurs National Park" ~ "Voyageurs",
    park_name == "Wrangell - St Elias National Park and Preserve" ~ "Wrangell-St Elias",
    park_name == "Yellowstone National Park" ~ "Yellowstone",
    park_name == "Zion National Park" ~ "Zion"))


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


ui <- fluidPage(
  dashboardPage(
      dashboardHeader(title = "CA Parks App"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Home Page", tabName = "home", icon = icon("home")),
          menuItem("Interactive Map", tabName = "map", icon = icon("globe", lib = "glyphicon")),
          menuItem("Species Locator", tabName = "locator", icon = icon("eye-open", lib = "glyphicon")),
          menuItem("Species by Park", tabName = "categories", icon = icon("stats", lib = "glyphicon")),
          menuItem("Parks Pics",tabName = "images", icon = icon("picture", lib = "glyphicon"))
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "home",
                  img(src = "yosemite_home.jpg", height = 260, width = 1370),
                    h1(strong("Welcome to the California National Parks Informational App")),
                    h4(strong("Created by: Danielle Sclafani & Meghan Fletcher")),
                    hr(),
                    h4("The intention of this application is to enable viewers to quickly find basic geographic and species information for California’s major national parks. Each tab provides different information including geographic information on all U.S. national parks, a CA species locator, differences between the species categories of each CA park and maps of the CA parks. It is our hope that this app can be used by a wide variety of individuals and corporations. From people within the general public looking to plan a fun weekend excursion to kids working on school research projects to organizations needing information on nearby parks, we believe this app has a little something for everyone."),
                    hr(),
                    h3(strong("Widget Information")),
                    h4("There are four interactive widgets in this app. Each widget is housed in a separate tab (see below)."),
                    h4(strong("Widget 1: U.S. National Parks Interactive Map")),
                    h4("This widget allows users to select one or more regions across the U.S. to see where the various national parks are using an interactive tmap. The map will automatically zoom to the region of interest."),
                    h4(strong("Widget 2: Species Locator")),
                    h4("This text widget allows users to type in the common name of a species they’re interested in to reveal which California parks contain that species. It is important to note that in order to have the desired species appear, one must input the common name in all lower-case letters. Additionally, it is important to be as specific. For instance, typing “deer” will generate a result that pulls up all deer species. To avoid this, be sure to be specific and type something like “mule deer” to get better results."),
                    h4(strong("Widget 3: Species Categories by Park")),
                    h4("Using this widget, users can view the differences in species categories between the California National Parks.  Users can utilize the drop-down menu to select which species category they’re interested in viewing and a bar graph containing the number of species in that category by park will appear."),
                    h4(strong("Widget 4: Park Maps")),
                    h4("By selecting one of the California National Parks on the left-hand side of the page, users can pull up a zoomable image of each of the California National Parks. These maps contain geographical information like where people can hike and camp and where visitor centers are located within the park."),
                    hr(),
                    h3(strong("Data & Source Information:")),
                    h4("The California National Parks Informational App was created and produced by Danielle Sclafani and Meghan Fletcher as part of their Advanced Data Analysis (ESM 244) graduate course at the Bren School of Environmental Science & Management at the University of California, Santa Barbara. Progress of this app was overseen by Dr. Allison Horst and Casey O’Hara."),
                    h4("Species and California national park data were obtained from the National Park Service using", tags$a(href="https://www.kaggle.com/nationalparkservice/park-biodiversity?select=parks.csv", "Kaggle")),
                    h4("Geometries for the National Parks of the U.S. came from", tags$a(href="https://www.naturalearthdata.com/downloads/10m-cultural-vectors/parks-and-protected-lands/", "rnaturalearth")),
                    h4("CA park maps were also procured from the", tags$a(href="https://www.nps.gov/carto/app/#!/parks/state/ca", "National Park Service website"))),
          
          tabItem(tabName = "map",
          tabPanel("U.S. National Parks Interactive Map",
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput(inputId = "region_selected", 
                                       label = h3("Select region:"),
                                       choices = unique(park_boundaries$nps_region), 
                                                  selected = "Pacific West"),
                                     
                                     hr(),
                          helpText("Select the region of interest to view where the national parks are located within those regions. Users can select more than one region and use the map to zoom in and out of specific locations."),
                                     fluidRow(column(5, verbatimTextOutput("value")))
                                     ),
                        mainPanel(
                          tabsetPanel(type = "tab",
                                      tabPanel("Map of Parks",  tmapOutput("region_plot")))
                        )
                      )
                      )),
          tabItem(tabName = "locator",
          tabPanel("Species Locator",
                      sidebarLayout(
                        sidebarPanel(
                          textInput(inputId = "species_locator",
                                    label = h3("Species Locator"),
                                   value = "lion"),
                          checkboxGroupInput(inputId = "state",
                                             label = h3("Select State"),
                                             choices = unique(species_park_bound$state),
                                             selected = "CA"),
                          hr(),
                        ),
                        mainPanel(tmapOutput("species_locator_plot"),
                                  h4("Enter the common name of the species you are interested in. Then select a state of interest."),
                                  h4("Please note that common names must be entered in all lowercase letters and a state must be selected at all times for the app to function properly. Additionally, be specific in entering common names for better results."))
                      )
                      )),
          tabItem(tabName = "categories",
          tabPanel("Species Categories by Park",
                      sidebarLayout(
                        sidebarPanel(selectInput(inputId = "species_category",
                                                        label = "Select species category:",
                                                        choices = unique(species_data$category)),
                                     hr(),
                                     helpText("By selecting a species category from the top-down menu, users can view the differences in the number of each species categories between each of the California National Parks.")
                                     ),
                        mainPanel(plotOutput("category_plot"))
                      )
                      )),
          tabItem(tabName = "images",
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
                          helpText("Here, users can select the California National Park that they're interested in to pull up a map image of that National park. By hovering the curser of the image, users can zoom in on the image to see where certain trails, camping sites and visitor centers are located.")),
                        mainPanel(
                          uiOutput("image"),
                          tags$style('div#image:hover {
                 transform: scale(1.5);
                 transform-origin: top left;
                }'))
                     ))
             )  
))
))


server <- function(input, output) {
  
  # Widget 1
  region_reactive <- reactive({
    park_boundaries %>% 
      filter(nps_region %in% input$region_selected)
    
  }) # Widget 1 reactive parentheses
  
  output$region_plot <- renderTmap(
    tm_basemap("Esri.WorldStreetMap")+
    tm_shape(region_reactive()) +
      tm_polygons(fill = "nps_region", alpha = 0.3)
    
  ) # widget 1 output parentheses

  
  # Widget 2
  
species_reactive <- reactive({
  species_park_bound %>% 
    filter(str_detect(common_names, pattern = input$species_locator)) %>% 
    filter(state == input$state)
})

output$species_locator_plot <- renderTmap(
  tm_basemap("Esri.WorldTopoMap")+
    tm_shape(species_reactive())+
    tm_polygons(fill = "nps_region", alpha = 0.3)
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
      labs(x = "", y = "Number of Species") +
      scale_color_viridis(discrete = TRUE)+
      scale_fill_viridis(discrete = TRUE) +
      geom_text(aes(label=n, hjust= -0.5, size = 3))
      
    
  ) # widget 3 output parentheses
  
  # Widget 4
  
  output$image <- renderUI({
    if(input$park_selected == "Channel Islands National Park"){
      tags$img(height = 600, width = 570, src = "images/channel_islands_map.jpg")
    }
    else if(input$park_selected == "Death Valley National Park"){
      tags$img(height = 600, width = 570, src = "images/death_valley_map.jpg")
    }
    else if(input$park_selected == "Joshua Tree National Park"){
      tags$img(height = 450, width = 650, src = "images/joshua_tree_map.jpg")
    }
    else if(input$park_selected == "Lassen Volcanic National Park"){
      tags$img(height = 600, width = 700, src = "images/lassen_volcanic_map.jpg")
    }
    else if(input$park_selected == "Pinnacles National Park"){
      tags$img(height = 600, width = 570, src = "images/pinnacles_map.jpg")
    }
    else if(input$park_selected == "Redwood National Park"){
      tags$img(height = 700, width = 600, src = "images/redwood_map.jpg")
    }
    else if(input$park_selected == "Sequoia and Kings Canyon National Parks"){
      tags$img(height = 630, width = 570, src = "images/sequoia_map.jpg")
    }
    else if(input$park_selected == "Yosemite National Park"){
      tags$img(height = 650, width = 600, src = "images/yosemite_map.jpg")
    }
    
  })
  
}

shinyApp(ui = ui, server = server)
