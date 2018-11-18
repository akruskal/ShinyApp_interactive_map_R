#
# HDAT 9800 Group Assessment — Health Explorer
#
# Group member task 1: Alexander Kruskal
# Group member task 2:
# Group member task 3: Anran Huang
#

library(shiny)
library(rgdal)
library(sp)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(ggplot2)

Task1.data <- read.csv('data/hospitals.csv')
names(Task1.data) <- make.names(names(Task1.data))
levels(Task1.data$State) <- c("ACT", "NSW", "NT",  "QLD", "QLD", "SA",  "TAS", "VIC", "WA")

State_polys = readOGR("data/STE_2016_AUST.shp")

HospitalIcons <- awesomeIconList(
  Public = makeAwesomeIcon(icon = 'hospital-o', markerColor = 'green', iconColor = 'white', library = "fa"),
  Private = makeAwesomeIcon(icon = 'hospital-o', markerColor = 'orange', iconColor = 'white', library = "fa")
)




# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Health Data Explorer"),
  
  tabsetPanel(
    tabPanel("Task 1 — Hospitals",  p(),
             
             sidebarLayout(
               sidebarPanel(
                 # add task 1 sidebar elements here
                 
                 # Input: Selector for choosing State ----
                 selectInput(inputId = "State",
                             label = "State",
                             choices = c("All", "New South Wales", "Victoria", "Queensland", "South Australia",
                                         "Tasmania", "Western Australia", "Northern Territory", "Australian Capital Territory")),
                 # Input: Selector for choosing Sector ----
                 selectInput(inputId = "Sector",
                             label = "Sector",
                             choices = c("All", "Public", "Private")),
                 # Input: Selector for choosing Beds ----
                 selectInput(inputId = "Beds",
                             label = "Beds",
                             choices = c("Any", ">500", "200-500", "100-199", "50-99", "<50", "Other"))
               ),
               
               mainPanel(
                 # add task 1 main panel elements here
                 leafletOutput('Map1')
                
               )
             )
             
    ),
    tabPanel("Task 2 — Distance from Emergency Department", p(),
             
             sidebarLayout(
               sidebarPanel(
                 # add task 2 sidebar elements here
               ),
               
               mainPanel(
                 # add task 2 main panel elements here
               )
             )
             
    ),
    tabPanel("Task 3 — Length of Hospital Stay", p(),
             
             sidebarLayout(
               sidebarPanel(
                 # add task 3 sidebar elements here
               ),
               
               mainPanel(
                 # add task 3 main panel elements here
               )
             )
             
    )
  )
)

# Define server logic
server <- function(input, output) {
  


  
  output$Map1 <- renderLeaflet({
    
    
      leaflet(Task1.data) %>%
      addTiles() %>%
      addPolygons(data=State_polys, weight=2, fillColor = "transparent") %>%
      addAwesomeMarkers(lng = ~Longitude, lat = ~Latitude,icon=~HospitalIcons[Sector], 
                        label = Task1.data$Hospital.name,
                        popup = paste(Task1.data$Hospital.name, Task1.data$Phone.number, Task1.data$Website, Task1.data$Description
                        ) )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

