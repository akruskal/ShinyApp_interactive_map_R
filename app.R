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

#**** Task 1 preprocessing ***************************************************
Task1.data <- read.csv('data/hospitals.csv')
names(Task1.data) <- make.names(names(Task1.data))
levels(Task1.data$State) <- c("ACT", "NSW", "NT",  "QLD", "QLD", "SA",  "TAS", "VIC", "WA")
colnames(Task1.data)[1] <- "Hospital.name"

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
  
  StateInput <- reactive({
    switch(input$State,
           "All" = 1, 
           "New South Wales" = Task1.data$State == 'NSW', 
           "Victoria" = Task1.data$State == 'VIC', 
           "Queensland" = Task1.data$State == 'QLD', 
           "South Australia" = Task1.data$State == 'SA',
           "Tasmania" = Task1.data$State == 'TAS', 
           "Western Australia" = Task1.data$State == 'WA', 
           "Northern Territory" = Task1.data$State == 'NT', 
           "Australian Capital Territory" = Task1.data$State == 'ACT')
  
  })
  
  SectorInput <- reactive({
    switch(input$Sector,
           "All" = 1, 
           "Public" = Task1.data$Sector == 'Public', 
           "Private" = Task1.data$Sector == 'Private')
    
  })
  
  BedsInput <- reactive({
    switch(input$Beds,
           "Any" = 1, 
           ">500" = Task1.data$Beds == '>500', 
           "200-500" = Task1.data$Beds == '200-500',
           "100-199" = Task1.data$Beds == '100-199',
           "50-99" = Task1.data$Beds == '50-99',
           "<50" = Task1.data$Beds == '<50',
           "Other" = Task1.data$Beds == '')
    
  })


  
  
  output$Map1 <- renderLeaflet({
    
    # 4 combinations of state inputs selected any with another one selected all  
    if (StateInput() == 1 & SectorInput() == 1 & BedsInput() == 1){
      Task1.reactive <- Task1.data[]
    }else if (StateInput() == 1 & SectorInput() == 1){
      Task1.reactive <- Task1.data[BedsInput(),]
    }else if(StateInput() == 1 & BedsInput() == 1){
      Task1.reactive <- Task1.data[SectorInput(),]
    }else if(StateInput() == 1) {
      Task1.reactive <- Task1.data[SectorInput() & BedsInput(),]
    # 2 combos left for SectorInputs() is all
    }else if(SectorInput() == 1 & BedsInput() == 1) {
      Task1.reactive <- Task1.data[StateInput(),]
    }else if(SectorInput() == 1) {
      Task1.reactive <- Task1.data[StateInput() & BedsInput(),]
    # 1 combo for BedsInput() is any
    }else if(BedsInput() == 1) {
      Task1.reactive <- Task1.data[StateInput() & SectorInput(),]
    }else {
      Task1.reactive <- Task1.data[StateInput() & SectorInput() & BedsInput(),]
    }
      
    
    
      leaflet(Task1.reactive) %>%
      addTiles() %>%
      addPolygons(data=State_polys, weight=2, fillColor = "transparent") %>%
      addAwesomeMarkers(lng = ~Longitude, lat = ~Latitude,icon=~HospitalIcons[Sector], 
                        label = Task1.data$Hospital.name,
                        popup = paste('<b>', Task1.data$Hospital.name, '</b> <br> Phone:', Task1.data$Phone.number, '<br> Website:', Task1.data$Website, '<br> Description:', Task1.data$Description
                        ) )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

