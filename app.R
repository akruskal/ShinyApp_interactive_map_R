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
library(readr)
library(plotly)
library(zoo)
library(lubridate)

data3 <- read_csv("/Users/anranhuang/Desktop/HDAT9700/GroupAssess9800/data/myhospitals-average-length-of-stay-data.csv")
data3$`Peer group` <- as.factor(data3$`Peer group`)
data3$`Category` <- as.factor(data3$`Category`)
data3$`Time period`<- as.factor(data3$`Time period`)
data3$`Average length of stay (days)` <- as.numeric(data3$`Average length of stay (days)`)
data3$`Number of overnight stays`<- as.integer(data3$`Number of overnight stays`)
data3$new[data3$`Number of overnight stays` < 100] = 100
data3$new[(data3$`Number of overnight stays` >= 100) & (data3$`Number of overnight stays` < 200)] = 200
data3$new[(data3$`Number of overnight stays` >= 200) & (data3$`Number of overnight stays` < 300)] = 300
data3$new[(data3$`Number of overnight stays` >= 300) & (data3$`Number of overnight stays` < 400)] = 400
data3$new[(data3$`Number of overnight stays` >= 400) & (data3$`Number of overnight stays` < 500)] = 500
data3$new[(data3$`Number of overnight stays` >= 500) & (data3$`Number of overnight stays` < 600)] = 600
data3$new[(data3$`Number of overnight stays` >= 600) & (data3$`Number of overnight stays` < 700)] = 700
data3$new[(data3$`Number of overnight stays` >= 700) & (data3$`Number of overnight stays` < 800)] = 800
data3$new[(data3$`Number of overnight stays` >= 800) & (data3$`Number of overnight stays` < 900)] = 900
data3$new[(data3$`Number of overnight stays` >= 900) & (data3$`Number of overnight stays` < 1000)] = 1000
data3$new <- as.factor(data3$new)


# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Health Data Explorer"),
  
  tabsetPanel(
    tabPanel("Task 1 — Hospitals",  p(),
             
             sidebarLayout(
               sidebarPanel(
                 # add task 1 sidebar elements here
               ),
               
               mainPanel(
                 # add task 1 main panel elements here
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
                 selectInput(inputId = "peers", label = "Peer Group", 
                             choices = levels(data3$"Peer group"),
                             selected = "Medium hospitals"),
                 selectInput(inputId = "categ", label = "Category", 
                             choices = levels(data3$Category),
                             selected = "Knee replacement")
               ),
               
               mainPanel(
                 # add task 3 main panel elements here
                 plotOutput("plot")
               )
             )
             
    )
  )
)

# Define server logic
server <- function(input, output) {

  # add your output slot render functions here
  output$plot<-renderPlot({
    selectedData <- reactive({
      data3 %>% filter(`Peer group`==input$peers, `Category`==input$categ) 
    })
    
    meanSelectedData <- reactive({
      data3 %>% filter(`Peer group`==input$peers, `Category`==input$categ) %>%
      group_by(`Time period`) %>%
      summarise(`mean` = mean(`Average length of stay (days)`,na.rm = TRUE))
    })
    
    p <- ggplot(data = selectedData() , 
           aes(x=`Time period`, y=`Average length of stay (days)`)) +
           geom_point(aes(size =`Number of overnight stays`), color="#000099", alpha=0.2) +
           ylim(0,6) +
           geom_segment(data = meanSelectedData(), aes(x =c(0.7,1.7,2.7,3.7,4.7,5.7), xend =c(1.3,2.3,3.3,4.3,5.3,6.3), y = `mean`, yend = `mean`), size=1)
           #geom_point(data = meanSelectedData(), aes(y = `mean`, na.rm = TRUE), colour = 'orange')
    p
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

