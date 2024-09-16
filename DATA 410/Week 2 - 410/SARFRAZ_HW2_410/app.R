#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(htmltools)
library(shinythemes)
library(shiny)
library(dplyr)
library(leaflet)
library(tidyverse)
library(ggmap)
library(rgdal)
library(plyr)


# This begins the app
server <- function(input, output) {
    ############# setting the working directory and loading state info. dataset
    setwd("C:/Users/hussainsarfraz/Desktop/DATA 410/Week 1 - 410")
    US_map_data <- read_csv('place_data.csv')
    
    US_map_data$Risk_category <- US_map_data$Risk
    US_map_data$Risk_category[US_map_data$Risk_category<=24]  <- 1
    US_map_data$Risk_category[US_map_data$Risk_category>24 & US_map_data$Risk_category<=49] <- 2
    US_map_data$Risk_category[US_map_data$Risk_category>49 & US_map_data$Risk_category<=74] <- 3
    US_map_data$Risk_category[US_map_data$Risk_category>74 & US_map_data$Risk_category<=98] <- 4
    
    
    ############# Reading in the shapefiles shared with me (from census website)
    states <- readOGR("Zipcodes_Poly", layer = "Zipcodes_Poly", encoding = "UTF-8")
    names(states@data)
    head(states@data)
    
    ############# Replacing the excel zip codes with shapefile zip codes 
    states@data <- data.frame(states@data, US_map_data[match(states@data$CODE, US_map_data$CODE),])
    
    # ############# Loading in my API key from google
    # ## API Key: AIzaSyDkQBJxfs7ZZCSq1bAdGeBfls67p4e-yoQ
    # register_google(key = "AIzaSyDkQBJxfs7ZZCSq1bAdGeBfls67p4e-yoQ")
    # 
    # ############# Assigning API info. to 'objectMap' Code and adding lat and lon to excel file
    # datageo<- geocode(as.character(US_map_data$CODE), source = "google")
    # US_map_data$lat<-datageo$lat
    # US_map_data$lon<-datageo$lon
    
    
    # We're going to make our pop up now, and let it hang out in this 
    # state_popup object
    # Notice that HTML code makes our titles bold and have carriage returns
    state_popup <- paste0("<strong>Risk Index: </strong>",
                          states@data$Risk,"%",
                          '<br>',
                          "<strong>Party: </strong>", 
                          states@data$Poverty,"%", 
                          '<br>',
                          "<strong>Education: </strong>", 
                          states@data$Education,"%",
                          '<br>',
                          "<strong>Unemployment: </strong>",
                          states@data$Unemployment,"%",
                          '<br>',
                          "<strong>Crime: </strong>",
                          states@data$Crime,"%",
                          '<br>',
                          "<strong>ACEs: </strong>",
                          states@data$ACEs,"%")
    
    states@data$Risk_category <- as.factor(states@data$Risk_category)
    class(states@data$Risk_category)
    
    # Choosing a color scheme
    factpal <- colorFactor(c("#ffdc87", "#ff9639","#ef404c","#d30020"),
                           states@data$Risk_category)
    
    
    # factpal(c(1,2,3,4))
    # factpal(c(states@data$Risk<24,
    #           states@data$Risk>24 & states@data$Risk<49,
    #           states@data$Risk>50 & states@data$Risk<74,
    #           states@data$Risk>75 & states@data$Risk<98))
    
    # Here's our map!
    map<-leaflet(states) %>% 
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(stroke = TRUE, 
                    smoothFactor = 0.2, 
                    fillOpacity = .8,
                    color = 'black',
                    fillColor = ~factpal(Risk_category),
                    weight = 1, 
                    popup = state_popup) %>%
        addLegend("bottomright", 
                  colors =c(c("#ffdc87", "#ff9639","#ef404c","#d30020")),
                  labels= c("0-24", "25-49" , "50-74" , "75-98"),  
                  title= "Risk: Lowest to Highest",
                  opacity = .8) 
    
    # output the map
    output$map <- renderLeaflet(map)}
        


# Define UI for application that draws a histogram
ui <- fluidPage(

            headerPanel("Mapping of Risk Index and other variables for specific Counties in Philadelphia"),
            tags$style("#text1{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"),
            
            br(),
            leafletOutput("map", width = "100%", height = "800px")
            )


# Run the application 
shinyApp(ui = ui, server = server)
