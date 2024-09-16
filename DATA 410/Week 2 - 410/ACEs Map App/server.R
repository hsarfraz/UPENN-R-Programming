#-----------------------------------#
# Server Script                     #                           
#                                   #
#-----------------------------------#

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#------------------------------------
# Beginning Shiny Code
#------------------------------------

# This line begins the shiny app
shinyServer(function(input, output) {
  
  #-------------------------
  # Maps
  #-------------------------
  
  # Use these coordinates to center the map

  
  #reading in data 
  acestate<-read.csv("test3.csv")
  
  #prepping the alt variable
  acestate$alt<- (100)- acestate$zed
  
  #reading  in the shapefile
  states <- readOGR("cb_2016_us_state_500k", layer = "cb_2016_us_state_500k",encoding = "UTF-8")
  
  # using a left_join 
  states@data <- left_join(states@data,acestate, by = c("STUSPS" = "Official.USPS.Code"))
  
  # make ACE variable a factor
  states$ACE <- factor(states$ACE)
  
  # writing the popup values
  state_popup <- paste0("<strong>State: </strong>", 
                        states$NAME, 
                        "<br>National 1 or more ACEs: 46%", 
                        "<br>State 1 or more ACEs: ",
                        states$alt,
                        "%",
                        "<br>State 3 or more ACEs: ", 
                        states$threemore,
                        "%")
  
  # coloring by factor level, from ACE variable
  factpal <- colorFactor(c("#f0f9e8", 
                           "#bae4bc",
                           "#7bccc4",
                           "#43a2ca"), states$ACE)
  # leaflet map works well with shiny
  # Notice I am putting this in "map" object
  map<-leaflet(states) %>%fitBounds(-124, 34, -62, 40)%>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = .8,
                color = ~factpal(ACE),
                weight = 1, 
                popup = state_popup) %>%
    addLegend("topright", 
              colors =c("#f0f9e8", "#bae4bc","#7bccc4", "#43a2ca"),
              labels= c("Significantly Lower", "Lower","Higher","Significantly Higher"),  
              title= "Difference in Rate of 1 or More ACEs from National Average",
              opacity = 1) 
  
  # output the map
  output$map <- renderLeaflet(map)
  
  
  
  
}) 
