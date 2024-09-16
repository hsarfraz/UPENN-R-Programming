#-----------------------------------#
# Maps I
#-----------------------------------#
install.packages(c("rgdal", "leaflet"))
library(tidyverse) 
library(rgdal)
library(leaflet)


#-----------------------------------#
# Interactive maps
#-----------------------------------#
# We'll use the leaflet package to build
# interactive maps

# First, let's bring up the default leaflet map
leaflet() %>% 
  addTiles()

# Using leaflet we can get some pretty views of the location
# we're interested in
leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng = -80.3050016, lat = 40.4340982)

# There are lots of map background options, to change the default map
# use addProviderTiles() and pull in the map of your choice
leaflet() %>% 
  addProviderTiles("Stamen.Watercolor") %>% 
  addMarkers(lng = -80.3050016, lat = 40.4340982)

# You can also change what your markers look like by using a different
# marker function
leaflet() %>% 
  addProviderTiles("Stamen.Toner") %>% 
  addCircles(lng = -80.3050016, lat = 40.4340982, radius = 350, color = "orange")

# Now let's try bringing in some data and mapping it
# The dataset contains the location of the birthplace of every US president
# so far
birthplace <- read.csv("birthplace.csv")
head(birthplace)

# We'll need to change the input of the addMarkers function slightly to 
# accommodate a vector
leaflet() %>% 
  addTiles() %>% 
  addMarkers(~Longitude, ~Latitude, data = birthplace)

# One of the coolest parts of leaflet is using the popup option
# We can display data from our dataset easily
leaflet() %>% 
  addTiles() %>% 
  addMarkers(~Longitude, ~Latitude, data = birthplace, popup = birthplace$President)


# The popup option can take vector data or character information. It also
# can read HTML code to format the popup nicely
leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(~Longitude, ~Latitude, data = birthplace, 
                   popup = paste("President:", birthplace$President, "<br>",
                                 "Birth Place:", birthplace$Town))

# There are a number of arguments that allow you to edit how your markers
# look, some of the most common are color, radius, and weight of the boundry line. 
leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(~Longitude, ~Latitude, 
                   data = birthplace, 
                   color = "purple", 
                   radius = 9, 
                   weight = 1,
                   popup = paste("President:", birthplace$President, "<br>",
                                 "Birth Place:", birthplace$Town))

# We can also make the circle markers opaque
leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(~Longitude, ~Latitude, 
                   data = birthplace, 
                   color = "purple", 
                   radius = 9, 
                   weight = 1,
                   fillOpacity = TRUE,
                   popup = paste("President:", birthplace$President, "<br>",
                                 "Birth Place:", birthplace$Town))








#----

leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(~Longitude, ~Latitude, 
                   data = birthplace, 
                   color = "purple", 
                   radius = 9, 
                   weight = 1,
                   fillOpacity = TRUE,
                   popup = paste("President:", birthplace$President, "<br>",
                                 "Birth Place:", birthplace$Town, "<br>",
                                 "Affiliation:", birthplace$Party))






# Let's try using color meaningfully

# First, we need to define which colors we want and map them
# to the data
pal <- colorFactor("Dark2", birthplace$Party)

leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(~Longitude, ~Latitude, 
                   data = birthplace, 
                   fillColor = ~pal(Party), 
                   radius = 9, 
                   weight = 1,
                   stroke = TRUE,
                   color = "navy",
                   fillOpacity = TRUE,
                   popup = paste("President:", birthplace$President, "<br>",
                                 "Birth Place:", birthplace$Town))







# But wait, there's more!
leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(~Longitude, ~Latitude, 
                   data = birthplace, 
                   fillColor = ~pal(Party), 
                   radius = 9, 
                   weight = 1,
                   stroke = TRUE,
                   color = "navy",
                   fillOpacity = TRUE,
                   popup = paste("President:", birthplace$President, "<br>",
                                 "Birth Place:", birthplace$Town))%>%
  addLegend("bottomright", 
            pal = pal,
            values = levels(birthplace$Party),
            title= "Party Affiliation",
            opacity = 1) 



#--------------------------------#
# Interactive Choropleths
#--------------------------------#


# We're going to make a map of the 2016 electorate
# This simple dataset records the outcome of the election by state
election<-read.csv("2016 Electoral Votes.csv")
head(election)

# Read in your shapefile, in this instance we are using
# state polygons downloaded from the census website
states <- readOGR("cb_2016_us_state_500k", layer = "cb_2016_us_state_500k", encoding = "UTF-8")
names(states@data)

head(states@data)

# Unfortunately, merge functions don't always
# play nicely with spacial objects 

states@data <- data.frame(states@data, election[match(states@data$NAME, election$State),])


# We're going to make our pop up now, and let it hang out in this 
# state_popup object
# Notice that HTML code makes our titles bold and have carriage returns
state_popup <- paste0("<strong>Party: </strong>", 
                      states@data$Winning.Party, 
                      "<br>State:", 
                      states@data$NAME)

# Choosing a color scheme
factpal <- colorFactor(c("#0000FF", "#FF0000"),
                       states@data$Winning.Party)

# Here's our map!
leaflet(states) %>% fitBounds(-124, 34, -62, 40) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke = TRUE, 
              smoothFactor = 0.2, 
              fillOpacity = .8,
              color = ~factpal(Winning.Party),
              weight = 1, 
              popup = state_popup) %>%
  addLegend("bottomright", 
            colors =c("#0000FF", "#FF0000"),
            labels= c("Democratic Candidate", "Republican Candidate"),  
            title= "Winning Party",
            opacity = 1) 

