library(ggmap)


# Paste your API key into the fucntion below and run it. Now you should be all set!

register_google(key = "AIzaSyDkQBJxfs7ZZCSq1bAdGeBfls67p4e-yoQ")

# We'll start by just geocoding one singular address
geocode("3814 Walnut St, Philadelphia PA", 
        output = "more", 
        source ="google")

# Let's see how we can geocode multiple locations at once! I am going to 
# use the same birthplace dataset from the leaflet lecture

birthplace <- read.csv("birthplace.csv")

head(birthplace)

birthplace$Latitude<-NULL
birthplace$Longitude<-NULL

head(birthplace)

datageo<- geocode(as.character(birthplace$Full.Address), source = "google")
birthplace$lat<-datageo$lat
birthplace$lon<-datageo$lon

# Check for any odd lat/lons 
stem(birthplace$lat)
stem(birthplace$lon)


# To reverse geocode, you'll use the revgeocode function from ggmap
revgeocode(location = c(-75.1634789,  39.9528479), output = "address")
