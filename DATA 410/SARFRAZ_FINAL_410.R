library(tidyverse) 
library(rgdal)
library(leaflet)
library(ggmap)
library(sp)

setwd("C:/Users/hussainsarfraz/Desktop/DATA 410/SARFRAZ_410_FINAL_TEST")
lea.dataset <- read.csv("LEA_data.csv", header = TRUE)
philly.dataset <- read.csv("Philly_schools (1).csv", header = TRUE, sep=",")

#lea.dataset empty column removal
unique(lea.dataset$spp_attendance_rate)
lea.dataset$spp_attendance_rate <- NULL

unique(lea.dataset$pssa_prof_reading_total_all)
match('pssa_prof_reading_total_all', names(lea.dataset))

unique(lea.dataset[,47])
unique(lea.dataset[,48])
unique(lea.dataset[,49])
unique(lea.dataset[,50])
unique(lea.dataset[,51])
unique(lea.dataset[,52])
unique(lea.dataset[,53])
unique(lea.dataset[,54])

lea.dataset[ , 47:54] <- NULL

#philly.dataset empty column removal
unique(philly.dataset$ZIP_PLUS_4)
philly.dataset$ZIP_PLUS_4 <- NULL

#replacing link blanks with NA
# unique(philly.dataset$HPADDR)
# philly.dataset$HPADDR[philly.dataset$HPADDR == ''] <- NA

########################################
###### MAP-PHILLY ZIPCODES (NOT COUNTY LEVEL)
########################################


############# Reading in the shapefiles shared with me (from census website)
philly <- readOGR("Zipcodes_Poly", layer = "Zipcodes_Poly", encoding = "UTF-8")
names(philly@data)
head(philly@data)

############# Replacing the excel zip codes with shapefile zip codes 
philly@data <- data.frame(philly@data, philly.dataset[match(philly@data$CODE, philly.dataset$SCHOOL_ZIP),])

############ Loading in my API key from google
# API Key: AIzaSyDkQBJxfs7ZZCSq1bAdGeBfls67p4e-yoQ
# register_google(key = "AIzaSyDkQBJxfs7ZZCSq1bAdGeBfls67p4e-yoQ")
# ########### Assigning API info. to 'objectMap' Code and adding lat and lon to excel file
# philly.dataset$address.city.state.zip <- paste(philly.dataset$ADDRESS,philly.dataset$CITY,philly.dataset$STATE_CD,philly.dataset$SCHOOL_ZIP)
# datageo<- geocode(as.character(philly.dataset$address.city.state.zip), source = "google")
philly.dataset$lat <- datageo$lat
philly.dataset$lon<-datageo$lon


# We're going to make our pop up now, and let it hang out in this 
# state_popup object
# Notice that HTML code makes our titles bold and have carriage returns
state_popup <- paste0("<strong>Zipcode: </strong>",philly.dataset$SCHOOL_ZIP)

zipcode_popup <- paste0(
  "<strong>School Name: </strong>",
  philly.dataset$SCHOOL_NAME_1,
  '<br>',
  "<strong>Address: </strong>",
  philly.dataset$ADDRESS,
  '<br>',
  "<strong>Phone Number: </strong>",
  philly.dataset$PHONE_NUMBER,
  '<br>',
  "<strong>Website Link: </strong>",
  "<a href='",philly.dataset$HPADDR,"'>",philly.dataset$HPADDR,"</a>",
  '<br>',
  "<strong>Major Race Category: </strong>",
  philly.dataset$race_category,
  '<br>',
  "<strong>Total Suspensions: </strong>",
  philly.dataset$Total_suspensions,
  '<br>',
  "<strong>Major Crime: </strong>",
  philly.dataset$crime_category)


##################################
##### CATEGORIZING VARIABLES - AVERAGE TEACHER SALARY
summary(philly.dataset$Average_salary)
philly.dataset$Average_salary_category <- philly.dataset$Average_salary

philly.dataset$Average_salary_category[philly.dataset$Average_salary_category<=52200] <- 1
philly.dataset$Average_salary_category[philly.dataset$Average_salary_category>52200 &
                                         philly.dataset$Average_salary_category<156600] <- 2

factpal <- colorFactor(c("#ff0000", "#ff9900"),
                       philly.dataset$Average_salary_category)

##################################
##### CATEGORIZING VARIABLES - RACE

summary(philly.dataset$African_American)
summary(philly.dataset$White)
summary(philly.dataset$Asian)
summary(philly.dataset$Latino)
summary(philly.dataset$Other)

philly.dataset <- philly.dataset %>%
  mutate( 
    race_category = ifelse((African_American>White & African_American>Asian & African_American>Latino & African_American>Other),'B','O'),
    race_category = ifelse((White>African_American & White>Asian & White>Latino & White>Other),'W',race_category),
    race_category = ifelse((Asian>African_American & Asian>White & Asian>Latino & Asian>Other),'A',race_category),
    race_category = ifelse((Latino>African_American & Latino>White & Latino>Asian & Latino>Other),'L',race_category),
    race_category = ifelse((Other>African_American & Other>White & Other>Asian & Other>Latino),'O',race_category)
  ) 


race.factpal <- colorFactor(c("#fff203",'#664c40',"#ff9203",'#ffe2c6'),
                       philly.dataset$race_category)

##################################
##### TOTAL SUSPENSIONS

summary(philly.dataset$Total_suspensions)

philly.dataset$Total_suspensions_category <- philly.dataset$Total_suspensions

philly.dataset$Total_suspensions_category[philly.dataset$Total_suspensions_category<=166.5] <- 1
philly.dataset$Total_suspensions_category[philly.dataset$Total_suspensions_category>166.5 &
                                            philly.dataset$Total_suspensions_category<=333.5] <- 2
philly.dataset$Total_suspensions_category[philly.dataset$Total_suspensions_category>333.5 &
                                            philly.dataset$Total_suspensions_category<=500.25] <- 3
philly.dataset$Total_suspensions_category[philly.dataset$Total_suspensions_category>500.25 &
                                            philly.dataset$Total_suspensions_category<=667] <- 4

suspension.factpal <- colorFactor(c("#2f5c39",'#81f099',"#e87d86",'#bd0918'),
                            philly.dataset$Total_suspensions_category)

##################################
##### MAJORITY DRUG CATEGORIZATION

summary(philly.dataset$Drugs)
summary(philly.dataset$Morals)
summary(philly.dataset$Assaults)
summary(philly.dataset$Weapons)
summary(philly.dataset$Thefts)

philly.dataset <- philly.dataset %>%
  mutate( 
    crime_category = ifelse((Drugs>Morals & Drugs>Assaults & Drugs>Weapons & Drugs>Thefts),'D','T'),
    crime_category = ifelse((Morals>Drugs & Morals>Assaults & Morals>Weapons & Morals>Thefts),'M',crime_category),
    crime_category = ifelse((Assaults>Drugs & Assaults>Morals & Assaults>Weapons & Assaults>Thefts),'A',crime_category),
    crime_category = ifelse((Weapons>Drugs & Weapons>Morals & Weapons>Assaults & Weapons>Thefts),'W',crime_category),
    crime_category = ifelse((Thefts>Drugs & Thefts>Morals & Thefts>Assaults & Thefts>Weapons),'T',crime_category)
  )
table(philly.dataset$crime_category)
crime.factpal <- colorFactor(c("#ff0d11",'#7ddde8',"#5c5757",'#64ff1c'),
                                  philly.dataset$crime_category)

##################################
##### ATTENDANCE

summary(philly.dataset$Attendance)

attendance.factpal <- scale_fill_gradient2(low="#ff3a1c",high='#64ff1c',
                             philly.dataset$Attendance,
                             values = c(0,50,100))
#cale_fill_gradient2( high = "blue",mid = "purple", low = "red", midpoint = 50)
# Here's our map!
leaflet(philly) %>% 
  addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke = TRUE,
              smoothFactor = 0.2,
              fillOpacity = .8,
              color = '#D3D3D3',
              weight = 1,
              popup = state_popup
  ) %>%
  addPolylines(color = "black", opacity = 1, weight = 1) %>%
  addCircleMarkers(~lon, ~lat, 
                   data = philly.dataset, 
                   color = "black", 
                   radius = 3, 
                   fillOpacity = .9,
                   weight = 1,
                   popup = zipcode_popup,
                   fillColor = ~attendance.factpal(Attendance)) %>%
  addLegend("bottomright", 
            pal = attendance.factpal,
            title= "Attendence Rate",
            values = philly.dataset$Attendance,
            opacity = .8)

##################################
##### LOW INCOME FAMILIES

summary(philly.dataset$Low_income_family)

attendance.factpal <- scale_fill_gradient2(low="#ff3a1c",high='#64ff1c',
                                           philly.dataset$Attendance,
                                           values = c(0,50,100))
#cale_fill_gradient2( high = "blue",mid = "purple", low = "red", midpoint = 50)
# Here's our map!
leaflet(philly) %>% 
  addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke = TRUE,
              smoothFactor = 0.2,
              fillOpacity = .8,
              color = '#D3D3D3',
              weight = 1,
              popup = state_popup
  ) %>%
  addPolylines(color = "black", opacity = 1, weight = 1) %>%
  addCircleMarkers(~lon, ~lat, 
                   data = philly.dataset, 
                   color = "black", 
                   radius = 3, 
                   fillOpacity = .9,
                   weight = 1,
                   popup = zipcode_popup,
                   fillColor = ~attendance.factpal(Attendance)) %>%
  addLegend("bottomright", 
            pal = attendance.factpal,
            title= "Attendence Rate",
            values = philly.dataset$Attendance,
            opacity = .8)

########################################
###### MAIN VARIABLE DESCRIPTION
########################################
transmute(philly.dataset,Attendance )
philly.dataset %>%
  transmute(Attendance,
            African_American,
            White,
            Asian,
            Latino,
            Other,
            Total_suspensions,
            Average_salary,
            Drugs,
            Morals,
            Assaults,
            Weapons,
            Thefts,
            Low_income_family) %>%
  summary()
# philly.dataset$African_American
# philly.dataset$White
# philly.dataset$Asian
# philly.dataset$Latino
# philly.dataset$Other
# philly.dataset$Total_suspensions
# philly.dataset$Average_salary
# philly.dataset$Drugs
# philly.dataset$Morals
# philly.dataset$Assaults
# philly.dataset$Weapons
# philly.dataset$Thefts
philly.dataset$Low_income_family
philly.dataset$Teacher_attendance
stargazer(subset(philly.dataset,
                 select = c(Attendance,
                   African_American,
                            White,
                            Asian,
                            Latino,
                            Other,
                            Total_suspensions,
                            Average_salary,
                            Drugs,
                            Morals,
                            Assaults,
                            Weapons,
                   Thefts,
                   Low_income_family)),
          type = 'latex',
          title = "Summary Statistic of Variables",
          covariate.labels = c('Attendance Rates',
            'African American Students',
                               'White Students',
                               'Asian Students',
                               'Latino Students',
                               'Students of other races',
                               'Number of Total Suspensions',
                               'Average Teacher Salary',
                               'Drug-related crimes',
                               'Moral misconduct-related crimes',
                               'Assault-related crimes',
                               'Weapon-related crimes',
                               'Theft-related crimes',
                               'Percantage of Low-income families attending school'))


########################################
###### DIFFERENCE IN MEANS
########################################

########################################
###### REGRESSION + MULTIVARIATE
########################################

########################################
###### CORRELATION (EXTRA)
########################################
philly.dataset$African_American
philly.dataset$Total_suspensions
philly.dataset$Average_salary
philly.dataset$Assaults
philly.dataset$Drugs

ggplot(data=philly.dataset,aes(x = African_American, y = Attendance)) +
  geom_point() + 
  geom_smooth(method=lm, formula = y ~ poly(x, 1), se = FALSE) 

library(ggplot2)
ggplot(data=philly.dataset,aes(x = African_American, y = Attendance)) +
  geom_point() + 
  xlab("Number of enrolled African American Students") +
  ylab("Attendance Rate") +
  ggtitle("Relationship between Attendance Rate and African American students enrollment") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    plot.title = element_text(size = 10,
                              hjust = 0.5)) +
  geom_smooth(method=lm, formula = y ~ poly(x, 1), se = FALSE) 


ggplot(data=philly.dataset,aes(x = Total_suspensions, y = Attendance)) +
  geom_point() + 
  geom_smooth(method=lm, formula = y ~ poly(x, 1), se = FALSE)

ggplot(data=philly.dataset,aes(x = Average_salary, y = Attendance)) +
  geom_point() + 
  geom_smooth(method=lm, formula = y ~ poly(x, 1), se = FALSE)

ggplot(data=philly.dataset,aes(x = Drugs, y = Attendance)) +
  geom_point() + 
  geom_smooth(method=lm, formula = y ~ poly(x, 1), se = FALSE)





