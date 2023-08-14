#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinythemes)
library(shiny)
library(rsconnect)
library(mfx)
library(ggplot2)
library(survey)
library(foreign)
library(ggplot2)
library(stargazer)
library(rgdal)
library(plyr)
library(leaflet)
library(dplyr)
library(htmltools)
library(tidyverse)
library(ggmap)
library(rsconnect)

############################################################################################################
##################################### UI CODE 
############################################################################################################
ui <- fluidPage(theme = shinytheme("flatly"),
                ####################################
                #NAVIGATION BAR
                ####################################
                navbarPage("Philly Schools App", #this is the title for our navigation bar
                           
                           ####################################
                           #FIRST PAGE - MAPPING
                           ####################################
                           tabPanel("Introduction - Maps",
                                    
                                    #color for all titles
                                    tags$head(
                                      tags$style("h2 {color: #ee5500; }
                                               h1 {color: #177ba6}; } ")),
                                    
                                    #first title
                                    headerPanel("Introduction/Brief Overview"),
                                    p('This website will be investigating the underlying factors as to why certain schools have higher attendance rates. Factors such as race, crime, and school funding will be compared with attendance rates to see if there is a correlation between any of the variables. The map below is giving a general overview of how the data looks visually.'),
                                    br(),
                                    p(strong('Note:'),'The map visualisations were created to give a rough illustration of how the data might look geographically. No substantive conclusions should be made by solely looking at the maps since most of the variables were categorised into groups or the variables displayed were the ones that occurred the most (for example, one map displays the most crimes that occured in different schools)'),
                                    headerPanel( "Philly Schools Maps"),
                                    
                                    ####################################
                                    # LEFT SIDEBAR
                                    ####################################
                                    # sidebarLayout(position = "left",
                                    #               sidebarPanel(
                                    #                 h4("Zip Code of Interest:"),
                                    #                 selectInput("map_zip", "Zip Code of Interest",
                                    #                             choices = philly.dataset$SCHOOL_ZIP,
                                    #                             selected = '19101'
                                    #                             )
                                    #             
                                    #               ),
                                    
                                    ####################################
                                    # TABS
                                    ####################################
                                    #mainPanel(
                                    br(),
                                    tabsetPanel(type = "tabs",
                                                
                                                #####################################
                                                # first tab
                                                #####################################
                                                tabPanel("Map of Philly Schools Attendance Rate",
                                                         fluidRow(
                                                           leafletOutput("attendance.map", width = "100%", height = "400px"),
                                                           #leafletOutput("map", width = "100%", height = "400px")
                                                           p( strong('Note:'), " The attendance rate midpoint is 95%, meaning anything between a 95-100% attendance rate would be the ideal.",
                                                             a("You can click on this link to learn more about the standard attendance cutoff rate amoung U.S schools", 
                                                               href = "https://www.theschoolrun.com/parents-guide-school-attendance#:~:text=An%20attendance%20rate%20of%2095,rate%20of%2090%25%20or%20below."))
                                                         )  
                                                ),#'Map of Philly Schools' end
                                                tabPanel("Map of Average Teacher Salary by School",
                                                         fluidRow(
                                                           leafletOutput("teacher.income.map", width = "100%", height = "400px"),
                                                           p(strong('Note:')," Information about specifc income levels was taken from U.S news.",
                                                             a("You can click on the link here to read the article used for reference", 
                                                               href = "https://money.usnews.com/money/personal-finance/family-finance/articles/where-do-i-fall-in-the-american-economic-class-system"))
                                                         
                                                           ) 
                                                ), #'Map of Average Teacher Salary by School' end  
                                                tabPanel("Map of Majority Race by School",
                                                         fluidRow(
                                                           leafletOutput("race.map", width = "100%", height = "400px")
                                                         ) 
                                                ), ##'Map of Majority Race by School' end 
                                                tabPanel("Percentage of total suspensions by school",
                                                         fluidRow(
                                                           leafletOutput("suspension.map", width = "100%", height = "400px")
                                                         )
                                                ),
                                                tabPanel("Map of Major Crimes by school",
                                                         fluidRow(
                                                           leafletOutput("crime.map", width = "100%", height = "400px")
                                                         )
                                                ),
                                                tabPanel("Map of low-income family attendance",
                                                         fluidRow(
                                                           leafletOutput("income.map", width = "100%", height = "400px")
                                                         )
                                                )
                                    )#all tab panel end
                                    
                                    #)#main tab panel end
                                    #)#sidebar tab panel end
                                    
                           ),#page one tab end 
                           
                           ####################################
                           #SECOND PAGE - MAIN VARIABLE DESCRIPTION
                           ####################################
                           tabPanel("Description of Main Variables",
                                    
                                    #color for all titles
                                    tags$head(
                                      tags$style("h2 {color: #ee5500; }
                                               h1 {color: #177ba6}; } ")),
                                    
                                    #first title
                                    headerPanel( "Description of Variables used in this website"),
                                    
                                    ####################################
                                    # LEFT SIDEBAR
                                    ####################################
                                    sidebarLayout(position = "left",
                                                  
                                                  sidebarPanel(
                                                    selectInput("main_variables", "Variable",
                                                                choices = c('Attendance',
                                                                            'African_American',
                                                                            'White',
                                                                            'Asian',
                                                                            'Latino',
                                                                            'Other_Races',
                                                                            'Total_suspensions',
                                                                            'Average_salary',
                                                                            'Drugs',
                                                                            'Morals',
                                                                            'Assaults',
                                                                            'Weapons',
                                                                            'Thefts',
                                                                            'Low_income_family'),
                                                                
                                                    )
                                                  ),
                                                  
                                                  ####################################
                                                  # TABS
                                                  ####################################
                                                  mainPanel(
                                                    br(),
                                                    tabsetPanel(type = "tabs",
                                                                
                                                                #####################################
                                                                # first tab
                                                                #####################################
                                                                tabPanel("Variable Summary",
                                                                         fluidRow(
                                                                           verbatimTextOutput("main.variable.des")
                                                                         )  
                                                                )
                                                                
                                                    )#all tab panel end
                                                    
                                                  )#main tab panel end
                                    )#sidebar tab panel end
                                    
                           ), #SECOND PAGE END
                           
                           ####################################
                           #THIRD PAGE - REGRESSION
                           ####################################
                           
                           #navigation bar title
                           tabPanel("Regression",
                                    
                                    #color for all titles
                                    tags$head(
                                      tags$style("h2 {color: #ee5500; }
                                               h1 {color: #177ba6}; } ")),
                                    
                                    #first title
                                    headerPanel( "Risk Prediction Model"),
                                    
                                    ####################################
                                    # RIGHT SIDEBAR
                                    ####################################
                                    sidebarLayout(position = "right",
                                                  sidebarPanel(
                                                    h2("Build your model"),
                                                    br(),
                                                    checkboxGroupInput("iv1", 
                                                                       label = "Select any of the independent variables below to calculate your model. You can change your selection at any time.", 
                                                                       list("African American Students"='African_American',
                                                                            "White Students"='White',
                                                                            "Asian Students"='Asian',
                                                                            "Latino Students"='Latino',
                                                                            "Students of other races"='Other',
                                                                            "Total number of school suspensions"='Total_suspensions',
                                                                            "Average Teacher Salary"='Average_salary',
                                                                            "Drug usage"='Drugs',
                                                                            "Moral mis-conduct"='Morals',
                                                                            "Number of Assaults-related crimes"='Assaults',
                                                                            "Number of Weapons-related crimes"='Weapons',
                                                                            "Number of thefts"='Thefts',
                                                                            "Percentage of Students that come from a low-income family" = 'Low_income_family'
                                                                       ), #text ~ variable name
                                                                       selected="African_American" #Note African_American is selected when the app starts 
                                                                       
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
                                                    )
                                                  ),
                                                  
                                                  
                                                  ####################################
                                                  # TABS 
                                                  ####################################
                                                  mainPanel(
                                                    br(),
                                                    tabsetPanel(type = "tabs", 
                                                                ####################################
                                                                #first tab shows regression table
                                                                ####################################
                                                                tabPanel("Regression Table",
                                                                         h3("Table of Regression Coefficients"),
                                                                         HTML('</br>'),
                                                                         tableOutput("regTab"),
                                                                         HTML('</br>'),
                                                                         helpText("The table displays the coefficients of your model in the log-odds scale. Larger numbers indicate that a variable has a greater effect. A positive number indicates that a variable increases the likelihood of Risk while a negative indicates that a variable decreases the likelihood of Risk. The P value determines statistical significance. P values below 0.05 are commonly accepted as significant."))
                                                    )#first tab panel end
                                                    
                                                  )#main tab panel end
                                    )#right sidebar end
                           ),#'week 3 -regression' nav. bar end
                           
                           ####################################
                           #FOURTH PAGE - CORRELATION
                           ####################################
                           tabPanel("Correlation Plots",
                                    
                                    #color for all titles
                                    tags$head(
                                      tags$style("h2 {color: #ee5500; }
                                               h1 {color: #177ba6}; } ")),
                                    
                                    #first title
                                    headerPanel("Correlation Plots"),
                                    
                                    ####################################
                                    # LEFT SIDEBAR
                                    ####################################
                                    sidebarLayout(position = "left",
                                                  sidebarPanel(
                                                    h4("Pick a independent variable:"),
                                                    selectInput("independent_variables", "independent variable",
                                                                choices = c('African_American',
                                                                            'White',
                                                                            'Asian',
                                                                            'Latino',
                                                                            'Other_Races',
                                                                            'Total_suspensions',
                                                                            'Average_salary',
                                                                            'Drugs',
                                                                            'Morals',
                                                                            'Assaults',
                                                                            'Weapons',
                                                                            'Thefts',
                                                                            'Low_income_family'),
                                                                selected = '19101'
                                                    )
                                                    
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
                                                    # philly.dataset$Low_income_family
                                                    
                                                  ),
                                                  
                                                  ####################################
                                                  # TABS
                                                  ####################################
                                                  mainPanel(
                                                    br(),
                                                    tabsetPanel(type = "tabs",
                                                                
                                                                #####################################
                                                                # first tab
                                                                #####################################
                                                                tabPanel("Map of Philly Schools Attendance Rate",
                                                                         fluidRow(
                                                                           plotOutput('correlation.plot')
                                                                         )  
                                                                ),#'Map of Philly Schools' end
                                                                
                                                    )#all tab panel end
                                                    
                                                  )#main tab panel end
                                    )#sidebar tab panel end
                                    
                           ),
                           ####################################
                           #LAST PAGE - DATASET DISPLAY
                           ####################################
                           tabPanel("Dataset Display",
                                    #color for all titles
                                    tags$head(
                                      tags$style("h2 {color: #ee5500; }
                                               h1 {color: #177ba6}; } ")
                                    ),
                                    headerPanel( "Philly School Dataset used in this Analysis"),
                                    dataTableOutput('philly.dataset.display')
                           )#LAST PAGE - DATASET DISPLAY END 
                           
                           
                           
                )#navigation bar function end
)

############################################################################################################
##################################### SERVER CODE 
############################################################################################################
server <- function(input, output) {
  
  lea.dataset <- read.csv("LEA_data.csv", header = TRUE)
  philly.dataset <- read.csv("Philly_schools (1).csv", header = TRUE, sep=",")
  
  ####################################
  # FIRST PAGE - MAPPING
  ####################################  
  ##################################
  ##### SHAPEFILES
  philly <- readOGR("Zipcodes_Poly", layer = "Zipcodes_Poly", encoding = "UTF-8")#Reading in the shapefiles shared with me (from census website)
  names(philly@data)
  head(philly@data)
  
  philly@data <- data.frame(philly@data, philly.dataset[match(philly@data$CODE, philly.dataset$SCHOOL_ZIP),])#Replacing the excel zip codes with shapefile zip codes
  
  ##################################
  ##### LOADING GOOGLE API AND CREATING GEO-MARKERS DATASET
  
  ## API Key: AIzaSyDkQBJxfs7ZZCSq1bAdGeBfls67p4e-yoQ
   register_google(key = "AIzaSyDkQBJxfs7ZZCSq1bAdGeBfls67p4e-yoQ")
   philly.dataset$address.city.state.zip <- paste(philly.dataset$ADDRESS,philly.dataset$CITY,philly.dataset$STATE_CD,philly.dataset$SCHOOL_ZIP) #this is to fix the address column (nothing related to google api)
   datageo<- geocode(as.character(philly.dataset$address.city.state.zip), source = "google")
  
  philly.dataset$lat <- datageo$lat
  philly.dataset$lon<-datageo$lon
  
  ##################################
  ##### POP-UPS
  philly.dataset <- philly.dataset %>%
    mutate( 
      race_category = ifelse((African_American>White & African_American>Asian & African_American>Latino & African_American>Other),'Black','O'),
      race_category = ifelse((White>African_American & White>Asian & White>Latino & White>Other),'White',race_category),
      race_category = ifelse((Asian>African_American & Asian>White & Asian>Latino & Asian>Other),'Asian',race_category),
      race_category = ifelse((Latino>African_American & Latino>White & Latino>Asian & Latino>Other),'Latino',race_category),
      race_category = ifelse((Other>African_American & Other>White & Other>Asian & Other>Latino),'Other',race_category)
    ) 
  
  philly.dataset <- philly.dataset %>%
    mutate( 
      crime_category = ifelse((Drugs>Morals & Drugs>Assaults & Drugs>Weapons & Drugs>Thefts),'Drugs','Theft'),
      crime_category = ifelse((Morals>Drugs & Morals>Assaults & Morals>Weapons & Morals>Thefts),'Morals',crime_category),
      crime_category = ifelse((Assaults>Drugs & Assaults>Morals & Assaults>Weapons & Assaults>Thefts),'Assult',crime_category),
      crime_category = ifelse((Weapons>Drugs & Weapons>Morals & Weapons>Assaults & Weapons>Thefts),'Weapons',crime_category),
      crime_category = ifelse((Thefts>Drugs & Thefts>Morals & Thefts>Assaults & Thefts>Weapons),'Theft',crime_category)
    )
  
  state_popup <- paste0("<strong>Zipcode: </strong>",philly@data$SCHOOL_ZIP)
  
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
    "<a href='",philly.dataset$HPADDR,"'>",philly@data$HPADDR,"</a>",
    '<br>',
    "<strong>Average Teacher Salary: </strong>",
    round(philly.dataset$Average_salary,digits=2),
    '<br>',
    "<strong>Majority Race Category: </strong>",
    philly.dataset$race_category,
    '<br>',
    "<strong>Total Suspension Percentage: </strong>",
    round(philly.dataset$Total_suspensions/667*100,digits=2),"%",
    '<br>',
    "<strong>Major Crime: </strong>",
    philly.dataset$crime_category,
    '<br>',
    "<strong>Attendance Rate: </strong>",
    philly.dataset$Attendance,"%",
    '<br>',
    "<strong>Percentage of students coming from low-income families: </strong>",
    philly.dataset$Low_income_family,"%")
  
  philly.dataset.filter <- reactive({
    subset(philly.dataset, SCHOOL_ZIP == input$map_zip)
  })
  
  ##################################
  ##### TEACHER INCOME MAP 
  philly.dataset$Average_salary_category <- philly.dataset$Average_salary
  
  philly.dataset$Average_salary_category[philly.dataset$Average_salary_category<=52200] <- 1
  philly.dataset$Average_salary_category[philly.dataset$Average_salary_category>52200 &
                                           philly.dataset$Average_salary_category<156600] <- 2
  factpal <- colorFactor(c("#ff0000", "#ff9900"),
                         philly.dataset$Average_salary_category)
  
  teacher.income.map <- leaflet(philly) %>% 
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
                     fillColor = ~factpal(Average_salary_category)) %>%
    addLegend("bottomright", 
              colors =c("#ff0000", "#ff9900"),
              labels= c("low income (less than $52,200)",
                        "middle income ($52,200-$156,600)"),  
              title= "Income Levels",
              opacity = .8)
  
  ##################################
  ##### RACE CATEGORIZATION MAP
  race.factpal <- colorFactor(c("#fff203",'#664c40',"#ff9203",'#ffe2c6'),
                              philly.dataset$race_category)
  # Here's our map!
  race.map <- leaflet(philly) %>% 
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
                     fillColor = ~race.factpal(race_category)) %>%
    addLegend("bottomright", 
              colors =c("#fff203",'#664c40',"#ff9203",'#ffe2c6'),
              labels= c("Asians",
                        "African Americans",
                        "Latino",
                        "White"),  
              title= "Race Categorization",
              opacity = .8)
  
  ##################################
  ##### TOTAL SUSPENSIONS CATEGORY
  
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
  # Here's our map!
  suspension.map <- leaflet(philly) %>% 
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
                     fillColor = ~suspension.factpal(Total_suspensions_category)) %>%
    addLegend("bottomright", 
              colors =c("#2f5c39",'#81f099',"#e87d86",'#bd0918'),
              labels= c("0-25%",
                        "26-50%",
                        "51-75%",
                        "76-100%"),  
              title= "Percentage of total suspensions",
              opacity = .8)
  
  ##################################
  ##### MAJORITY DRUG CATEGORIZATION
  crime.factpal <- colorFactor(c("#ff0d11",'#7ddde8',"#5c5757",'#64ff1c'),
                               philly.dataset$crime_category)
  # Here's our map!
  crime.map <- leaflet(philly) %>% 
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
                     fillColor = ~crime.factpal(crime_category)) %>%
    addLegend("bottomright", 
              colors =c("#ff0d11",'#7ddde8',"#5c5757",'#64ff1c'),
              labels= c("Assult",
                        "Drugs",
                        "Theft",
                        "Weapons"),  
              title= "Type of Crime",
              opacity = .8)
  
  
  ##################################
  ##### ATTENDANCE MAP
  
  ## Make vector of colors for values smaller than 0 (20 colors)
  rc1 <- colorRampPalette(colors = c("#bd1b02", "#f78877"), space = "Lab")(17.06)
  ## Make vector of colors for values larger than 0 (180 colors)
  rc2 <- colorRampPalette(colors = c("#6dcf6f", "#4d8c4e"), space = "Lab")(2.48)
  
  ## Combine the two color palettes
  rampcols <- c(rc1, rc2)
  
  attendance.factpal <- colorNumeric(palette = rampcols, domain = philly.dataset$Attendance)
  
  # Here's our map!
  attendance.map <-leaflet(philly) %>%
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
  
  ## Make vector of colors for values smaller than 0 (20 colors)
  rc1 <- colorRampPalette(colors = c("#13f50f", "#a4faa2"), space = "Lab")(24.76)
  ## Make vector of colors for values larger than 0 (180 colors)
  rc2 <- colorRampPalette(colors = c("#7a97f5", "#0a43fc"), space = "Lab")(24.76)
  
  ## Combine the two color palettes
  rampcols <- c(rc1, rc2)
  
  income.factpal <- colorNumeric(palette = rampcols, domain = philly.dataset$Low_income_family)
  
  # Here's our map!
  income.map <-leaflet(philly) %>%
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
                     fillColor = ~income.factpal(Low_income_family)) %>%
    addLegend("bottomright",
              pal = income.factpal,
              title= "Low-income student percentage",
              values = philly.dataset$Low_income_family,
              opacity = .8)
  
  
  ##################################
  ##### GENERAL MAP  
  map <- leaflet(philly) %>% 
    addTiles() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(stroke = TRUE,
                smoothFactor = 0.2,
                fillOpacity = .8,
                color = '#D3D3D3',
                weight = 1,
                popup = state_popup) %>%
    addPolylines(color = "black", opacity = 1, weight = 1) %>%
    addCircleMarkers(~lon, ~lat,
                     data = philly.dataset,
                     color = "black",
                     radius = 3,
                     fillOpacity = .9,
                     weight = 1,
                     popup = zipcode_popup)
  # Here's our map!
  # map <- renderLeaflet({
  #   leaflet(philly) %>% 
  #   addTiles() %>%
  #   addProviderTiles("CartoDB.Positron") %>%
  #   addPolygons(stroke = TRUE,
  #               smoothFactor = 0.2,
  #               fillOpacity = .8,
  #               color = '#D3D3D3',
  #               weight = 1
  #   ) %>%
  #   addPolylines(color = "black", opacity = 1, weight = 1) %>%
  #   addCircleMarkers(~lon, ~lat,
  #                    data = philly.dataset.filter(),
  #                    color = "black",
  #                    radius = 3,
  #                    fillOpacity = .9,
  #                    weight = 1,
  #                    popup = state_popup)
  # })
  
  # output the map
  output$map <- renderLeaflet(map)
  output$teacher.income.map <- renderLeaflet(teacher.income.map)
  output$race.map <- renderLeaflet(race.map)
  output$suspension.map <- renderLeaflet(suspension.map)
  output$crime.map <- renderLeaflet(crime.map)
  output$attendance.map <- renderLeaflet(attendance.map)
  output$income.map <- renderLeaflet(income.map)
  
  ####################################
  # SECOND PAGE - DESCRIPTION OF MAIN VARIABLES
  ####################################
  
  mainvar.filter <- reactive({
    
    
    transmute(philly.dataset,
              'Selected Variable:' = philly.dataset[[input$main_variables]])
  })
  
  
  output$main.variable.des <- renderPrint({
    
    summary(mainvar.filter())
    # philly.dataset %>%
    #   transmute( 
    #     Attendance,
    #     African_American,
    #     White,
    #     Asian,
    #     Latino,
    #     Other,
    #     Total_suspensions,
    #     Average_salary,
    #     Drugs,
    #     Morals,
    #     Assaults,
    #     Weapons,
    #     Thefts,
    #     Low_income_family
    #     ) %>%
    #   summary()
  })
  
  
  ####################################
  # THIRD PAGE - REGRESSION
  ####################################
  
  ####REGRESSION WITHOUT WEIGHTS (NO 'svydesign' OBJECT NEEDED HERE)
  model <- reactive({
    lm(paste("Attendance"," ~ ",paste(input$iv1,collapse="+")),family= quasibinomial, philly.dataset)
  })
  
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
  
  ####COVARIATE LABELS IN REGRESSION MODEL CODE
  covar.label <- reactive({
    covars<-character()
    if ('African_American' %in% input$iv1){
      covars <- c(covars,"1 unit change in African American students increases/decreases Attendance by:")
    }
    
    if ('White' %in% input$iv1){
      covars <- c(covars,"1 unit change in White students increases/decreases Attendance by:")
    }
    
    if ('Asian' %in% input$iv1){
      covars <- c(covars,"1 unit change in Asian students increases/decreases Attendance by:")
    }
    
    if ('Latino' %in% input$iv1){
      covars <- c(covars,"1 unit change in Latino students increases/decreases Attendance by:")
    }
    
    if ('Other' %in% input$iv1){
      covars <- c(covars,"1 unit change in students of other races increases/decreases Attendance by:")
    }
    
    if ('Total_suspensions' %in% input$iv1){
      covars <- c(covars,"1 unit change in school total suspensions increases/decreases Attendance by:")
    }
    
    if ('Average_salary' %in% input$iv1){
      covars <- c(covars,"1 unit change in teachers average salary increases/decreases Attendance by:")
    }
    
    if ('Drugs' %in% input$iv1){
      covars <- c(covars,"1 unit change in drug usage increases/decreases Attendance by:")
    }
    
    if ('Morals' %in% input$iv1){
      covars <- c(covars,"1 unit change in dis-orderly conduct increases/decreases Attendance by:")
    }
    
    if ('Assaults' %in% input$iv1){
      covars <- c(covars,"1 unit change in assult increases/decreases Attendance by:")
    }
    
    if ('Weapons' %in% input$iv1){
      covars <- c(covars,"1 unit change in weapon-related crimes increases/decreases Attendance by:")
    }
    
    if ('Thefts' %in% input$iv1){
      covars <- c(covars,"1 unit change in theft-related crimes increases/decreases Attendance by:")
    }
    
    if ('Low_income_family' %in% input$iv1){
      covars <- c(covars,"1 unit change in low-income family percentage increases/decreases Attendance by:")
    }
    return(covars)
  })
  
  ####REGRESION OUTPUT
  output$regTab <- renderText({
    covars <- covar.label()
    stargazer(model(),type="html",dep.var.labels ="Attendance",covariate.labels = covars,omit.stat = c("f","ser","aic","adj.rsq"))
  })
  
  
  ####################################
  # FOURTH PAGE CORRELATION
  ####################################
  # 'African_American',
  # 'White',
  # 'Asian',
  # 'Latino',
  # 'Other',
  # 'Total_suspensions',
  # 'Average_salary',
  # 'Drugs',
  # 'Morals',
  # 'Assaults',
  # 'Weapons',
  # 'Thefts'
  output$correlation.plot <- renderPlot({
    
    if (input$independent_variables == 'African_American') {
      ggplot(data=philly.dataset,aes(x = African_American, y = Attendance)) +
        geom_point() + 
        xlab("Number of enrolled African American Students") +
        ylab("Attendance Rate") +
        ggtitle("Relationship between Attendance Rate and African American students enrollment") +
        theme_bw() +
        theme(axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              plot.title = element_text(size = 12,hjust = 0.5)) +
        geom_smooth(method=lm, formula = y ~ poly(x, 1), se = FALSE)
      
    } else if (input$independent_variables == 'White') {
      ggplot(data=philly.dataset,aes(x = White, y = Attendance)) +
        geom_point() + 
        xlab("Number of enrolled White Students") +
        ylab("Attendance Rate") +
        ggtitle("Relationship between Attendance Rate and White students enrollment") +
        theme_bw() +
        theme(axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              plot.title = element_text(size = 12,hjust = 0.5)) +
        geom_smooth(method=lm, formula = y ~ poly(x, 1), se = FALSE)
      
    } else if (input$independent_variables == 'Asian') {
      ggplot(data=philly.dataset,aes(x = Asian, y = Attendance)) +
        geom_point() + 
        xlab("Number of enrolled Asian Students") +
        ylab("Attendance Rate") +
        ggtitle("Relationship between Attendance Rate and Asian students enrollment") +
        theme_bw() +
        theme(axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              plot.title = element_text(size = 12,hjust = 0.5)) +
        geom_smooth(method=lm, formula = y ~ poly(x, 1), se = FALSE)
      
    } else if (input$independent_variables == 'Latino') {
      ggplot(data=philly.dataset,aes(x = Latino, y = Attendance)) +
        geom_point() + 
        xlab("Number of enrolled Latino Students") +
        ylab("Attendance Rate") +
        ggtitle("Relationship between Attendance Rate and Latino students enrollment") +
        theme_bw() +
        theme(axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              plot.title = element_text(size = 12,hjust = 0.5)) +
        geom_smooth(method=lm, formula = y ~ poly(x, 1), se = FALSE)
      
    } else if (input$independent_variables == 'Other_Races') {
      ggplot(data=philly.dataset,aes(x = Other, y = Attendance)) +
        geom_point() + 
        xlab("Number of enrolled Students from other races") +
        ylab("Attendance Rate") +
        ggtitle("Relationship between Attendance Rate and students from other minority racial groups enrollment") +
        theme_bw() +
        theme(axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              plot.title = element_text(size = 12,hjust = 0.5)) +
        geom_smooth(method=lm, formula = y ~ poly(x, 1), se = FALSE)
      
    } else if (input$independent_variables == 'Total_suspensions') {
      ggplot(data=philly.dataset,aes(x = Total_suspensions, y = Attendance)) +
        geom_point() + 
        xlab("Number of total suspensions") +
        ylab("Attendance Rate") +
        ggtitle("Relationship between Attendance Rate and number of total suspensions") +
        theme_bw() +
        theme(axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              plot.title = element_text(size = 12,hjust = 0.5)) +
        geom_smooth(method=lm, formula = y ~ poly(x, 1), se = FALSE)
      
    } else if (input$independent_variables == 'Average_salary') {
      ggplot(data=philly.dataset,aes(x = Average_salary, y = Attendance)) +
        geom_point() + 
        xlab("Average Teacher Salary") +
        ylab("Attendance Rate") +
        ggtitle("Relationship between Attendance Rate and average teacher salary") +
        theme_bw() +
        theme(axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              plot.title = element_text(size = 12,hjust = 0.5)) +
        geom_smooth(method=lm, formula = y ~ poly(x, 1), se = FALSE)
      
    } else if (input$independent_variables == 'Drugs') {
      ggplot(data=philly.dataset,aes(x = Drugs, y = Attendance)) +
        geom_point() + 
        xlab("Number of drug-related crimes") +
        ylab("Attendance Rate") +
        ggtitle("Relationship between Attendance Rate and drug-related crimes") +
        theme_bw() +
        theme(axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              plot.title = element_text(size = 12,hjust = 0.5)) +
        geom_smooth(method=lm, formula = y ~ poly(x, 1), se = FALSE)
      
    } else if (input$independent_variables == 'Morals') {
      ggplot(data=philly.dataset,aes(x = Morals, y = Attendance)) +
        geom_point() + 
        xlab("Number of moral misconduct-related crimes") +
        ylab("Attendance Rate") +
        ggtitle("Relationship between Attendance Rate and moral misconduct-related crimes") +
        theme_bw() +
        theme(axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              plot.title = element_text(size = 12,hjust = 0.5)) +
        geom_smooth(method=lm, formula = y ~ poly(x, 1), se = FALSE)
      
    } else if (input$independent_variables == 'Assaults') {
      ggplot(data=philly.dataset,aes(x = Assaults, y = Attendance)) +
        geom_point() + 
        xlab("Number of assault-related crimes") +
        ylab("Attendance Rate") +
        ggtitle("Relationship between Attendance Rate and assault-related crimes") +
        theme_bw() +
        theme(axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              plot.title = element_text(size = 12,hjust = 0.5)) +
        geom_smooth(method=lm, formula = y ~ poly(x, 1), se = FALSE)
      
    } else if (input$independent_variables == 'Weapons') {
      ggplot(data=philly.dataset,aes(x = Weapons, y = Attendance)) +
        geom_point() + 
        xlab("Number of weapon-related crimes") +
        ylab("Attendance Rate") +
        ggtitle("Relationship between Attendance Rate and weapon-related crimes") +
        theme_bw() +
        theme(axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              plot.title = element_text(size = 12,hjust = 0.5)) +
        geom_smooth(method=lm, formula = y ~ poly(x, 1), se = FALSE)
      
    } else if (input$independent_variables == 'Thefts') {
      ggplot(data=philly.dataset,aes(x = Thefts, y = Attendance)) +
        geom_point() + 
        xlab("Number of theft-related crimes") +
        ylab("Attendance Rate") +
        ggtitle("Relationship between Attendance Rate and theft-related crimes") +
        theme_bw() +
        theme(axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              plot.title = element_text(size = 12,hjust = 0.5)) +
        geom_smooth(method=lm, formula = y ~ poly(x, 1), se = FALSE)
      
    } else if (input$independent_variables == 'Low_income_family') {
      ggplot(data=philly.dataset,aes(x = Low_income_family, y = Attendance)) +
        geom_point() + 
        xlab("Percentage of students coming from low-income families") +
        ylab("Attendance Rate") +
        ggtitle("Relationship between Attendance Rate and low-income families") +
        theme_bw() +
        theme(axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              plot.title = element_text(size = 12,hjust = 0.5)) +
        geom_smooth(method=lm, formula = y ~ poly(x, 1), se = FALSE)
    }
  })
  
  
  
  ####################################
  # LAST PAGE - DATASET DISPLAY
  ####################################
  output$philly.dataset.display <- renderDataTable({ philly.dataset })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
