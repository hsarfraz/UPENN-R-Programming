### Specifying Libraries
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

### SERVER CODE - R code goes here

server<-function(input, output) { 

    ############# setting the working directory and loading state info. dataset
    setwd("C:/Users/hussainsarfraz/Desktop/DATA 410/Week 1 - 410")
    US_map_data <- read_csv('place_data.csv')
    #US_map_data <- readOGR(dsn=path.expand("C:/Users/hussainsarfraz/Desktop/DATA 410/Week 3 - 410/SARFRAZ_HW3_410/place_data.csv"), layer="place_data.csv")
    ####################################
    # HOMEPAGE - WEEK 2 CODE
    ####################################
    
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
    output$map <- renderLeaflet(map)
    
    ####################################
    # REGRESSION - WEEK 3 CODE
    ####################################
    
    ####REGRESSION WITHOUT WEIGHTS (NO 'svydesign' OBJECT NEEDED HERE)
    model <- reactive({
        lm(paste("Risk"," ~ ",paste(input$iv1,collapse="+")),family= quasibinomial, US_map_data)
    })
    
    ####COVARIATE LABELS IN REGRESSION MODEL CODE
    covar.label <- reactive({
        covars<-character()
        if ('Poverty' %in% input$iv1){
            covars <- c(covars,"Poverty Percentage change with 1 unit increase is Risk")
        }
        
        if ('Education' %in% input$iv1){
            covars <- c(covars,"Education Percentage change with 1 unit increase is Risk")
        }
        
        if ('Unemployment' %in% input$iv1){
            covars <- c(covars,"Unemployment Percentage change with 1 unit increase is Risk")
        }
        
        if ('Crime' %in% input$iv1){
            covars <- c(covars,"Crime Percentage change with 1 unit increase is Risk")
        }
        
        if ('ACEs' %in% input$iv1){
            covars <- c(covars,"ACEs Percentage change with 1 unit increase is Risk")
        }
        
        return(covars)
    })
    
    ####REGRESION OUTPUT
    output$regTab <- renderText({
        covars <- covar.label()
        stargazer(model(),type="html",dep.var.labels ="Risk",covariate.labels = covars,omit.stat = c("f","ser","aic","adj.rsq"))
    })
    
}

####################################
# UI SCRIPT
####################################
ui<-fluidPage(theme = shinytheme("flatly"),
              ####################################
              #NAVIGATION BAR
              ####################################
              navbarPage("Risk App", #this is the title for our navigation bar
                         
                         ####################################
                         #HOMEPAGE - WEEK 2
                         ####################################
                         tabPanel("Week 2", #tabPanel() creates our first tab
                                  tags$head(
                                      tags$style("h2 {color: #04B4AE; }
                                               h1 {color: #04B4AE}; } ")
                                      ), # this is setting the color palette for our tab headers 1 and 2
                                  
                                  headerPanel("Mapping of Risk Index and other variables for specific Counties in Philadelphia"),
                                  br(),
                                  leafletOutput("map", width = "100%", height = "800px"),
                                  ),
                         
                         ####################################
                         #SECOND PAGE - WEEK 3 REGRESSION
                         ####################################
                         
                         #navigation bar title
                         tabPanel("Week 3 - Regression",
                                  
                                  #color for all titles
                                  tags$head(
                                      tags$style("h2 {color: #ee5500; }
                                               h1 {color: #04B4AE}; } ")),
                                  
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
                                                                       list("Poverty"='Poverty', "Education"='Education', "Unemployment"='Unemployment', "Crime"='Crime', "ACEs"='ACEs'),
                                                                       selected="Poverty" #Note race is selected when the app starts 
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
                                                                         helpText("The table displays the coefficients of your model in the log-odds scale. Larger numbers indicate that a variable has a greater effect. A positive number indicates that a variable increases the likelihood of Risk while a negative indicates that a variable decreases the likelihood of Risk. The P value determines statistical significance. P values below 0.05 are commonly accepted as significant.")),
                                                                
                                                                # ####################################
                                                                # #second tab
                                                                # ####################################
                                                                # tabPanel("Predict the Probability of ACEs",
                                                                #          fluidRow(h3("Compare the Predicted Probability of ACEs between Two Children",align = "center")),
                                                                #          fluidRow(
                                                                #              column(6,
                                                                #                     h4("Child A"),
                                                                #                     conditionalPanel(
                                                                #                         condition = "input.iv1.indexOf('Race') != -1",
                                                                #                         selectInput("varRace", "Is the child either White or Black? If neither, select Other.",
                                                                #                                     list("White", "Black", "Other"))
                                                                #                     ),
                                                                #                     conditionalPanel(
                                                                #                         condition = "input.iv1.indexOf('Absence') != -1",
                                                                #                         selectInput("varAbsent", "Has the child been absent 11 or more days this year?",
                                                                #                                     list("Yes", "No"),
                                                                #                                     selected = "No")
                                                                #                     ),
                                                                #                     conditionalPanel(
                                                                #                         condition = "input.iv1.indexOf('Contact') != -1",
                                                                #                         selectInput("varContact", "Has the school contacted home this year about the child's behavior?",
                                                                #                                     list("Yes", "No"),
                                                                #                                     selected = "No")
                                                                #                     ),
                                                                #                     conditionalPanel(
                                                                #                         condition = "input.iv1.indexOf('Homework') != -1",
                                                                #                         selectInput("varHomework", "Does the child frequently or always fail to complete homework?",
                                                                #                                     list("Yes", "No"),
                                                                #                                     selected = "No")
                                                                #                     ),
                                                                #                     conditionalPanel(
                                                                #                         condition = "input.iv1.indexOf('Lunch') != -1",
                                                                #                         selectInput("varLunch", "Does the child receive the free lunch program?",
                                                                #                                     list("Yes", "No"),
                                                                #                                     selected = "No")
                                                                #                     ),
                                                                #                     conditionalPanel(
                                                                #                         condition = "input.iv1.indexOf('Retained') != -1",
                                                                #                         selectInput("varRetained", "Has the child ever repeated a grade?",
                                                                #                                     list("Yes", "No"),
                                                                #                                     selected = "No")
                                                                #                     ),
                                                                #                     textOutput("ACEscore"),
                                                                #                     HTML('</br>')
                                                                #              ),
                                                                #              column(6,
                                                                #                     h4("Child B"),
                                                                #                     conditionalPanel(
                                                                #                         condition = "input.iv1.indexOf('Race') != -1",
                                                                #                         selectInput("varRace2", "Is the child either White or Black? If neither, select Other.",
                                                                #                                     list("White", "Black", "Other"))
                                                                #                     ),
                                                                #                     conditionalPanel(
                                                                #                         condition = "input.iv1.indexOf('Absence') != -1",
                                                                #                         selectInput("varAbsent2", "Has the child been absent 11 or more days this year?",
                                                                #                                     list("Yes", "No"),
                                                                #                                     selected = "No")
                                                                #                     ),
                                                                #                     conditionalPanel(
                                                                #                         condition = "input.iv1.indexOf('Contact') != -1",
                                                                #                         selectInput("varContact2", "Has the school contacted home this year about the child's behavior?",
                                                                #                                     list("Yes", "No"),
                                                                #                                     selected = "No")
                                                                #                     ),
                                                                #                     conditionalPanel(
                                                                #                         condition = "input.iv1.indexOf('Homework') != -1",
                                                                #                         selectInput("varHomework2", "Does the child frequently or always fail to complete homework?",
                                                                #                                     list("Yes", "No"),
                                                                #                                     selected = "No")
                                                                #                     ),
                                                                #                     conditionalPanel(
                                                                #                         condition = "input.iv1.indexOf('Lunch') != -1",
                                                                #                         selectInput("varLunch2", "Does the child receive the free lunch program?",
                                                                #                                     list("Yes", "No"),
                                                                #                                     selected = "No")
                                                                #                     ),
                                                                #                     conditionalPanel(
                                                                #                         condition = "input.iv1.indexOf('Retained') != -1",
                                                                #                         selectInput("varRetained2", "Has the child ever repeated a grade?",
                                                                #                                     list("Yes", "No"),
                                                                #                                     selected = "No")
                                                                #                     ),
                                                                #                     textOutput("ACEscore2"),
                                                                #                     HTML('</br>')
                                                                #              )
                                                                #          ),
                                                                #          fluidRow(helpText("Probability that the children have 1 or more ACEs.")),
                                                                #          HTML('</br>'),
                                                                # 
                                                                #          fluidRow( h4("A Visual Comparison of Probability of ACEs between Child A and Child B", align = "center"),
                                                                #                    plotOutput("comparePlot")
                                                                #          )
                                                                # )#second tab panel end
                                                                
                                                    )#first tab panel end
                                                    )#main tab panel end
                                                
                                  )#right sidebar end
                                  )#'week 3 -regression' nav. bar end
                         
                         )#navigation bar function end
) 
# This is our new code to merge the server and ui objects into an app!
shinyApp(ui = ui, server = server)
