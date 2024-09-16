### Specifying Libraries
library(shinythemes)
library(shiny)
library(rsconnect)
library(mfx)
library(ggplot2)
library(survey)
library(foreign)
library(stargazer)
library(rgdal)
library(plyr)
library(leaflet)
library(dplyr)
library(htmltools)
library(tidyverse)
library(ggmap)
library(gridExtra)
library(grid)
library(lattice)

### SERVER CODE - R code goes here

server<-function(input, output) { 
    
    ############# setting the working directory and loading state info. dataset
    setwd("C:/Users/hussainsarfraz/Desktop/DATA 410/Week 1 - 410")
    #setwd('C:/Users/hussainsarfraz/Desktop/DATA 410/Week 4 - 410/SARFRAZ_HW4_410')
    US_map_data.original <- read_csv('place_data.csv')
    
    US_map_data <- US_map_data.original
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
    
    ####################################
    # BARCHARTS - WEEK 4 CODE
    ####################################
    
    #Taking input from menu and filtering dataset to only include selected zipcodes
    filtered_dataset <- reactive({
        subset(US_map_data.original, CODE == input$first_zip | CODE == input$second_zip)
                            })

    output$plot <- renderPlot({filtered_dataset() %>% tidyr::gather("id", "value", 2:7) %>%
        ggplot(., aes(factor(CODE, levels = c(input$first_zip, input$second_zip)), value))+
        geom_bar(stat = "identity",aes(fill = factor(CODE, levels = c(input$first_zip, input$second_zip)) ))+
            scale_fill_manual(values = c('#554f66','#c2d1a4')) +
        xlab('Zip Code') +
        ylab('Percentage') +
        facet_wrap(~id) +
        theme(legend.position="none")
        }) 
    

}

####################################
# UI SCRIPT
####################################
ui<-fluidPage(theme = shinytheme("flatly"),
              ####################################
              #NAVIGATION BAR
              ####################################
              navbarPage("ACEs App", #this is the title for our navigation bar
                         
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
                                                                
                                                    )#first tab panel end
                                                )#main tab panel end
                                                
                                                                         )#right sidebar end
                         ),#'week 3 -regression' nav. bar end
                         

                         ####################################
                         #THIRD PAGE - WEEK 4 - BAR CHARTS
                         ####################################
                         
                         #navigation bar title
                         tabPanel("Week 4 - Bar Charts",

                                  #color for all titles
                                  tags$head(
                                      tags$style("h2 {color: #ee5500; }
                                               h1 {color: #04B4AE}; } ")),

                                  #first title
                                  headerPanel( "Compare Risk Index Factors Across Zip Codes"),

                                  ####################################
                                  # LEFT SIDEBAR
                                  ####################################
                                  sidebarLayout(position = "left",
                                                sidebarPanel(
                                                    h4("Zip Code of Interest:"),
                                                        selectInput("first_zip", "Zip Code of Interest",
                                                                    choices = US_map_data$CODE,
                                                                    selected = '19102'),

                                                    h4("Benchmark Zip Code:"),
                                                        selectInput("second_zip", "Benchmark Zip Code",
                                                                    choices = US_map_data$CODE,
                                                                    selected = '19144'),
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
                                                                tabPanel("Bar Charts of each variable by Zip Code",
                                                                         fluidRow(

                                                                             plotOutput("plot")

                                                                         ),
                                                                         h6(strong('Risk:'),'A composite index of measures of pverty, education, unemployment, crime, and ACEs'),
                                                                         h6(strong('Poverty:'),'Percent of families with children below poverty level'),
                                                                         h6(strong('Education:'),'Percent with less than 9th grade education'),
                                                                         h6(strong('Unemployment:'),'Percent of unemployed'),
                                                                         h6(strong('Crime:'),'Shooting victims per 10,000'),
                                                                         h6(strong('ACEs:'),'Percent with at least one Adverse Childhood Experience'),
                                                                                                                                              )#first tab panel end
                                                                )#all tab panel end

                                                    )#main tab panel end
                                                )#sidebar tab panel end

                                  )#right sidebar end
                         )#'week 4 -Bar Chart' nav. bar end
                         
              )#navigation bar function end


# This is our new code to merge the server and ui objects into an app!
shinyApp(ui = ui, server = server)
