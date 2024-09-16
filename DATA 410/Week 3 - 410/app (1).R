
#-----------------------------------#
# Server                            #
# Prediction Tab and Plot V         #
#                                   #
#-----------------------------------#


# In this script, we are combining the UI and Server into one script called 
# app.R
# This changes our syntax slightly. instead
# of using the shinyui and server functions, we will create
# two objects, server and ui, and then use the shinyApp() fuction
# at the end of the script to run our app. Pretty cool!

#--------------------------------------
# Library
#--------------------------------------
# We only need to include our library at the top of this script
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



#------------------------------------
# Beginning Shiny Code
#------------------------------------

# This line creates the object server
server<-function(input, output) {
 
  
  #------------------------------------
  # Data Set up
  #------------------------------------
  # Loading in my dataframe
  load("finaldata2.rda")
  
  #save(data, file="finaldata2.rda")
  #holding data in dataframe dat
  dat<-data
  
  # svydesign from survey package adds weights to my observations
  comp42.design <- svydesign(id= ~idnumr, data= data, weights= data$nschwt)
  
  
  
  #------------------
  # Regression
  #------------------
  
  # creating a reactive regression forumula that uses inputs from the check list
  # as independent variables to predict the variable ACE_BI
  # then, put that formula into the svyglm() from survey package which outputs 
  # a weighted regression
  model <- reactive({
    
    svyglm(paste("ACE_BI"," ~ ",paste(input$iv1,collapse="+")),family= quasibinomial, comp42.design)
    
  })
  
  # Creating pretty labels for the stargazer table
  # Here, we are using if statements to build a vector, covars, that is dependent on the inputs
  #from the beck list. 
  covar.label <- reactive({
    covars<-character()
    if ('Race' %in% input$iv1){
      covars <- c(covars,"Black","Other Race")
    } 
    
    if ('Absence' %in% input$iv1){
      covars <- c(covars,"Chronically Absent")
    } 
    
    if ('Contact' %in% input$iv1){
      covars <- c(covars,"School Contacted Home")
    } 
    
    if ('Homework' %in% input$iv1){
      covars <- c(covars,"Rarely Completes Homework")
    } 
    
    if ('Lunch' %in% input$iv1){
      covars <- c(covars,"Receives Free Lunch")
    } 
    
    if ('Retained' %in% input$iv1){
      covars <- c(covars,"Retained a Grade")
    } 
    return(covars)
  })
  
  #Create nice regression table output
  #stargazer() comes from the stargazer package
  output$regTab <- renderText({
    covars <- covar.label()
    stargazer(model(),type="html",dep.var.labels ="ACEs Prediction",covariate.labels = covars,omit.stat = c("f","ser","aic","adj.rsq"))
  })
  
  
  
  #--------------------------------
  # Prediction
  #--------------------------------
  
  # Using the inputs from the checklist, we are building a dataframe to use for predict()
  aceRiskA <- reactive({
    if (input$varRace == "Black"){
      isRace <- "Black Only"
    } else if (input$varRace == "White") {
      isRace <- "White Only"
    } else {
      isRace <- "Other Race"
    }
    
    if (input$varAbsent == "No"){
      isAbsent <- 0
    } else {
      isAbsent <- 1
    }
    
    if (input$varContact == "No"){
      isContact <- 0
    } else {
      isContact <- 1
    }
    
    if (input$varHomework == "No"){
      isHomework <- 0
    } else {
      isHomework <- 1
    }
    
    if (input$varLunch == "No"){
      isLunch <- 0
    } else {
      isLunch <- 1
    }
    
    if (input$varRetained == "No"){
      isRetained <- 0
    } else {
      isRetained <- 1
    }
    
    # Here's the dataframe 
    child.df<-data.frame(Race=factor(isRace, levels=c("White Only","Black Only","Other Race")),
                         Absence=factor(isAbsent,levels=c(0,1)),
                         Contact=factor(isContact,levels=c(0,1)),
                         Homework=factor(isHomework,levels=c(0,1)),
                         Lunch=factor(isLunch,levels=c(0,1)),
                         Retained=factor(isRetained,levels=c(0,1)))
    
    #Include the variable if it is in the input list, else remove it
    coefList <- coef(model())
    if ('Race' %in% input$iv1){
      
    } else {
      child.df <- subset(child.df, select = -Race)
    }
    
    if ('Absence' %in% input$iv1){
      
    } else {
      child.df <- subset(child.df, select = -Absence)
    }
    
    if ('Contact' %in% input$iv1){
      
    } else {
      child.df <- subset(child.df, select = -Contact)
    }
    
    if ('Homework' %in% input$iv1){
      
    } else {
      child.df <- subset(child.df, select = -Homework)
    }
    
    if ('Lunch' %in% input$iv1){
      
    } else {
      child.df <- subset(child.df, select = -Lunch)
    }
    
    if ('Retained' %in% input$iv1){
      
    } else {
      child.df <- subset(child.df, select = -Retained)
    }
    
    # get a prediction of ace score based on input data
    aceRisk <- as.data.frame(predict(model(),newdata=child.df,type="response"))
    
    return(aceRisk)
  })
  # reporting the predction and error estimate
  output$ACEscore <- renderText({ 
    
    aceRound <- round(aceRiskA()$response,2)
    aceError <- round(aceRiskA()$SE,2)
    paste("ACE risk is ",aceRound,"\u00B1", aceError)
  })
  # creating the inputs for the 2nd child
  aceRiskB <- reactive({
    if (input$varRace2 == "Black"){
      isRace <- "Black Only"
    } else if (input$varRace2 == "White") {
      isRace <- "White Only"
    } else {
      isRace <- "Other Race"
    }
    
    if (input$varAbsent2 == "No"){
      isAbsent <- 0
    } else {
      isAbsent <- 1
    }
    
    if (input$varContact2 == "No"){
      isContact <- 0
    } else {
      isContact <- 1
    }
    
    if (input$varHomework2 == "No"){
      isHomework <- 0
    } else {
      isHomework <- 1
    }
    
    if (input$varLunch2 == "No"){
      isLunch <- 0
    } else {
      isLunch <- 1
    }
    
    if (input$varRetained2 == "No"){
      isRetained <- 0
    } else {
      isRetained <- 1
    }
    #here's the dataframe
    child.df<-data.frame(Race=factor(isRace, levels=c("White Only","Black Only","Other Race")),
                         Absence=factor(isAbsent,levels=c(0,1)),
                         Contact=factor(isContact,levels=c(0,1)),
                         Homework=factor(isHomework,levels=c(0,1)),
                         Lunch=factor(isLunch,levels=c(0,1)),
                         Retained=factor(isRetained,levels=c(0,1)))
    #Selects which inputs to include
    coefList <- coef(model())
    if ('Race' %in% input$iv1){
      
    } else {
      child.df <- subset(child.df, select = -Race)
    }
    
    if ('Absence' %in% input$iv1){
      
    } else {
      child.df <- subset(child.df, select = -Absence)
    }
    
    if ('Contact' %in% input$iv1){
      
    } else {
      child.df <- subset(child.df, select = -Contact)
    }
    
    if ('Homework' %in% input$iv1){
      
    } else {
      child.df <- subset(child.df, select = -Homework)
    }
    
    if ('Lunch' %in% input$iv1){
      
    } else {
      child.df <- subset(child.df, select = -Lunch)
    }
    
    if ('Retained' %in% input$iv1){
      
    } else {
      child.df <- subset(child.df, select = -Retained)
    }
    
    #prediction for childB
    aceRisk <- as.data.frame(predict(model(),newdata=child.df,type="response"))
    
    return(aceRisk)
  })
  # outputting the prediction and error
  output$ACEscore2 <- renderText({ 
    
    aceRound <- round(aceRiskB()$response,2)
    aceError <- round(aceRiskB()$SE,2)
    paste("ACE risk is ",aceRound,"\u00B1", aceError)
  })
  
  # Plot is a standard ggplot bar chart with reactive data
  output$comparePlot <- renderPlot({
    Child <- c("A","B")
    plotData <- as.data.frame(Child)
    plotData$Score <- c(aceRiskA()$response,aceRiskB()$response)
    plotData$SE <- c(aceRiskA()$SE,aceRiskB()$SE)
    ErrorBars <- aes(ymax = plotData$Score + plotData$SE, ymin=plotData$Score - plotData$SE)
    d<- ggplot(data=plotData, aes(x=Child, y=Score, fill=Child)) + 
      geom_bar(width=.4, stat="identity")+
      theme_classic(base_size = 16) +
      geom_errorbar(ErrorBars, width=0.25)+
      xlab("Child") + ylab("Probability") +
      ggtitle("")+
      scale_y_continuous(limits = c(0, 1))
    print(d)
  })
  
  #-------------------------
  # Map
  #-------------------------
  
  # Everything pertaining to the map is the same as in our first script
  
  # Use these coordinates to center the map
  lat <- 39.1582
  lng <- -75.5244
  zoom <- 8
  
  #reading in data 
  acestate<-read.csv("test3.csv")
  
  #prepping the alt variable
  acestate$alt<- (100)- acestate$zed
  
  #reading  in the shapefile
  states <- readOGR("cb_2016_us_state_500k", layer = "cb_2016_us_state_500k",encoding = "UTF-8")
  
  # using a left_join because the datafile and the shapefile line up EXACTLY - if they dont, you cant use left join
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
  factpal <- colorFactor(c("#f0f9e8", "#bae4bc",
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
 
}


#-----------------------------------#
# UI                                #
#                                   #
#                                   #
#-----------------------------------#

# Here's our new UI code

# Notice there is no function here, just the fluidPage()
ui<-fluidPage(theme = shinytheme("flatly"),
              # navbarPage() is making multiple tabs in our app        
              navbarPage("ACEs App", #this is the title for our navigation bar
                         tabPanel("Welcome", #tabPanel() creates our first tab
                                  tags$head(
                                    tags$style("h2 {color: #04B4AE; }
                                               h1 {color: #04B4AE}; }
                                               ")), # this is setting the color palette for our tab headers 1 and 2
                                  headerPanel(" "), # I haven't created a title for this page, try adding one! 
                                  br(), #a break creates a space
                                  h2("How to Use This App"), # the number following h corresponds to size
                                  h4(tags$ul(
                                    tags$li("Analyzing ACEs: Build a regression model and predict the probability a child has 1 or more ACEs"), #bullet point 
                                    tags$li("Mapping ACEs: Explore the differences in rates of ACEs between states") #bullet point
                                  )), 
                                  h4("To begin, select \"Analyzing ACEs\" on the navigation bar. You will be asked to select a series of independent variables in order to build a regression model. Once you have selected your independent variables, the second tab will present you with a visualization of how much of an effect these variables have on the percent chance a child has 1 or more ACEs. The third tab allows you to enter information for two children to calculate their predicted chance of having 1 or more ACEs."),
                                  h2("The Data"),
                                  h4("This app uses data from the National Children's Health Survey 2011/2012. The survey, conducted by phone, asks a parent or legal guardian about one of the children in their household at random. The survey covers information on a child's current physical, developmental, and mental health, family stability, school experiences, community, and whether or not the child has experienced any of the ACEs defined above. This dataset does not contain information on physical and sexual abuse."),
                                  h2("Methodology"),
                                  h4("This app uses logistic regression to predict the probability of a child having 1 or more ACEs based on the child's race, whether or not they have missed 11 or more days of school, whether the school has contacted home about the child's behavior, whether the child participates in the free lunch program, whether the child frequently or always fails to complete homework, and whether the child has been retained one or more grades in school. For this app, the set of ACEs include: witnessing domestic violence, neglect, separation from parent, death of parent, parental incarceration, emotional abuse, victimized by a hate crime, household substance abuse, and household mental illness.")
                                    ),
                         
                         # First Navigation bar tab
                         # this information will appear accross the inner-tabs, called panels
                         tabPanel("Analyzing ACEs",
                                  tags$head(
                                    tags$style("h2 {color: #ee5500; }
                                               h1 {color: #04B4AE}; }
                                               
                                               ")),
                                  headerPanel( "ACEs Prediction Model"),
                                  #The sidebar allows me to select
                                  #multiple independent variables
                                  sidebarLayout(position = "right",
                                                sidebarPanel(
                                                  h2("Build your model"),
                                                  br(),
                                                  checkboxGroupInput("iv1", 
                                                                     label = "Select any of the independent variables below to calculate your model. You can change your selection at any time.", 
                                                                     list("Race"="Race", "Absent 11 or more days"="Absence", "School contacted home"="Contact","Child rarely completes homework"="Homework",
                                                                          "Child receives free lunch"="Lunch", "Child has repeated a grade" = "Retained"), selected="Race"
                                                                     #Note race is selected when the app starts
                                                                      )
                                                                ),
                                                
                                                
                                                
                                                mainPanel(
                                                  br(),
                                                  #create a 1st tab panel
                                                  tabsetPanel(type = "tabs", 
                                                              #first panel shows regression table
                                                              tabPanel("Regression Table",
                                                                       h3("Table of Regression Coefficients"),
                                                                       HTML('</br>'),
                                                                       tableOutput("regTab"),
                                                                       HTML('</br>'),
                                                                       helpText("The table displays the coefficients of your model in the log-odds scale. Larger numbers indicate that a variable has a greater effect. A positive number indicates that a variable increases the likelihood of ACEs while a negative indicates that a variable decreases the likelihood of ACEs. The P value determines statistical significance. P values below 0.05 are commonly accepted as significant.")),
                                              
                                                              tabPanel("Predict the Probability of ACEs",
                                                                       fluidRow(h3("Compare the Predicted Probability of ACEs between Two Children",align = "center")),
                                                                       fluidRow(
                                                                         column(6,
                                                                                h4("Child A"),
                                                                                conditionalPanel(
                                                                                  condition = "input.iv1.indexOf('Race') != -1",
                                                                                  selectInput("varRace", "Is the child either White or Black? If neither, select Other.",
                                                                                              list("White", "Black", "Other"))
                                                                                ),
                                                                                conditionalPanel(
                                                                                  condition = "input.iv1.indexOf('Absence') != -1",
                                                                                  selectInput("varAbsent", "Has the child been absent 11 or more days this year?",
                                                                                              list("Yes", "No"),
                                                                                              selected = "No")
                                                                                ),
                                                                                conditionalPanel(
                                                                                  condition = "input.iv1.indexOf('Contact') != -1",
                                                                                  selectInput("varContact", "Has the school contacted home this year about the child's behavior?",
                                                                                              list("Yes", "No"),
                                                                                              selected = "No")
                                                                                ),
                                                                                conditionalPanel(
                                                                                  condition = "input.iv1.indexOf('Homework') != -1",
                                                                                  selectInput("varHomework", "Does the child frequently or always fail to complete homework?",
                                                                                              list("Yes", "No"),
                                                                                              selected = "No")
                                                                                ),
                                                                                conditionalPanel(
                                                                                  condition = "input.iv1.indexOf('Lunch') != -1",
                                                                                  selectInput("varLunch", "Does the child receive the free lunch program?",
                                                                                              list("Yes", "No"),
                                                                                              selected = "No")
                                                                                ),
                                                                                conditionalPanel(
                                                                                  condition = "input.iv1.indexOf('Retained') != -1",
                                                                                  selectInput("varRetained", "Has the child ever repeated a grade?",
                                                                                              list("Yes", "No"),
                                                                                              selected = "No")
                                                                                ),
                                                                                textOutput("ACEscore"),
                                                                                HTML('</br>')
                                                                         ),
                                                                         column(6,
                                                                                h4("Child B"),
                                                                                conditionalPanel(
                                                                                  condition = "input.iv1.indexOf('Race') != -1",
                                                                                  selectInput("varRace2", "Is the child either White or Black? If neither, select Other.",
                                                                                              list("White", "Black", "Other"))
                                                                                ),
                                                                                conditionalPanel(
                                                                                  condition = "input.iv1.indexOf('Absence') != -1",
                                                                                  selectInput("varAbsent2", "Has the child been absent 11 or more days this year?",
                                                                                              list("Yes", "No"),
                                                                                              selected = "No")
                                                                                ),
                                                                                conditionalPanel(
                                                                                  condition = "input.iv1.indexOf('Contact') != -1",
                                                                                  selectInput("varContact2", "Has the school contacted home this year about the child's behavior?",
                                                                                              list("Yes", "No"),
                                                                                              selected = "No")
                                                                                ),
                                                                                conditionalPanel(
                                                                                  condition = "input.iv1.indexOf('Homework') != -1",
                                                                                  selectInput("varHomework2", "Does the child frequently or always fail to complete homework?",
                                                                                              list("Yes", "No"),
                                                                                              selected = "No")
                                                                                ),
                                                                                conditionalPanel(
                                                                                  condition = "input.iv1.indexOf('Lunch') != -1",
                                                                                  selectInput("varLunch2", "Does the child receive the free lunch program?",
                                                                                              list("Yes", "No"),
                                                                                              selected = "No")
                                                                                ),
                                                                                conditionalPanel(
                                                                                  condition = "input.iv1.indexOf('Retained') != -1",
                                                                                  selectInput("varRetained2", "Has the child ever repeated a grade?",
                                                                                              list("Yes", "No"),
                                                                                              selected = "No")
                                                                                ),
                                                                                textOutput("ACEscore2"),
                                                                                HTML('</br>')
                                                                                )
                                                                                  ),
                                                                       fluidRow(helpText("Probability that the children have 1 or more ACEs.")),
                                                                       HTML('</br>'),
                                                                       
                                                                       fluidRow( h4("A Visual Comparison of Probability of ACEs between Child A and Child B", align = "center"),
                                                                                 plotOutput("comparePlot")
                                                                                )
                                                                        )
                                                              
                                                  ))
                                  )),
                         # Here's our third page
                         tabPanel("Mapping ACEs", 
                                  tags$head(
                                    tags$style("h2 {color: #ee5500; }
                                               h1 {color: #04B4AE}; }
                                               ")),
                                  headerPanel("Mapping Differences in Percentage of Children with ACEs by State"),
                                  br(),
                                  h2(""),
                                  leafletOutput("map", width = "1200px", height = "800px")
                                    ))
                         ) 
# This is our new code to merge the server and ui objects into an app!
shinyApp(ui = ui, server = server)
