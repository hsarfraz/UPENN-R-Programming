#-----------------------------------
# Intro to Shiny
#-----------------------------------
library(shiny)
library(shinythemes)
library(leaflet)
library(dplyr)
library(data.table)
library(DT)


# This very basic shiny app takes our presidents birthplace
# leaflet map and allows us to filter which presidents are
# displayed by party affiliation.It also displays a data 
# table in another tab. 


# This begins the app
server <- function(input, output) {
  # Here we are reading in our birthplace data
  birthplace <- read.csv("birthplace.csv")
  #-------------------
  # Presidents Map
  #-------------------
  
  # Here is out map exactly as we made it in our last script,
  # except we are placing it in the object "map"
  map <- leaflet() %>%
    addTiles() %>%
    addCircleMarkers(
      ~ Longitude,
      ~ Latitude,
      data = birthplace,
      color = "purple",
      radius = 9,
      weight = 1,
      popup = paste(
        "President:",
        birthplace$President,
        "<br>",
        "Birth Place:",
        birthplace$Town
      )
    )
  
  # here we tell shiny to render the map in our app
  output$map <- renderLeaflet(map)
  
  
  #--------------------
  # Reactive
  #--------------------  
  
  # This section of code defines a reactive that will allow us to filter
  # the data based on party affiliation
  # A reactive takes input from your ui and can store that information in 
  # memory so that it can be passed to render functions
  filteredData <- reactive({
    if (input$CODE1 != "All")
    {
      birthplace <- filter(birthplace, Party == input$CODE1)
    }
    else {
      birthplace
    }
  })
  
  #--------------------
  # Observe
  #--------------------
  
  # The observe function allows us to take the input from the reactive above
  # and edit pieces of the leaflet map when combined with leafletProxy()
  observe({
    mapdata <- filteredData()
    if (nrow(mapdata) != 0) {
      #if mapdata exists, then do the following:
      leafletProxy("map", data = mapdata) %>%
        clearMarkers() %>% # clear existing markers
        addCircleMarkers(
          # add new markers from this new data
          ~ Longitude,
          ~ Latitude,
          data = mapdata,
          color = "purple",
          radius = 9,
          weight = 1,
          popup = paste(
            # keep the same popup info, but different dataset
            "President:",
            mapdata$President,
            "<br>",
            "Birth Place:",
            mapdata$Town
          )
        )
    }
    
  })
  
  #-------------------
  # Data Table
  #-------------------
  
  # Using DT package, we're making a data table
  output$table2 <- DT::renderDataTable(DT::datatable({
    # Choosing just a few columns to display
    data <- birthplace[, 2:5]
    
    # Allowing the dropdowns to filter our code
    if (input$CODE2 != "All") {
      data <- data[data$Party == input$CODE2, ]
    }
    data
  },
  # We can rename the columns displayed in the table
  colnames = c("Name", "Party", "Place", "State")))
  

} # Closes the server function


#-----------------------------------
# UI
#-----------------------------------

ui <- shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  # This applies preset aesthetic decisions to the entire app
  # Check out this page for more themes: https://rstudio.github.io/shinythemes/
  navbarPage(
    "Birthplace of US Presidents",
    # navbarPage lets us set up a navigation bar to toggle between tabPanels
    # First tab
    tabPanel(
      "Welcome",
      headerPanel(" "),
      # This is just a blank space - trying typing in the quotes and see what happens!
      br(),
      # this is a space
      headerPanel("Welcome to the Birthplace of US Presidents' Map!"),
      br(),
      h2("Instructions:"),
      # h#() is the same typeface at different sizes
      h4(("Type information about your app here!")),
      br()
    ),
    # Second tab
    tabPanel(
      "Presidents Map",
      headerPanel("Add your title here!"),
      br(),
      h2(""),
      # the map is called here
      leafletOutput("map",
                    width = "100%",
                    height = "600px"),
     # the absolute panel is the widget that allows you to select by party
       absolutePanel(
        class = "panel panel-default",
        draggable = TRUE,
        top = 300,
        left = 50,
        right = "auto",
        bottom = "auto",
        width = 330,
        height = "auto",
        h2("Select a subset of Presidents to display."),
        # selectInput makes the dropdown selector. The first argument is a name
        # you are giving the output, the second is the text that appears above the
        # drop down, and the 3rd is the selections that populate the dropdown. 
        # Any time you refer to the output given by the dropdowns in the server
        # script, you call "CODE1"
        selectInput(
          "CODE1",
          "Party Affiliation:",
          c(
            "All",
            "Democratic",
            "Democratic Republican",
            "Federalist",
            "Republican",
            "Whig",
            "Unaffiliated"
          )
        )
      )
    ),
    # Third tab
    tabPanel(
      "Data Table",
      headerPanel("Examine the Data"),
      # Create a new Row in the UI for selectInputs
      selectInput(
        "CODE2",
        "Party:",
        c(
          "All",
          "Democratic",
          "Democratic Republican",
          "Federalist",
          "Republican",
          "Whig",
          "Unaffiliated"
        )
      ),
      # Create a new row for the table.
      DT::dataTableOutput("table2")
    )
  )
))
shinyApp(ui = ui, server = server)
