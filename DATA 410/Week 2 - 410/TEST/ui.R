#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

shinyUI(
  
  #########################
  #NAVIGATION BAR
  #########################
  
  navbarPage(
    "Welcome! This is the navigation bar",
    # navbarPage lets us set up a navigation bar to toggle between tabPanels
    # First tab
    tabPanel(
      "Risk Index",
      headerPanel(" "),
      # This is just a blank space - trying typing in the quotes and see what happens!
      br(),
      # this is a space
      headerPanel("Look below to see a map that shows the risk index mapped by different zip codes"),
      br(),
      h2("Instructions:"),
      # h#() is the same typeface at different sizes
      h4(("Type information about your app here!")),
      br(),
      # Set the style for the header
      tags$head(
        tags$style("h2 {color: #ee5500; }, h1 {color: #04B4AE; }")),
      headerPanel("Mapping of Risk Index and other variables for specific Counties in Philadelphia"),
      br(),
      leafletOutput("map", width = "100%", height = "800px")
    ),
    # Second tab
    tabPanel(
      "Presidents Map",
      headerPanel("Add your title here!"),
      br(),
      h2(""),
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
)