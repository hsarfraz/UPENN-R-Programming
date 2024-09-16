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

############# setting the working directory and loading state info. dataset
setwd("C:/Users/hussainsarfraz/Desktop/DATA 410/Week 4 - 410/SARFRAZ_HW4_410")
US_map_data <- read_csv('place_data.csv')

summary(US_map_data)
list(US_map_data)
length(names(US_map_data)) #7 total columns

US_map_data <- subset(US_map_data, CODE == 19102 | CODE == 19144 )

US_map_data$CODE <- factor(US_map_data$CODE, levels = c(19144,19102)) 


US_map_data %>% tidyr::gather("id", "value", 2:7) %>% 
  ggplot(., aes(CODE, value))+
  geom_bar(stat = "identity", fill = "cadetblue")+
  facet_wrap(~id)



graphs %>% tidyr::gather("id", "value", 1:4) %>% 
  ggplot(., aes(Group.1, value))+
  geom_bar()+
  xlab("Year") +
  ylab("Count") +
  geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(~id)

#grid.arrange(graphs[[1:2]])
grid.arrange(graphs[[1]],graphs[[2]])

myplots <- lapply(split(US_map_data,US_map_data$cov.out),function(x){
    x$topic <- factor(x$topic, levels=x$topic[order(x$pe,decreasing=F)])

    ggplot(x, aes(x =topic, y =pe, fill =titles, width =0.75)) +
        geom_bar(stat ='identity') +
        scale_fill_discrete(drop=F)+ #to force all levels to be considered, and thus different
        theme_bw()+
        theme(legend.position = 'none')+
        labs(y='Percent Increase', x="", title = unique(x$titles))

})

summary(lm(Risk~Poverty,paste(input$iv1,collapse="+"), data=US_map_data))




############# PROFESSOR CODE
# svydesign from survey package adds weights to my observations
svydesign_US_map_data <- svydesign(id= ~0, data= US_map_data)

# creating a reactive regression forumula that uses inputs from the check list
# as independent variables to predict the variable ACE_BI
# then, put that formula into the svyglm() from survey package which outputs 


# a weighted regression
model <- reactive({
  svyglm(paste("Risk"," ~ ",paste(input$iv1,collapse="+")),family= quasibinomial, svydesign_US_map_data)
})

# Creating pretty labels for the stargazer table
# Here, we are using if statements to build a vector, covars, that is dependent on the inputs
#from the beck list. 

covar.label <- reactive({
  covars<-character()
  if ('Poverty' %in% US_map_data$Poverty){
    covars <- c(covars,"Poverty Percentage")
  }

  if ('Education' %in% US_map_data$Education){
    covars <- c(covars,"Education Percentage")
  }

  if ('Unemployment' %in% US_map_data$Unemployment){
    covars <- c(covars,"Unemployment Percentage")
  }

  if ('Crime' %in% US_map_data$Crime){
    covars <- c(covars,"Crime Percentage")
  }

  if ('ACEs' %in% US_map_data$ACEs){
    covars <- c(covars,"ACEs Percentage")
  }

  return(covars)
})

#Create nice regression table output
#stargazer() comes from the stargazer package
output$regTab <- renderText({
  covars <- covar.label()
  stargazer(model(),type="html",dep.var.labels ="ACEs Prediction",covariate.labels = covars,omit.stat = c("f","ser","aic","adj.rsq"))
})

############# Variables for Regression Analysis
# states@data$Risk
# states@data$Poverty 
# states@data$Education
# states@data$Unemployment
# states@data$Crime
# states@data$ACEs
