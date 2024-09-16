########################################################################
#                       libraries                                      #
########################################################################
library(shinythemes)
library(shiny)
library(rsconnect)
library(DT)
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
library(wordcloud)
library(wordcloud2)
library(tidytext)

########################################################################
#                       UI - USER INTERFACE                            #
########################################################################
ui <- fluidPage(theme = shinytheme("flatly"),
 
                navbarPage("ACEs App", #this is the title for our navigation bar
    
    ####################################
    #HOMEPAGE - WEEK 2
    ####################################
    tabPanel("Week 2 - Mapping", #tabPanel() creates our first tab
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
             
    ),
    
    ####################################
    #FOURTH PAGE - WEEK 5 - ADVANCED REGULAR EXPRESSIONS
    ####################################

    #navigation bar title
    tabPanel("Week 5 - Advanced Regular Expressions",

             #color for all titles
             tags$head(
                 tags$style("h2 {color: #ee5500; }
                                               h1 {color: #04B4AE}; } ")),

             #first title
             headerPanel( "Crimes Dataset"),
             br(),

             dataTableOutput('weekfivedf'),


    ), #"Week 5 - Advanced Regular Expressions" - fourth page end
    
    ####################################
    #FIFTH PAGE - WEEK 6 - WORD CLOUDS
    ####################################
    
    tabPanel("Week 6 - Word Clouds",
     
     #color for all titles
     tags$head(
         tags$style("h2 {color: #ee5500; }
                    h1 {color: #04B4AE}; } ")),
     
     #first title
     headerPanel( "Word Clouds of Donald Trump and Hilary Clinton (likes and dislikes"),
     
     ####################################
     # LEFT SIDEBAR
     ####################################
     sidebarLayout(position = "left",
                   sidebarPanel(

                     h4("Word Count:"),
                       sliderInput("max",
                                   "Maximum Number of Words:",
                                   min = 0,  max = 300,  value = 100),
                       
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
                                   tabPanel("Donald Trump Likes",
                                            fluidRow(column(12,
                                                h4(align = "center",'Word Cloud of why people ',strong('like'),' Donald Trump'),
                                                
                                                column(width = 12,
                                                       h5(align = "center",strong('Regular Word Cloud Format')),
                                                       plotOutput("trump.like.wordcloud")
                                                       ),
                                                
                                            #     column(width = 3,
                                            #            h5(align = "center",strong('Analysis')),
                                            #                                                                   # wordcloud2Output('trumplikewordcloudWC2'),
                                            # ),
                                            h5(align = "center",strong('Analysis')),
                                            p('In this word cloud, there are many people who have written the word', strong('business'), ' which means that most people probably like Trump because he is a business person and does not come from a political background. This change in preference could indicate the lost trust that individuals have towards political figures or the government which is why they now believe that a business person could fix the issues of their country. The words ``honest, businessman, career, change, jobs, and policy" also pop up which supports what was stated earlier about people wanting a President who does not come from a political background.'),
                                            br(),
                                            p('Additionally, many people seem to like Trump because of the new policies and increase of jobs he has promised to his supporters.')
                                            
                                            
                                            )

                                              ),
                                   ),#first tab panel end
                                   
                                   #####################################
                                   # second tab
                                   #####################################
                                   tabPanel("Donald Trump Dislikes",
                                       fluidRow(column(12,
                                           h4(align = "center",'Word Cloud of why people ',strong('dislike'),' Donald Trump'),
                                           
                                           column(width = 12,
                                                  h5(align = "center",strong('Regular Word Cloud Format')),
                                                  plotOutput("trump.dislike.wordcloud")
                                                  ),
                                           
                                           # column(width = 6,
                                           #        h5(align = "center",strong('WordCloud2 Format')),
                                           #        wordcloud2Output('trump.dislike.wordcloudWC2')
                                           #        ),
                                           
                                           h5(align = "center",strong('Analysis')),
                                           p('One of the top word that have appeared in this word cloud are: ', strong('racist, lack, women, experience, racism, sexist, views, liar, etc.') ,' These words indicate that people dislike Trump because of his views towards certain topics (one example could be his views on building a wall between Mexico). Another thing that was apparent was his lack of ', strong('experience') ,' which could refer to his experience as a politician. Also, due to Trumps comments about women many people call him ', strong('sexist') ,' and think he is not the ideal presidential candidate.')
                                           
                                       )
                                           ),
                                   ),
                                   
                                   #####################################
                                   # third tab
                                   #####################################
                                   tabPanel("Hillary Clinton Likes",
                                            fluidRow(column(12,
                                                h4(align = "center",'Word Cloud of why people ',strong('like'),' Hillary Clinton'),
                                                
                                                
                                                column(width = 12,
                                                       h5(align = "center",strong('Regular Word Cloud Format')),
                                                       plotOutput("clinton.like.wordcloud")
                                                ),
                                                
                                                # column(width = 6,
                                                #        h5(align = "center",strong('WordCloud2 Format')),
                                                #        wordcloud2Output('clinton.like.wordcloudWC2')),

                                                h5(align = "center",strong('Analysis')),
                                                p('One of the biggest words in this word cloud is ', strong('experience') , ' and this makes sense since Hiliary Clinton is a well known politician, unlike Trump. Other words that appear are ', strong('Trump, Women, Qualified, Obama.'), ' Many people support Hiliary Clinton because she is a woman and also with her affiliation/support from Barack Obama by being in the Democratic party.')
                                            )
                                            ),
                                   ),
                                   
                                   #####################################
                                   # fourth tab
                                   #####################################
                                   tabPanel("Hillary Clinton Dislikes",
                                            fluidRow(column(12,
                                                h4(align = "center",'Word Cloud of why people ',strong('dislike'),' Hillary Clinton'),
                                                
                                                
                                                column(width = 12,
                                                       h5(align = "center",strong('Regular Word Cloud Format')),
                                                       plotOutput("clinton.dislike.wordcloud")
                                                ),
                                                
                                                # column(width = 6,
                                                #        h5(align = "center",strong('WordCloud2 Format')),
                                                #        wordcloud2Output('clinton.dislike.wordcloudWC2')
                                                #        ),
                                                
                                                h5(align = "center",strong('Analysis')),
                                                p('Most of the major words in this wordcloud are ', strong('liar, trust, email, abortion, untrustworthy, dishonest, scandal, email, etc.') , ' Most people seem to not like Hillary Clinton due to the email scandal she was a part of and that event decreased her credibility drastically since many people referred to her as a ', strong('liar.'),' Also, many people disagree with Clintons views on certain topics such as abortion.')
                                            )
                                            ),
                                   ),
                                   
                                   tabPanel('Analysis -HW6 Answers',
                                            fluidRow(column(12,
                                                            h4(align = "center",'Analysis/answer to HW questions'),
                                                            
                                                            h3(align = "left",strong('Question 1:'), 'Compare the "like" word clouds for Trump and Obama (from lecture.) Are there similarities/differences? What conclusions do you draw?'),
                                                             
                                                            p('The majority of the like words for Donald Trump had to do with him being a businessman. Also, many people believed that Trump would be able to improve many things in America such as job opportunities and government policies which could indicate an individuals dis-trust with the American government since they believe that a businessman can do a better job in managing a country instead of an established politician.'),
                                                            p('On the other hand, the likewords that appeared in Obamas word cloud were ', strong('change, people, like, democrat, country, health, economy, war, Black etc.') ,' It seems like many people liked Obama because they believed that Obama could improve the American economy (in areas like healthcare, war, etc). Many people also liked the fact that Obama would be the first Black president elected in the United States.'),
                                                            h5(align = 'center',strong('Similarities')),
                                                            p('The similarities between the Obama and Trump word clouds is the belief that both presidential candidates would bring positive change in the U.S economy. In Obamas case, many believed that Obama would bring positive change in healthcare and other policies. While in Trumps case many believed that Trump will bring positive changes in employment.'),
                                                            h5(align = 'center',strong('Differences')),
                                                            p('The differences between the Obama and Trump word clouds has to do with government affiliations. In the Obama word cloud the word ', strong('democrat') ,' was big which showed that many people voted for Obama because he was an experienced politician as well as a democrat. However, this was not the case for Trump and many people liked him because he was not linked with politics and was a businessman his whole life. This change could represent a frustration or mis-trust between U.S citizens and the Government. Many individuals were probably let down by the way major politicians handled certain events which is why they decided to place more trust on a businessman to manage the country.'),
                                                            
                                                            h3(align = "left",strong('Question 2:'), 'Compare the word clouds for Donald Trump and Hilary Clinton. Comment on your findings.'),
                                                            
                                                            p('As mentioned in ', strong('question 1'), ' many people ', strong('liked'), ' Trump because he was a businessman and promised many improvements in the United States economy. Many people ', strong('dis-liked'), ' Trump because they did not like how he talked about certain topics, thought he was racist, and believed he was inexperienced to run for president since he was not an experienced politician.'),
                                                            p('Many people ', strong('liked'), ' Hillary Clinton because of her experience and her views on certain topics. Some people also liked Hillary due to her affiliation with the Democratic party and her support from Barack Obama. Many people ', strong('dis-liked'), ' Hillary Clinton because they lost trust in her due to the email scandal she was involved in. Words like ', strong('liar'), ' were really big in Hillarys dis-like word cloud. Also, some people also disagreed with Hillarys views on certain topics such as abortion. The email scandal that Hillary was involved in severely impacted her credibility as a politician which is why the word ', strong('trust'), ' was really big in the dis-like word cloud.'),
                                                            p('I believe that the email scandal that Hillary Clinton was a part of negatively impacted her reputation which probably prompted more support towards Donald Trump. This probably put more dis-trust in people who were already disappointed or sceptical about the ability of politicians to effectively manage Americas economy/problems which is why the words ', strong('businessman'), ' were in Trumps ', strong('like'), ' word cloud.')
                                                            # 
                                                            )
                                            )
                                            )
                                   
                                   
                       )#all tab panel end
                       
                   )#main tab panel end
                   
     )#left sidebar end
     
    ),
    
    
    )#nav. bar page end
)

########################################################################
#                            SERVER                                    #
########################################################################
server <- function(input, output) {
    
    clinton.likes <- read.csv("redacted_2016_clinton_likes.csv", header = TRUE)
    clinton.dislikes <- read.csv("redacted_clinton_2016_dislikes.csv", header = TRUE)
    trump.likes <- read.csv("redacted_trump_2016.csv", header = TRUE)
    trump.dislikes <- read.csv("redacted_trump_2016_dislikes.csv", header = TRUE)
    
    df <- read.csv("clerylog.csv", header = TRUE, sep=",")
    
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
  
    ####################################
    # ADVANCED REGULAR EXPRESSIONS - WEEK 5 CODE
    ####################################
    df$Nature <- gsub("1 oz","one oz", df$Nature)#I am replacing the part of the description that says '1 oz' to 'one oz'
    grep('one',df$Nature, value = TRUE) #checking if replace was successful
    
    ##removing all offence levels now
    df$Nature <- gsub("[0-9]","", df$Nature)
    
    ##creating dummy variable
    df$theft.dummy<- grepl('theft',df$Nature, ignore.case = TRUE)
    
    
    ##creating dummy variable
    df$DUI.dummy<- grepl('dui',df$Nature, ignore.case = TRUE)
    
    ##creating dummy variable
    df$drug.dummy<- grepl('drug|marijuana|heroin|meth|cocaine',df$Nature, ignore.case = TRUE)
    
    ########Problem  3: Use gregexpr() to  find observations that are associated
    ########            with either heroin, marijuana, or meth
    drug.matches <- gregexpr('drug|marijuana|heroin|meth|cocaine',df$Nature, ignore.case = TRUE)
    df$three.drug <- regmatches(df$Nature, drug.matches)
    
    df$threecheck.drug <- ifelse((df$three.drug != 'character(0)'),TRUE,FALSE)
    
    ########Problem 4: Create a new column with JUST the date (or date range) the crime occurred
    df$date.occured.only <- df$Date.Time.Occurred
    
    ##removing time
    #grep(' [0-9]{4}',df$date.occured.only, value = TRUE) #viewing all variables that have time
    df$date.occured.only <- gsub(' [0-9]{4}','',df$date.occured.only)
    
    #grep(' [0-9]{2}:[0-9]{2}| [0-9]{1}:[0-9]{2}',df$date.occured.only, value = TRUE) #viewing all variables that have time with a colon + one or two digits as the first number
    df$date.occured.only <- gsub(' [0-9]{2}:[0-9]{2}| [0-9]{1}:[0-9]{2}','',df$date.occured.only)
    
    #removing dash
    #grep('-',df$date.occured.only, value = TRUE) #viewing dates that have a dash (both, dates with and without a range)
    
    #dates with a range
    #grep(' - ',df$date.occured.only, value = TRUE) #viewing dates with a range ONLY
    df$date.occured.only <- gsub(' - ','@',df$date.occured.only)
    
    #dates without a range
    #grep(' -',df$date.occured.only, value = TRUE) #viewing dashes WITHOUT a date range
    df$date.occured.only <- gsub(' -','',df$date.occured.only)
    
    #adding a '-' to dates with a range
    df$date.occured.only <- gsub('@','-',df$date.occured.only) #I replaced @ with a dash
    df$date.occured.only <- gsub('- ','-',df$date.occured.only) #I noticed one dash had a additional space and just removed that space so all the dashes can look consistent
    
    #checking and removing 'report pending' status
    #grep('[a-z]',df$date.occured.only, value = TRUE,ignore.case = TRUE) #displaying all values that are words
    df$date.occured.only <- gsub('[a-z]',NA,df$date.occured.only,ignore.case = TRUE)
    
    #now 2014 is a year but no specifc date was given. I left it in since the year is a part of the date
    
    #here is the code if I needed to remove it
    # grep('[0-9]{3}',df$date.occured.only, value = TRUE,ignore.case = TRUE)
    # table(nchar(df$date.occured.only) == 4)
    # 
    # date.character.count <- nchar(df$date.occured.only) == 4
    # df$date.occured.only <- ifelse((date.character.count == TRUE),NA,df$date.occured.only)
    
    output$weekfivedf <- renderDataTable({ df })
    

    ####################################
    # WORD CLOUD - WEEK 6 CODE
    ####################################
    
    ###### TRUMP LIKES  
    names(trump.likes)[1] <- 'Respondent ID'
    names(trump.likes)[2] <- 'What do you like about Trump?'
    
    trump.likes.words <-trump.likes %>% 
      unnest_tokens(word, `What do you like about Trump?`) 
    
    trump.likes.wordsC <- trump.likes.words %>% 
      anti_join(stop_words) %>%
      count(word, sort= TRUE)
    
    
    
    pal <- brewer.pal(6,"Dark2")
    output$trump.like.wordcloud <- renderPlot({
      wordcloud(words = trump.likes.wordsC$word,
                freq  = trump.likes.wordsC$n,
                max.words = input$max,
                random.order = FALSE,
                colors = pal)
    })
    
    
    output$trumplikewordcloudWC2 <- renderWordcloud2({
      wordcloud2(data=trump.likes.wordsC[0:input$max, ])
    })


   
    ###### TRUMP DIS-LIKES 
    names(trump.dislikes)[1] <- 'Respondent ID'
    names(trump.dislikes)[2] <- 'What do you dis-like about Trump?'
    
    trump.dislikes.words <-trump.dislikes %>% 
      unnest_tokens(word, `What do you dis-like about Trump?`) 
    
    trump.dislikes.wordsC <- trump.dislikes.words %>% 
      anti_join(stop_words) %>%
      count(word, sort= TRUE)
    
    pal <- brewer.pal(6,"Dark2")
    output$trump.dislike.wordcloud <- renderPlot({
      wordcloud(words = trump.dislikes.wordsC$word,
                freq  = trump.dislikes.wordsC$n,
                max.words = input$max,
                random.order = FALSE,
                colors = pal)
    })
    
    output$trump.dislike.wordcloudWC2 <- renderWordcloud2({
      wordcloud2(data=trump.dislikes.wordsC[0:input$max, ])
    })
    
    ###### CLINTON LIKES  
    names(clinton.likes)[1] <- 'Respondent ID'
    names(clinton.likes)[2] <- 'What do you like about Clinton?'
    
    clinton.likes.words <-clinton.likes %>% 
      unnest_tokens(word, `What do you like about Clinton?`) 
    
    clinton.likes.wordsC <- clinton.likes.words %>% 
      anti_join(stop_words) %>%
      count(word, sort= TRUE)
    
    pal <- brewer.pal(6,"Dark2")
    output$clinton.like.wordcloud <- renderPlot({
      wordcloud(words = clinton.likes.wordsC$word,
                freq  = clinton.likes.wordsC$n,
                max.words = input$max,
                random.order = FALSE,
                colors = pal)
    })
    
    output$clinton.like.wordcloudWC2 <- renderWordcloud2({
      wordcloud2(data=clinton.likes.wordsC[0:input$max, ])
    })
    
    ###### CLINTON DISLIKES 
    
    names(clinton.dislikes)[1] <- 'Respondent ID'
    names(clinton.dislikes)[2] <- 'What do you dis-like about Clinton?'
    
    clinton.dislikes.words <-clinton.dislikes %>% 
      unnest_tokens(word, `What do you dis-like about Clinton?`) 
    
    clinton.dislikes.wordsC <- clinton.dislikes.words %>% 
      anti_join(stop_words) %>%
      count(word, sort= TRUE)
    
    pal <- brewer.pal(6,"Dark2")
    output$clinton.dislike.wordcloud <- renderPlot({
      wordcloud(words = clinton.dislikes.wordsC$word,
                freq  = clinton.dislikes.wordsC$n,
                max.words = input$max,
                random.order = FALSE,
                colors = pal)
    })
    
    output$clinton.dislike.wordcloudWC2 <- renderWordcloud2({
      wordcloud2(data=clinton.dislikes.wordsC[0:input$max, ])
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



# 
# ui <- fluidPage(
#   fluidRow(
#     column(12,
#            "Fluid 12",

#            fluidRow(
#              column(6,
#                     "Fluid 6",

#                     fluidRow(
#                       column(6, 
#                              "Fluid 6"),
#                       column(6,
#                              "Fluid 6")
#                     )
#              ),
#              column(width = 6,
#                     "Fluid 6")
#            )
#     )
#   )
# )

#choice <- reactive({ input$wordcloud.decision })
# output$trumplikewordcloud <- renderPlot({
#   if ( choice()=='Regular Word Cloud') {
# 
#     wordcloud(words = trump.likes.wordsC$word,
#               freq  = trump.likes.wordsC$n,
#               max.words = input$max,
#               random.order = FALSE,
#               colors = pal)
# 
# } else if ( choice()=='Wordcloud2') {
# 
#     #wordcloud2(data=trump.likes.wordsWC2[0:input$max, ])
# 
# }
# })
