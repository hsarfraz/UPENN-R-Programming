library(readr)
library(readxl)
library(tidyverse)
library(dplyr)

#Question 1
#For this question, you will investigate the relationship between education completion
#rates and poverty levels. The data is currently found in two separate files. You will
#follow the steps outlined below to prepare the data, merge the data, and then analyze
#the data.
#1. Read in 'education_long.csv'

setwd("C:/Users/hussainsarfraz/Desktop/DATA 210")
education_long <-  read_csv('education_long.csv')

view(education_long)

#2. Reshape the data into 'wide format'

education_long_tidy <- spread(education_long,
                              key = Year,
                              value = College_Completion_Rate)

view(education_long_tidy)

#3. Read in sheet 2 of the file 'PovertyReport.xlsx'

poverty_report_sheet2 <-  read_xlsx('PovertyReport.xlsx', sheet=2)

view(poverty_report_sheet2)

#4. The poverty data was collected in 2017. Select rows from the education dataset from
#2017.

education_long_tidy <- education_long_tidy %>%
  select('Name',
         `2013-2017_Rural`,
         `2013-2017_Total`,
         `2013-2017_Urban`)

#5. Merge these two data sets so that all matched observations are retained.

education_poverty_join <- merge(x = education_long_tidy, 
                                y = poverty_report_sheet2, 
                                by = 'Name')

view(education_poverty_join)

"
anti_join(x = education_long_tidy, 
          y = poverty_report_sheet2, 
          by = 'Name')
"

#6. Graphically investigate the relationship between general poverty levels and the
#college completion rates for each demographic group. What do you notice?

education_poverty_join %>%
  ggplot() +
  geom_point(mapping = aes(x=Percent,y=`2013-2017_Rural`,color='blue')) +
  geom_point(mapping = aes(x=Percent,y=`2013-2017_Total`,color='red')) +
  geom_point(mapping = aes(x=Percent,y=`2013-2017_Urban`,color='green')) +
  xlab("Poverty Percntage Rate") + 
  ylab("College Completion Rate") +
  ggtitle("College Completion Rate for each Poverty Level/Demographic Group") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 10)) +
  scale_color_manual(values = c('blue','green','red'),
                     name="Legend", 
                     labels=c(
                       'blue' = 'Rural Residents',
                       "green" = 'Total Residents',
                       "red" = 'Urban Residents'))

education_poverty_join %>%
  ggplot(mapping = aes(x=Percent,y=`2013-2017_Rural`)) +
  geom_point(color='blue') +
  geom_smooth(color='black') +
  xlab("Poverty Percentage Rate") + 
  ylab("College Completion Rate") +
  ggtitle("College Completion Rate for Rural Individuals") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 10)) 
  
  education_poverty_join %>%
    ggplot(mapping = aes(x=Percent,y=`2013-2017_Total`)) +
    geom_point(color='red') +
    geom_smooth(color='black') +
    xlab("Poverty Percentage Rate") + 
    ylab("College Completion Rate") +
    ggtitle("College Completion Rate for Rural and Urban Individuals") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          plot.title = element_text(size = 10)) 
  
  education_poverty_join %>%
    ggplot(mapping = aes(x=Percent,y=`2013-2017_Urban`)) +
    geom_point(color='dark green') +
    geom_smooth(color='black') +
    xlab("Poverty Percentage Rate") + 
    ylab("College Completion Rate") +
    ggtitle("College Completion Rate for Urban Individuals") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          plot.title = element_text(size = 10)) 

#Question 2
#For this question, you will investigate the relationship of the racial demographics of a
#school and the critical reading score on the SAT.
#1. Load the datasets profiling NYC schools from 2006-2012 and NYC schools 2010 SAT
#scores, respectively.

NYC_2006_2012 <- read_csv('2006_-_2012_School_Demographics_and_Accountability_Snapshot.csv')
NYC_2010_SAT <- read_csv('SAT__College_Board__2010_School_Level_Results.csv')

# view(NYC_2006_2012)
# view(NYC_2010_SAT)

#2. For this problem, we want to merge the 2010 SAT dataset with the the NYC school
#profiles for that year. First, subset the NYC school profile so that it only includes
#data from the 2009/2010 school year.

NYC_2009_2010 <- NYC_2006_2012 %>%
  filter(schoolyear == '20092010')

# view(NYC_2009_2010)

#3. If you look at the dimensions of the data, it seems that there are substantially
#more schools included in the profile2010 data. Look through both datasets carefully
#to see why this might be (Hint: search for areas where there are missing values)
--------
#This is because the NYC_2009_2010 dataset includes all type of schools from
#2009-2010. High schoolers take the SAT which means that in order for the 
#NYC_2009_2010 dataset to have the same dimensions as the NYC_2010_SAT dataset
#I would need to filter the NYC_2009_2010 dataset to only include high schools
  
NYC_2009_2010 %>% 
  filter(is.na(grade9) | is.na(grade10)| is.na(grade11) | is.na(grade12)) %>%
  transmute(grade7,grade8,grade9,grade10,grade11,grade12) %>%
  arrange(desc(grade10))

NYC_2009_2010 %>% 
  filter(!is.na(grade9) & !is.na(grade10) & !is.na(grade11) & !is.na(grade12)) %>%
  transmute(grade7,grade8,grade9,grade10,grade11,grade12) %>%
  arrange(desc(grade10))



#4. Remove the values that do not belong.

NYC_2009_2010 <- NYC_2009_2010 %>% 
  filter(!is.na(grade9) | !is.na(grade10) | !is.na(grade11) | !is.na(grade12)) 

NYC_2009_2010$prek <- NYC_2009_2010$k <- NYC_2009_2010$grade1 <- NULL  
NYC_2009_2010$grade2 <- NYC_2009_2010$grade3 <- NYC_2009_2010$grade4 <- NULL 
NYC_2009_2010$grade5 <- NYC_2009_2010$grade6 <- NYC_2009_2010$grade7 <- NULL 
NYC_2009_2010$grade8 <- NULL
'
duplicated(NYC_2009_2010$DBN) %>%
  table()
  

NYC_2009_2010 <- NYC_2009_2010 %>% 
  filter(!is.na(grade9), !is.na(grade10), !is.na(grade11), !is.na(grade12))
  
NYC_2009_2010$c(7:16)
NYC_2009_2010$prek[NYC_2009_2010$prek] <- NA

NYC_2009_2010 <- NYC_2009_2010 %>%
  filter(prek==is.na())

table(NYC_2006_2012$schoolyear)
sort(table(NYC_2006_2012$schoolyear), decreasing = T)
'

#5. Merge the datasets. Which variable is the most appropriate to use as a unique
#identifier?

# The DBN code is the most appropriate variable to use as a unique identifier since 
#it contains all the unique codes of all the schools 
dim(NYC_2009_2010)

anti_join(x = NYC_2009_2010, 
          y = NYC_2010_SAT, 
          by = 'DBN',
          all = F) %>%
  view()

NYC_2009_2010 <- merge(x = NYC_2009_2010, 
              y = NYC_2010_SAT, 
              by = 'DBN',
              all = F)

view(NYC_2009_2010)

#6. Now, let's explore this data a bit. Say we want to see how the racial demographics
#of a school impact the school's average critical reading score on the SAT. Plot these
#two variables for each race on four different graphs and discuss findings.
NYC_2009_2010$asian_per

NYC_2009_2010 %>% 
  mutate(`School Name` = str_to_upper(`School Name`)) %>%
  filter(`School Name` != Name) %>%
  view()

NYC_2009_2010 %>%
  ggplot() +
  geom_point(mapping = aes(x=hispanic_per,y=`Critical Reading Mean`,color='blue')) +
  geom_point(mapping = aes(x=black_per,y=`Critical Reading Mean`,color='brown')) +
  geom_point(mapping = aes(x=white_per,y=`Critical Reading Mean`,color='purple')) +
  geom_point(mapping = aes(x=asian_per,y=`Critical Reading Mean`,color='red')) +
  xlab("Percentage of Race in School") + 
  ylab("School Average SAT Critical Reading Score") +
  ggtitle("Performace of different Races on the SAT Reading Section") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 10)) +
  scale_color_manual(values = c('blue','brown','purple','red'),
                     name="Races", 
                     labels=c(
                       'blue' = 'Hispanic',
                       "brown" = 'Black',
                       'purple' = 'White',
                       "red" = 'Asian'))

NYC_2009_2010 %>%
  ggplot(mapping = aes(x=hispanic_per,y=`Critical Reading Mean`)) +
  geom_point(color='blue') +
  geom_smooth(color='black') +
  xlab("Percentage of Race in School") + 
  ylab("School Average SAT Critical Reading Score") +
  ggtitle("Performace of Hispanics on the SAT Reading Section") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 10)) 

NYC_2009_2010 %>%
  ggplot(mapping = aes(x=black_per,y=`Critical Reading Mean`)) +
  geom_point(color='brown') +
  geom_smooth(color='black') +
  xlab("Percentage of Race in School") + 
  ylab("School Average SAT Critical Reading Score") +
  ggtitle("Performace of Hispanics on the SAT Reading Section") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 10)) 

NYC_2009_2010 %>%
  ggplot(mapping = aes(x=white_per,y=`Critical Reading Mean`)) +
  geom_point(color='purple') +
  geom_smooth(color='black') +
  xlab("Percentage of Race in School") + 
  ylab("School Average SAT Critical Reading Score") +
  ggtitle("Performace of Hispanics on the SAT Reading Section") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 10)) 

NYC_2009_2010 %>%
  ggplot(mapping = aes(x=asian_per,y=`Critical Reading Mean`)) +
  geom_point(color='red') +
  geom_smooth(color='black') +
  xlab("Percentage of Race in School") + 
  ylab("School Average SAT Critical Reading Score") +
  ggtitle("Performace of Hispanics on the SAT Reading Section") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 10)) 
