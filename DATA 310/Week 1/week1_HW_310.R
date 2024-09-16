#install.packages('stargazer')

library(readr)
library(tidyverse)
library(stargazer)

#(a) Read in the dataset AlabamaCourt.csv
setwd("C:/Users/hussainsarfraz/Desktop/DATA 310")
alabama.court <-  read_csv('AlabamaCourt.csv')


#(b) Construct two new variables:
#. A variable, "black", indicating whether the person convicted of the crime is
#  African-American. The persons's race is represented by the variable "race",
#  with this variable being equal to "B' when the person is African-American.
#. A variable, "amountremain", indicating how much money the person convicted
#  of the crime still owes to the state of Alabama. This is can be constructed by 
#  subtracting the variable "amountpaid", which represents how
#  much money the person had paid to date, from the variable "amountdue",
#  which represents the total amount of the assessed LFOs.

alabama.court <- alabama.court %>%
  mutate(black = ifelse((race=='B'), 1, 0),
         amountremain = amountdue - amountpaid)

#(c) Use Stargazer to create two descriptive statistics tables. One describing the
#    African-American units in the sample and one describing the non-African-Americans
#    units in the data. 
#    In this table, include three variables: "amountremain", "amountdue", "amountpaid". 
#    Make sure to label the three variables in these tables so that
#    someone could understand the variable that is being described.

### Checking if dataset is tibble |output:TRUE
#is_tibble(alabama.court)

### checking total number of rows |output:2926
#nrow(alabama.court)

### double checking row number |output:1366
# subset(as.data.frame(alabama.court), 
#        alabama.court$race == 'B',
#        select = c(amountremain,amountdue,amountpaid)) %>%
#   nrow()

### African Americans
stargazer(subset(as.data.frame(alabama.court), 
                 alabama.court$race == 'B',
                 select = c(amountremain,amountdue,amountpaid)),
          type = 'text',
          digits = 2,
          title = "Legal Financial Obligations (LFOs) in Alabama Court Cases for African Americans",
          covariate.labels = c('Remaining LFOs','LFOs Assesed','LFOs Paid to Date'))

### double checking row number (2926-1366=1560) |output:1560
# subset(as.data.frame(alabama.court),
#        alabama.court$race != 'B',
#        select = c(amountremain,amountdue,amountpaid)) %>%
#   nrow()

### non-African Americans
stargazer(subset(as.data.frame(alabama.court),
                 alabama.court$race != 'B',
                 select = c(amountremain,amountdue,amountpaid)),
          type = 'text',
          digits = 2,
          title = "Legal Financial Obligations (LFOs) in Alabama Court Cases for non-African Americans",
          covariate.labels = c('Remaining LFOs','LFOs Assesed','LFOs Paid to Date'))


# (d) Create three kernel density plots that compare the distributions of the
#     variables "amountremain", "amountdue", "amountpaid", respectively, for 
#     African Americans and non-African-Americans.

# To remove the scientific notation in the graphs
options(scipen=999)

# histogram and kernal density plot for the remaining LFO to be paid
## for African-Amercans
# alabama.court %>%
#   subset(.,
#          alabama.court$race == 'B',
#          select = c(amountremain)) %>%
#   mutate(amountremain1 = amountremain + 1) %>%
#   ggplot(., aes(amountremain1)) +
#   geom_density() +
#   scale_x_continuous( trans='log10',
#                       breaks = c(1,10,100,1000,100000)) +
#   xlab("$ of LFOs Remaining for African Americans") +
#   ylab('Number of Cases')


## for non-African Americans

# alabama.court %>%
#   subset(.,
#          alabama.court$race != 'B',
#          select = c(amountremain)) %>%
#   mutate(amountremain1 = amountremain + 1) %>%
#   ggplot(., aes(amountremain1)) +
#   geom_density() +
#   scale_x_continuous( trans='log10',
#                       breaks = c(1,10,100,1000,100000)) +
#   xlab("$ of LFOs Remaining for non-African Americans") +
#   ylab('Number of Cases')


#combined
alabama.court %>%
  mutate(amountremain1 = amountremain + 1) %>%
  ggplot(.) +
  geom_density(aes(x = amountremain1,group=black,
                       fill=as.factor(black)),
               alpha=0.3) +
  scale_fill_manual(values = c('red','blue'),
                    name="Race Key",
                    labels=c("Non-African Americans",
                             "African Americans")) +
  scale_x_continuous( trans='log10',
                      breaks = c(1,10,100,1000,10000,100000)) +
  xlab("$ of LFOs Remaining") +
  ylab('Number of Cases') +
  ggtitle("$ of LFOs Remaining for Convicted Criminals") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 


# histogram and kernal density plot for the LFOs assesed
## for African-Amercans
# alabama.court %>%
#   subset(.,
#          alabama.court$race == 'B',
#          select = c(amountdue)) %>%
#   mutate(amountdue1 = amountdue + 1) %>%
#   ggplot(., aes(amountdue1)) +
#   geom_density() +
#   scale_x_continuous( trans='log10',
#                       breaks = c(1,10,100,1000,100000)) +
#   xlab("$ of LFOs Assesed for African-Amercans") +
#   ylab('Number of Cases')

## for non-African Americans
# alabama.court %>%
#   subset(.,
#          alabama.court$race != 'B',
#          select = c(amountdue)) %>%
#   mutate(amountdue1 = amountdue + 1) %>%
#   ggplot(., aes(amountdue1)) +
#   geom_density() +
#   scale_x_continuous( trans='log10',
#                       breaks = c(1,10,100,1000,100000)) +
#   xlab("$ of LFOs Assesed for non-African Americans") +
#   ylab('Number of Cases')

#combined
alabama.court %>%
  mutate(amountdue1 = amountdue + 1) %>%
  ggplot(.) +
  geom_density(aes(x = amountdue1,group=black,
                   fill=as.factor(black)),
               alpha=0.3) +
  scale_fill_manual(values = c('red','blue'),
                    name="Race Key",
                    labels=c("Non-African Americans",
                             "African Americans")) +
  scale_x_continuous( trans='log10',
                      breaks = c(1,10,100,1000,10000,100000)) +
  xlab("$ of LFOs Assesed") +
  ylab('Number of Cases') +
  ggtitle("$ of LFOs Assesed for Convicted Criminals") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# histogram and kernal density plot for the LFOs paid to date
## for African-Amercans
# alabama.court %>%
#   subset(.,
#          alabama.court$race == 'B',
#          select = c(amountpaid)) %>%
#   mutate(amountpaid1 = amountpaid + 1) %>%
#   ggplot(., aes(amountpaid1)) +
#   geom_density() +
#   scale_x_continuous( trans='log10',
#                       breaks = c(1,10,100,1000,10000,100000)) +
#   xlab("$ of LFOs Paid to Date for African Americans") +
#   ylab('Number of Cases')

## for non-African Americans
# alabama.court %>%
#   subset(.,
#          alabama.court$race != 'B',
#          select = c(amountpaid)) %>%
#   mutate(amountpaid1 = amountpaid + 1) %>%
#   ggplot(., aes(amountpaid1)) +
#   geom_density() +
#   scale_x_continuous( trans='log10',
#                       breaks = c(1,10,100,1000,100000)) +
#   xlab("$ of LFOs Paid to Date for non-African Americans") +
#   ylab('Number of Cases')

#combined
alabama.court %>%
  mutate(amountpaid1 = amountpaid + 1) %>%
  ggplot(.) +
  geom_density(aes(x = amountpaid1,group=black,
                   fill=as.factor(black)),
               alpha=0.3) +
  scale_fill_manual(values = c('red','blue'),
                    name="Race Key",
                    labels=c("Non-African Americans",
                             "African Americans")) +
  scale_x_continuous( trans='log10',
                      breaks = c(1,10,100,1000,10000,100000)) +
  xlab("$ of LFOs Paid to Date") +
  ylab('Number of Cases') +
  ggtitle("$ of LFOs Paid for Convicted Criminals") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# (e) Write a paragraph or two, in which you make conclusions about whether there
# are racial differences in this sample in the amount of LFOs that people convicted
# of crimes owe to the state of Alabama based on the data being summarized in the
# previous parts of this question.


# 3. Please answer the following questions using R:

college.basketball <- read_csv('CollegeBasketball.csv')

#(a) Make a new variable that is equal to "PredictedDifference" minus "ActualDifference". 
#    Create a histogram that shows the distribution of this variable. 
#    Also report the mean of the absolute value of this variable.

college.basketball <- college.basketball %>%
  mutate(predicted.diff_result = PredictedDifference - ActualDifference) 

ggplot(data = college.basketball) +
  geom_histogram(aes(x = predicted.diff_result)) +
  scale_x_continuous(breaks = c(-40,-30,-20,-10,0,10,20,30,40)) +
  xlab("Difference of Game Predictions minus Actual Result") +
  ylab('Number of Times Differnce Occuered') +
  theme_bw() +
  ggtitle('Game Prediction Differences') +
  theme(plot.title = element_text(hjust = 0.5)) 

# how far points are from line of regression
mean(abs(college.basketball$predicted.diff_result))

# (b) Let W represent the event that the favorite won a basketball game by more points
# than expected, E represent the event that the favorite won a basketball game by
# exactly the number of points that were expected, and L be the event that the
# favorite won a basketball game by fewer points than expected or lost a basketball
# game. Make new variables indicating whether these events occurred in each game
# and use these variables to calculate within the sample P(W), P(E), and P(L).
# 2

college.basketball <- college.basketball %>%
  mutate(W = ifelse((predicted.diff_result <  0),1,0),
         E = ifelse((predicted.diff_result == 0),1,0),
         L = ifelse((predicted.diff_result >  0),1,0)) 

#P(W)
mean(college.basketball$W)

#P(E)
mean(college.basketball$E)

#P(L)
mean(college.basketball$L)

# (c) Make a new variable that is equal to "PredictedPoints" minus "ActualPoints"
# Create a histogram that shows the distribution of this variable. 
#Also report the mean of the absolute value of this variable.

college.basketball <- college.basketball %>%
  mutate(predicted.point_result = PredictedPoints - ActualPoints)

ggplot(data = college.basketball) +
  geom_histogram(aes(x = predicted.point_result)) +
  scale_x_continuous(breaks = c(-40,-20,0,20,40,60)) +
  xlab("Difference of Point Predictions minus Actual Result") +
  ylab('Number of Times Differnce Occurred') +
  theme_bw() +
  ggtitle('Game Point Differences') +
  theme(plot.title = element_text(hjust = 0.5)) 

# how far points are from line of regression
mean(abs(college.basketball$predicted.point_result))

# (d) Let M represent the event that more combined points were scored than expected,
# T represent the event that the combined points scored were exactly the number
# expected, and F represent the event that fewer combined points were scored than
# expected. Make new variables indicating whether these events occurred in each
# game and use these variables to calculate within the sample P(M), P(T), and
# P(F).

college.basketball <- college.basketball %>%
  mutate( M =  ifelse((predicted.point_result <  0),1,0),
         'T' = ifelse((predicted.point_result == 0),1,0),
         'F' = ifelse((predicted.point_result >  0),1,0)) 

#P(M)
mean(college.basketball$M)

#P(T)
mean(college.basketball$'T')

#P(F)
mean(college.basketball$'F')

# (e) Using the conditional probability formula discussed in lecture, calculate within
# the sample P(W | M), P(L | M), P(W | F), P(L | F).

# #P(W)
# mean(college.basketball$W)
# #P(L)
# mean(college.basketball$L)
# 
# #P(M)
# mean(college.basketball$M)
# #P(F)
# mean(college.basketball$'F')

#P(W | M)
(mean(college.basketball$M)*mean(college.basketball$W))/mean(college.basketball$M)
  
#P(L | M)
(mean(college.basketball$M)*mean(college.basketball$L))/mean(college.basketball$M)
  
#P(W | F)
(mean(college.basketball$'F')*mean(college.basketball$W))/mean(college.basketball$'F')
  
#P(L | F)
(mean(college.basketball$'F')*mean(college.basketball$L))/mean(college.basketball$'F')
  

# (f) Write a paragraph or two, in which you make conclusions about whether the
# evidence is consistent with my theory based on the data being summarized in the
# previous parts of this question.