#Problem 1
#We are going to work again the ACS County Data to investigate the relationship between
#median household income and the percent of children living in poverty in counties. 
#Load in the "ACSCountyData.Rdata" dataframe.

load('C:/Users/hussainsarfraz/Desktop/DATA 310/Week 6/ACSCountyData (1).Rdata')
library(tidyverse)

#(a) First, to make things more readable, recode the median.income variable to be 
#    expressed in thousands of dollars.

acs$median.income <- acs$median.income / 1000

#(b) Plot the relationship between median income on the x axis and percent child
#    poverty on the y axis and describe what you see.

ggplot(data=acs) +
  geom_point(aes(x = median.income, y = percent.child.poverty)) + 
  xlab("Median Income (Thousands)") +
  ylab("Percentage of Child Poverty") +
  ggtitle("Relationship between Median Income and Child Poverty") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 10))

# (c) Run a bi-variate linear regression on this relationship, discuss what the
#     coefficients (including the intercept) mean, and visualize the result on top 
#     of the scatterplot you produced above. 

#     Just to make things easier for the next step, you may want to use code 
#     similar to this to plot the result:
    p <- ggplot(acs, aes(x=median.income, y=percent.child.poverty)) + geom_point() +
         ylim(0,100) +
         labs(x="Median Income (Thousands)", y = "Percent Childen in Poverty") +
         geom_smooth(method = lm, formula = y ~ poly(x, 1), se = FALSE)

summary(lm(percent.child.poverty~median.income, data=acs))

ggplot(data=acs,aes(x = median.income, y = percent.child.poverty)) +
  geom_point() + 
  geom_smooth(method=lm, formula = y ~ poly(x, 1), se = FALSE) +
  xlab("Median Income (Thousands)") +
  scale_x_continuous(breaks=c(0,25,50,75,100,125,150)) + 
  ylab("Percentage of Child Poverty") +
  ggtitle("Relationship between Median Income and Child Poverty") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 10))

#(d) Looking at this relationship visually, why doesn't this regression satisfy 
#    Gauss-Markov Assumption 2 (functional form)? Use the poly() function to add
#    the square of median.income to your model and determine whether this improves
#    model fit, making reference to both the visual change in the regression line 
#    and to the R2 of each model.

summary(lm(percent.child.poverty~poly(median.income,2,raw=T), data=acs))

ggplot(data=acs,aes(x = median.income, y = percent.child.poverty)) +
  geom_point() + 
  geom_smooth(method=lm, formula = y ~ poly(x, 2), se = FALSE) +
  xlab("Median Income (Thousands)") +
  ylab("Percentage of Child Poverty") +
  scale_x_continuous(breaks=c(0,25,50,75,100,125,150)) + 
  ggtitle("Relationship between Median Income and Child Poverty") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 10))

#(e)In this new regression with a second order polynomial term, what is the the
#   effect of an additional $1000 in median income when median income is at $30k?
#   What is the the effect of an additional $1000 in median income when median 
#   income is at $100k? Does this make theoretical sense?

################written response (see notes)

#(f)A possible confounding variable to this relationship is the unemployment rate,
#   which may affect both the median income of a county and the percent of 
#   children living in poverty. Use the cor() function to investigate the
#   relationships between median income, unemployment, and child poverty. Based on
#   the patten of correlations, what is likely to happen to the coefficient on
#   median.income if you add unemployment rate to the first regression model (the
#   one without the polynomial terms)?

summary(lm(percent.child.poverty~median.income + unemployment.rate, data=acs))

cor(acs$median.income, acs$unemployment.rate, use="pairwise.complete") #-0.4990148

cor(acs$median.income, acs$percent.child.poverty, use="pairwise.complete") #-0.7448909

cor(acs$unemployment.rate, acs$percent.child.poverty, use="pairwise.complete") #0.6814938


#(g)Run this regression with unemployment rate and median income (no polynomial
#   terms), and determine the degree to which the coefficient on median.income 
#   changes. Interpret the other coefficients in the model as well, being sure to
#   adjust your language to the fact that there are now multiple indpeendent
#   variables.


#old thing
summary(lm(percent.child.poverty~median.income, data=acs))

#new thing
summary(lm(percent.child.poverty ~ median.income + unemployment.rate, data = acs))


m2 <- lm(percent.child.poverty ~ median.income + unemployment.rate, data = acs)
summary(m2)


#(h)Another possible confounding variable is the census region people are living 
#   in. For example, living in the south could be associated with both lower 
#   average incomes and more child poverty. Create an indicator variable for the
#   4 census regions (or change the variable into a factor variable) and then
#   re-estimate the regression with median income and unemployment to take into 
#   account what region each county is in. Interpret the coefficients from this
#   regression.

unique(acs$census.region)
#type is character, convert to factor
class(acs$census.region)

acs$census.region <- factor(acs$census.region)
summary(lm(percent.child.poverty ~ median.income + unemployment.rate +
             census.region, 
           data = acs))





#(i)It's possible that the effect of median income is different conditional on 
#   whether a county is urban or not. Create an indicator variable for whether a
#   county is urban (population density greater or equal to 1000) or not. Interact
#   this variable with median income in the regression with unemployment rate
#   and census region indicators. Interpret the coefficients on median income, the
#   urban indicator, and the interaction term.

acs$population.density <- (acs$population.density >= 1000)

#convert class to factor
class(acs$population.density)

acs$population.density <- factor(acs$population.density)

summary(lm(percent.child.poverty ~ median.income + unemployment.rate +
                           census.region + population.density, 
                         data = acs))

## This was to test the coef() function
# regression <- summary(lm(percent.child.poverty ~ median.income + unemployment.rate +
#              census.region + population.density, 
#            data = acs))
# 
# coef(regression)["median.income"]

