#----------------------------------------
# Homework Week 6 Answer Key
#----------------------------------------
rm(list=ls())
#We're going to use the ACS county data to further think about multivariate regression

#First load in the ACS county data, which is data on a number of characteristics
#of counties
setwd("C:/Users/Marc/Dropbox/PORES/DATA Certificate/DATA 310 (201)/Week 6/13. Homework")
load("ACSCountyData.Rdata")

#Let's look at the relationship between the median household income (in thousands of dollars)
#and the percent of children living in poverty in the county

#(a) First, re-code median.income to be in thousands of dollars

acs$median.income <- acs$median.income/1000


#(b) Plot the relationship between these two variables and describe what you see
library(ggplot2)
p <- ggplot(acs, aes(x=median.income, y=percent.child.poverty)) + geom_point() + 
  ylim(0,100) +
  labs(x="Median Income (Thousands)", y = "Percent Childen in Poverty") +
  geom_smooth(method = lm, formula = y ~ poly(x, 1), se = FALSE)
p

#This is, predictably, a negative relationship. As the median income
#of a county increases, the percent of childen living in poverty decreases
#Which makes sense! Although the relationship does seem to not be linear...


#(c) Run a bi-variate linear regression on this relationship, discuss
#what the coefficients mean, and visualize the result
m1 <- lm(percent.child.poverty ~ median.income, data=acs)
summary(m1)

#For every 1000 dollars of additional income in a county, child poverty
#decreases at by .61%. 

#When median income is at 0 (not a real number), expected child poverty is 
#at 53%

#(d) Looking at this relationship visually, does this regression satisfy Gauss-Markov
#assumption (2) functional form?
#Add the square of median income to your model and
#determine whether this improves model fit visually and by making reference to the R^2


m2 <- lm(percent.child.poverty ~ poly(median.income,2, raw=T),data=acs )
summary(m2)

# The R2 of this model is .70, which is a substantial improvement in the model fit 
# versus when income was not squared. Approximately 15 percentage points more variance 
# in child poverty is explained from adding the squared term

p <- ggplot(acs, aes(x=median.income, y=percent.child.poverty)) + geom_point() + 
  ylim(0,100) +
  labs(x="Median Income (Thousands)", y = "Percent Childen in Poverty") +
  geom_smooth(method = lm, formula = y ~ poly(x, 2), se = FALSE)
p

#This is borne out visually as well, the regression now much closely fits the data.
#However, because of the functional form of having a squared term, the regression line
#does predict that additional median income will lead to more child poverty after around
#70k.


#(e) In this new regression with a second order polynomial term, what is the effect of 
#an additional 1000 of median income at 30k? What is the effect at 100k?
#Does this theoretically make sense?
#What is the intercept in this new model, what do we make of this?

coef(m2)
coef(m2)[2] + 2*coef(m2)[3]*30

#When median income is at 30k, an additional $1000 of median household income leads to a 
#1.22 point decline in the percent of children living in poverty in the county. 

coef(m2)[2] + 2*coef(m2)[3]*100

#When median income is at 100k, an additional $1000 of median household income leads to a 
#.33 point increase in the percent of children living in poverty in the county. 

#Here we see the consequences of the squared term. To fit the strongly negative relationship
#from 0-50, the quadratic must just as steeply go upwards after the minimum at around 70k.
#So, this leads to the prediction that when income is 100k, additing additional wealth
# to a county will lead to more child poverty, which is absurd.


# (f)A possible confounding variable to this relationship is the unemployment rate
#which may affect both the median income of a county and the percent of children 
#living in poverty. 
#Use the cor() function investigate the relationships between 
#Median income, child poverty rates, and the unemployment rate
#based on this pattern of correlation, how is the coefficient on median income
#likely to change if you add it to the first regression model (the one without the 
#polynomial terms)

#Included and dependent
cor(acs$median.income, acs$percent.child.poverty, use="pairwise.complete")
#There is a negative correlation between median income and unemployment rate
#Included and omitted
cor(acs$median.income, acs$unemployment.rate, use="pairwise.complete")
#There is a negative relationship between median income and unemployment.

#As such, the coefficient is negative biased. It will become more positive
#once we include the unemployment rate

#(g) Run this second regression and determine the degree to which the coefficient on 
#median income changes. Interpret the other coefficients in the model, being sure
#to adjust your language to the fact that there are now multiple independent variables.

m4 <- lm(percent.child.poverty ~ median.income + unemployment.rate, data=acs)
summary(m4)

#The original coefficient on median income was -0.61. In this regression it is -.44.
#As predicted this coefficient moved in a positive direction after adding the unemployment rate

#The interpretations are now
#Holding the unemployment rate constant, a $1000 increase in the median income of a county
#is associated with a .44 percentage point decrease in the percent of children living in poverty.

##Holding the median income of a county constant, a 1 percentage point increase in the unemployment rate of a county
#is associated with a 1.36 percentage point increase in the percent of children living in poverty.

##When the median income and unemployment rate of a county are both 0, the average percent of 
#children living in poverty is expected to be 36.6. There is no such county.



#(h) Another possible confounding variable is the census region people are living in
#Living in the south could also be associated with lower average incomes and more 
#child poverty for example. 
#Create indicator variables for the 4 census regions (or change the variable into a factor variable)
#and then re-estimate the regression
#(again, without the polynomials) to take into account what region each county is in.
#How do you now interepret the coefficients in the regression?
acs$census.region <- as.factor(acs$census.region)
acs$census.region <- relevel(acs$census.region, "northeast")

m5 <- lm(percent.child.poverty ~ median.income + unemployment.rate + census.region, data=acs)
summary(m5)

#Holding all else equal, a $1000 increase in the median income of a county
#is associated with a .39 percentage point decrease in the percent of children living in poverty.

##Holding all else equal, a 1 percentage point increase in the unemployment rate of a county
#is associated with a 1.19 percentage point increase in the percent of children living in poverty.

##Holding constant the unemployment rate and median income of a county
#The percent of children living in poverty in the midwest is  1.82 percentage points less
#than in the northeast.

##Holding constant the unemployment rate and median income of a county
#The percent of children living in poverty in the south is  1.72 percentage points higher
#than in the northeast.

##Holding constant the unemployment rate and median income of a county
#The percent of children living in poverty in the west is  1.2 percentage pointsless
#than in the northeast.

#When the median income and unemployment rate of a county are both 0, the average percent of 
#children living in poverty is expected to be 35.1 in the south. There is no such county.






#(i) It's possible that the effect of median income is different based on whether a county
#is urban or not. 
#Create an indicator variable for whether a county is urban (population density greater
#than 1000). Interact this variable with median income in the regression with unemployment
#rate, and the census region indicators.
#Interpret the coefficients on median income, the urban indicator, and the interaction term.

acs$urban <- NA
acs$urban[acs$population.density>=1000] <- 1
acs$urban[acs$population.density<1000] <- 0


m6 <- lm(percent.child.poverty ~ median.income*urban +
                                 unemployment.rate +
                                 census.region, data=acs)
summary(m6)

#The coefficient on median.income is the effect of a county with a median income $1000
#higher for rural counties (i.e. when urban is equal to 0).
#In rural counties, when median income rises by $1000 the percent of children living in 
#poverty decreases by .44 percentage points.

#The coefficient on urban is the effect of being an urban vs. being a rural county
#when median income is equal to 0. This is a nonsense coefficient.

#The coefficient on the interaction term represents the change in the effect of median
#income when going from a rural to urban county. The effect of income in an urban county
#is .16 points more positive than it is in a rural county, and that difference in effects is statistically
#significant.

coef(m6)["median.income"] + coef(m6)["median.income:urban"]

#In urban counties, when median income rises by $1000 the percent of children living in 
#poverty decreases by .28 percentage points. This effect is smaller than it is in rural counties
#and the difference in the effect sizes between urban and rural counties is statistically significant.


#Note that you can interpret this interaction in the opposite manner
#The interaction term is also how the effect of being in an urban vs. rural
#county changes as incoem changes. For example, the effect of being an urban county
#when that county is poor (15k), medium (40k), and wealthy (75k)

coef(m6)["urban"] + coef(m6)["median.income:urban"]*15
coef(m6)["urban"] + coef(m6)["median.income:urban"]*40
coef(m6)["urban"] + coef(m6)["median.income:urban"]*75