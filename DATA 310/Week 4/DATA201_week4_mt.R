## Data 310
#Synchnous Session
#M. Trussler
#February 11, 2021

setwd("C:/Users/trussler-adm/Dropbox (PORES)/PORES/DATA Certificate/DATA 310 (201)/Week 4/11. Synchronous Session")

######################
#### The T distribution for samples

#We've become pretty good at understanding the sampling distribution of the sample
#mean. But as we left off last week we noticed a problem: we are dependent on knowing
#the population variance (sigma^2) to know what that distribution is likely to be

#This is something we never have in the *real* world, and as such we need a different
#method of constructing confidence intervals (and soon, conducting hypothesis tests)
#that take into account this lack of information.

#Let's consider we are in the real world and we run a sample on 15 people and 
#collect information on variable X. 

#The data collection process
set.seed(21051988)
x <- rnorm(15, mean = 15, sd=12)

#So we are left with a variable with the following information:

mean(x)
sd(x)
length(x)

#What do we have to do to put a 95% confidence interval around this observation?

#The key theoretical insight this week is that we can estimate the sampling distribution
#of the sample mean with only the information that is contained in a single sample.

#Critically, we learned in lectures the sampling distribution of the sample mean 
#is T-distributed with n-1 degrees of freedom.

#Like the *standard* normal, the t-distribution is a reference distribution that tells
#us how many standard errors above and below the mean contain a certain amount of probability

#Let's go to the whiteboard to think this through....



#Back in R

#Using the qt function to find the upper and lower bounds for a sample mean
#with these degrees of freedom:
qt(p=c(.025,.975), df=14)

#Regardless of the particular mean and standard error, any sample with 15 observations
#will have a CI that is the mean +/- 2.14 standard errors.

#Calculating the standard error of the sample mean:
sem <- sd(x)/sqrt(15)
sem

#Constructing the confidence interval:
mean(x) +qt(p=.975, df=14)*sem
mean(x) -qt(p=.975, df=14)*sem

#qt takes probabilities and turns them into standard errors
#pt takes standard errors and turns the into probabilities

qt(.975, df=14)
pt(2.14, df=14)



#Similarly, what if we wanted to know what the probability mass is that is 
#+/- two units around the mean

#To put into the units of the t distrubtion, need to convert into standard error 
#units
sem <- sd(x)/sqrt(15)
2/sem

#We want to know what the probability mass is +/- 0.72 standard errors in the
#the t distribution with 14 degrees of freedom

pt(2/sem, df=14) - pt(-2/sem, df=14)

#There is about 52% of the probability mass of the sampling distribution +/- 2
#away from the mean.



##Here's why this is helpful: Most of the things we now calculate will be t distributed
# That means that if we have the degrees of freedom and a standard error we can calculate
# a confidence interval for anything. 

#If I give you those two pieces of information, you can now do it. 

######################
####Difference in means test

#Consider a difference in means
set.seed(21051988)

#Data generating process
j <- sample(c(0,1), 100, replace= T)
y <- 2 + j*4 + rnorm(100,0,5)

j1 <- j[j==1]
y1 <- y[j==1]

j0 <- j[j==0]
y0 <- y[j==0]

#Here we have two different sample means

#y1
mean(y1)
sd(y1)
length(y1)


#y0
mean(y0)
sd(y0)
length(y0)


#And we want a 95% confidence interval on the difference of the means y1-y0
mean(y1) - mean(y0)

#Key is that if we know the degrees of freedom and standard error we can calculate
#this confidence interval same as we did above

#Degrees of Freedom Calculation
#(Ugly, will do once on your problem set, and then never again)

num <- ((sd(y1)^2)/54 + (sd(y0)^2)/46)^2
den <- ( ((((sd(y1)^2)/54)^2)/53)  +  ((((sd(y0)^2)/46)^2)/45) )
num/den

#Degrees of freedom is approximately 96

#standard error is

sedm <- sqrt(((sd(y1)^2)/54) + ((sd(y0)^2)/45) )

#Now that we have these two things we can:

#First determine the bounds for the t distribution 

qt(.975, df=96)

#1.98

#And then add this many standard errors around our estimate

(mean(y1) - mean(y0)) - sedm*1.98
(mean(y1) - mean(y0)) + sedm*1.98


#The 95% confidence interval from this difference in means runs from [2.40, 7.18]


#Note that, in the real world, you don't really have to do any of these calculations: 
#(though, you do on your homework this one time.)
t.test(y1, y0)

#And, as discussed in lecture, the same thing can be found via
#regression

m <- lm(y ~ j)
confint(m)



#One last thing on t-distribution is that it is increasingly 
#shaped like the standard normal as the sample size increases,

range <- seq(-3,3,.001)

plot(range,dnorm(range), type="l", col="darkblue", main="df=5")
points(range, dt(range, df=5), type="l", col="firebrick")
legend("topleft", c("Standard Normal", "T Distribution"),
       lty=c(1,1), col=c("darkblue", "firebrick"))


plot(range,dnorm(range), type="l", col="darkblue", main="df=30")
points(range, dt(range, df=30), type="l", col="firebrick")
legend("topleft", c("Standard Normal", "T Distribution"),
       lty=c(1,1), col=c("darkblue", "firebrick"))


plot(range,dnorm(range), type="l", col="darkblue", main="df=100")
points(range, dt(range, df=100), type="l", col="firebrick")
legend("topleft", c("Standard Normal", "T Distribution"),
       lty=c(1,1), col=c("darkblue", "firebrick"))


plot(range,dnorm(range), type="l", col="darkblue", main="df=1000")
points(range, dt(range, df=1000), type="l", col="firebrick")
legend("topleft", c("Standard Normal", "T Distribution"),
       lty=c(1,1), col=c("darkblue", "firebrick"))


#which means that the standard error units for a particular
#confidence interval will converge on familiar values as 
#Sample size increases

qt(.975, df=5)
qt(.975, df=10)
qt(.975, df=30)
qt(.975, df=50)
qt(.975, df=100)
qt(.975, df=500)
qt(.975, df=1000)
qt(.975, df=5000)
qt(.975, df=10000)

######################
#### Bivariate Regression basics

#As I discussed in the first class regression is the workhorse of
#data science. There is never a project I work on where
#I don't use regression in some way or another

#Consider two continuous variables x&y
#Data generating process
set.seed(21051988)
x <- rnorm(100,mean=50, sd=14)
y <- 3 + 6*x + rnorm(100, mean = 0, sd=30)
# 

# With two variables we can easily visualize using a scatterplot
plot(x,y)


#The question of regression is how to summarize what is happening
#between these two variables.

#As we saw in lecture the way that we summarize a relationship betwen
#two variables like this is through a regression line.

#To draw a line in a cartesian plane we need two pieces of information:
#the y-intercept and the slope

#All OLS (Ordinary Least Squares) regression
#Does is choose an intercept and slope that minimizes the
#(squared) distance between each point and the resultant line.

#We will see over the next weeks why this is the best possible method
#to draw a line.

#We can use the lm command to estimate this line
m <- lm(y ~ x)

summary(m)


#What is this output telling us?

#The line which minimizes the sum of the squared residuals
#crosses the y intercept when y equals 13.48, and has a slope of 
#5.75

#y = mx+ b
#y = 5.75x + 13.48

#y = a + Bx
#y = 13.48 + 5.75x

#To simply draw that line with the base R functionality
plot(x,y)
abline(m)

#And with ggplot
library(ggplot2)
ggplot()+
  geom_point(aes(x=x, y=y))+
  geom_smooth(aes(x=x, y=y), method = "lm")

#As an exercise to demonstrate the continued importance of the central
#limit theorem, here is the distribution of the coeficient on x
#from 1000 samples from the same population:

coef.estimate <- rep(NA, length(1000))

for(i in 1: 1000){
  x.l <- rnorm(100,mean=50, sd=14)
  y.l <- 3 + 6*x.l + rnorm(100, mean = 0, sd=30)
  
  m.l <- lm(y.l ~ x.l)
  coef.estimate[i] <-  coef(m.l)["x.l"]
}

plot(density(coef.estimate))
#Normally distributed sampling distribution
#What's the standard deviation of this sampling distribution??

sd(coef.estimate)

#What was our estimate of the standard error above?

summary(m)

#Not exact (simulation!) but pretty close.
#**The standard error is the standard deviation of the sampling distribution**


#An example with real data can help us to better understand how 
#to interpret regression output.

#I'm going to load in some data from the Americas Community Survey on
#counties
load("ACSCountyData.Rdata")

#Let's look at the relationship between the median household income
#and the percent of childen living in povery in the county

plot(acs$median.income, acs$percent.child.poverty, xlab="Median Income",
     ylab="Percent Childen Living in Poverty")

#This is, predictably, a negative relationship. As the median income
#of a county increases, the percent of childen living in poverty decreases
#Which makes sense!

#To put numbers to this relationship we use regression

m1 <- lm(percent.child.poverty ~ median.income, data=acs)
summary(m1)

#How do we interpret these estimates?
#The slope coefficient *always* represents the effect of 
# a one unit change of x on y. 
#By "one unit" we mean ONE UNIT IN OUR NUMBER SYSTEM.
#as in 1-2, 423-424, 4849-4850. Always.

#Here, because the x variable is in dollars, we can write the sentence

#When the median income increases by $1 the percent of childen living in
#poverty decreases by .0006 percent. 

#That's not particularly helpful. 

#What happens if we create a new variable that is median income in thousands
#of dollars?

acs$median.income.thousands <- acs$median.income/1000

plot(acs$median.income.thousands, acs$percent.child.poverty, xlab="Median Income (thousands)",
     ylab="Percent Childen Living in Poverty")

m2 <- lm(percent.child.poverty ~ median.income.thousands, data=acs)
summary(m2)

#We still interpret the regression in terms of a one unit change in x
#But now, one unit is a change in 1000 dollars. 

#So now we would write
#When median income increases by $1000 dollars in a county, 
#the percent of childen living in poverty decreaes by .61%.

#Note that this is decribing the same relationship, just with a rescaled x

#What about the intercept in this new regression?

summary(m2)

#The intercept is the average value of our outcome variable when 
#all other variables in the model are equal to 0. 
#We just have one variable in the model right now, 
#So we can state this in a pretty easy sentence

#When the median income of a county is 0, the average percent of 
#children living in poverty is 53.6%

#A good thing to ask yourself: does this make any sense??
#Mechanically to draw a line we need a y-intercept.
#That doesn't mean that it is a sensible value to know
#There are no counties with median income of 0....

#This value will get more absurd the more variables are added


######################
####Visualizing Regression
plot(acs$median.income.thousands, acs$percent.child.poverty,
     xlab="Median Income (thousands)",
     ylab="Percent Childen Living in Poverty")
abline(m2, col="firebrick", lwd=2)

ggplot(data = acs)+
  geom_point(aes(x=median.income.thousands, y=percent.child.poverty))+
  geom_smooth(aes(x=median.income.thousands, y=percent.child.poverty), method = "lm")

#You will have also at this point spotted the other problem with this regression
#The underlying relationship isn't actually all that well summarized by a
#straight line.

#This is something we will diagnose and fix in later classes. But this 
#shows well the limitations of bivariate regression
#(And the importance of visualzing your data!)

######################
####Prediction with regression

#Another way to interpret the output of a regression is to consider
#points on the "line" as predicted values for the outcome variable for 
#different levels of the x variable. 

#Consider again our regression of median income and child poverty

summary(m2)

#Given these values, we can determine what the predicted level of
#child poverty is for any income level.

#The equation for the regression line is

#child poverty = 53.584 - 0.61*median income

#So, if we want to know the predicted level of 
#child poverty when the median income is 75k


53.584 - 0.61*75

#7.8%

#Note however, there is nothing constraining this to give you
#realistic values 

#What is the predicted level of child poverty when
#median income is 300k a year?

53.584 - 0.61*300

#-129%, which doesn't exist.....

#As a last thing, a helpful function to run on a regression output
#is coef(), which just give you the vector of coefficients

coef(m2)

#You can then use the names to call the full number without rounding

coef(m2)["median.income.thousands"]
coef(m2)["(Intercept)"]