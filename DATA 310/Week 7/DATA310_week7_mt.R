#--------------------------------------------
# Week 7 Synchronous Session Lesson Plan
#--------------------------------------------
#M Trussler
#March 4, 2021
rm(list=ls())
setwd("C:/Users/trussler-adm/Dropbox (PORES)/PORES/DATA Certificate/DATA 310 (201)/Week 7/11. Synchronous session")

#For tonight's synchronous session we will focus on three things:
#(1) Robust standard errors
#(2) Clustered standard errors
#(3) Visualizing Regression in Tables 


#####
#Robust standard error

#The normal standard errors for a regression assume that the errors around the
#regression line do not depend on values of X.
#Put in mathy terms E{epsilon_i | x} = sigma

#What does this look like in practice:
#To set up some extreme examples:

#Homoskedastic Errors
x <- runif(100, min=0, max=100)
y <- 2 + 3*x + rnorm(100, mean=0, sd=15)

plot(x,y)
abline(lm(y~x), col="firebrick" ,lwd=2)

#Heteroskedastic Errors
x <- runif(100, min=0, max=100)
y <- 2 + 3*x + rnorm(100, mean=0, sd=x)

plot(x,y)
abline(lm(y~x), col="firebrick" ,lwd=2)

#We often think about heteroskedasticity as this "fan" shape, but what we are 
#looking for is if the errors being made in a regression are likely to be 
#larger or smaller for certain values of x

#Let's look at a practical example using the data on California schools 
library(AER)
data("CASchools")
head(CASchools)

dat <- CASchools

#Let's again look this time at the relationship between percent of students with
#english as a second language and math test scores
dat$esl <- dat$english

#lm(math ~ esl)

#Before we plot this to assess hetero/homoskedasticity let's think through what
#is likely to be the case
#Do we expect more *variation* in math scores when esl is high/low?
#Probably, yes. My expectation is that schools with a low number of esl 
#kids will be in all sorts of neighborhoods with all sorts of different levels
#of funding, family income etc. 
#So I expect a lot of variation in math scores in these types of schools.
#Schools with a high number of esl kids will all be in similar types of neighborhoods
#so I expect a bit less variation in str in these schools. 

#Note, we're not talking about the relationship between esl and math scores, but how the 
#random variance in math changes across levels of esl. 
#Further, while we have tools to diagnose heteroskedasticity, part of what you do
#will be using your theoretical knowledge of cases to determine whether it is likely
#to be a problem. This is what I did here with my deep knowledge of the California
#school system (kidding! I cheated and looked at the data first...)

#Let's take a look at what we see.

plot( dat$esl, dat$math)
abline(lm(dat$math ~ dat$esl), col="firebrick", lwd=2)

#This seems to reflect our prediction, in general there seems to be a lot more 
#variation in math scores when esl is lower, and less when esl is higher. 

#There is a specific test for heteroskedasticity beyond what we can see with our eyes:
library(lmtest)
m <- lm(math ~ esl, data=dat)
bptest(m)
#This test will be statistically significant if heteroskedasticity is present
#Here we reject the null hypothesis of homoskedasticity. 

#How do we go about fixing this?

#Note that one way to fix heteroskedasticity is to better model the reasons for higher variance
#in some cases. 

#We think that there is more unmodeled variance at the low end of esl. In other words
#schools without a lot of english as a second language children come from all different
#types of places. If we add variables that explain math scores among that group better
#heteroskedasticity will be reduced. 

#So, for example, if we add to our regression other variables
#which help us determine what sort of schools these are...

m <- lm(math ~ esl + lunch + calworks + income, data=dat)
summary(m)
bptest(m)

#We no longer have a heteroskedastic regression. 

#The other way to deal with heteroskedasticity is to employ
#Hubert-White (or simply "robust") standard errors. 

#For this we need the sandwich package
library(sandwich)
library(lmtest)
m <- lm(math ~ esl, data=dat)
summary(m)

robust.vcov <- vcovHC(m, type="HC1")

coeftest(m, vcov=robust.vcov)
summary(m)

#In this case, accurately using robust standard errors increases
#the efficiency of our model and we get lower standard errors. 
#That's good!

#What happens if we mis-apply robust standard errors to something which
#doesn't have heteroskedasticity?
dat$str <- dat$students/dat$teachers

m <- lm(str ~ esl, data=dat)
bptest(m)

robust.vcov <- vcovHC(m, type="HC1")

coeftest(m, vcov=robust.vcov)
summary(m)

#Honestly.... not much. 
#As long as your sample is sufficiently large, robust standard errors
#will only lead to possibly slightly higher standard errors. But, could 
#lead to reduced standard errors if heteroskedasticity is present. 

#In sum, it's rarely a bad choice to use robust standard errors. While you
#should make some mentions of possible sources of heteroskedasticity, it's good
#to look at using robust SEs as a default. 
#Indeed, HW7 shows this via simulation. 

####################################
##Clustered standard errors
#The other potential problem we have with standard errors is auto-correlation
#With autocorrelation we are asking: does knowing the error on one case
#allow us to understand the error on a second case?
#When we don't account for autocorrelation our standard errors will be 
#artificially low, increasing the likelihood of Type I errors.

#The big violations to this come from when you resample the same units over time
#and when you sample multiple units from the same cluster, where the clusters units inside
#a cluster have something in common. 

#For the time example: Consider you want to know the relationship between weather
#and bike riding and you sampled me every day. I'm always going to be more likely to ride
# a bike given the weather compared to what would be expected. Knowing my error on day 1 
#tells you something about my error on each subsequent days. My answers are correlated. 
#Time series regression is very complex, and requires more time then what we have for in 
#this class...


#For the clustering, let's consider a school example: how student to teacher ratio
#maps onto math scores

m <- lm(math ~ str, data=dat)
summary(m)
plot(dat$str, dat$math)
abline(lm(math ~ str, data=dat), col="firebrick", lwd=2)

#All looks well here, but we know that these schools come from different counties.
#It is plausible that school funding is dependent on county, and therefore the errors here
#might be related. 
#All schools in a county might, for example, have systematically better math scores regardless of STR. 
#Knowing information about one school in a county help you to understand the information
#of another school in a county. 

#Let's come up with our own way of determining, let's look at the average residual size
#for the counties just to see. 

#Determine what the residuals of each school are
dat$residuals <- residuals(m)

#Loop across counties getting average values of x, y, and residuals

counties <- unique(dat$county)

avg.str <- rep(NA, length(counties))
avg.math <- rep(NA, length(counties))
avg.residual <- rep(NA, length(counties))

for(i in 1:length(counties)){
  avg.str[i] <- mean(dat$str[dat$county==counties[i]], na.rm=T)
  avg.math[i] <- mean(dat$math[dat$county==counties[i]], na.rm=T)
  avg.residual[i] <- mean(dat$residuals[dat$county==counties[i]], na.rm=T)
  
  }
cbind.data.frame(counties,avg.str,avg.math,avg.residual)[order(avg.residual),]
#Yes, the average residuals between counties vary wildly. 
#Schools in Alameda county (east bay), have systematically high test scores have schools that are
#all way above the regression line. Similar with Marin county, santa cruz, santa barbara....
#All of these places have unusually high test scores for their level of STR.
#(All pass the "face validity" test of being rich places, too.)
#Sacremento has higher STR and lower test scores and are mainly below the regression line.
#Similar with Fresno...

#Because county is structuring outcomes here, there is autocorrelation based on county.
#If there wasn't we would see average residuals around 0 for all counties.


#Consider if we had some assignment that wasn't related to outcomes
#Number of clown colleges nearby: 
dat$clown.colleges <- sample(seq(1,15,1), nrow(dat), replace=T)

clown.colleges <- unique(dat$clown.colleges)

avg.str <- rep(NA, length(clown.colleges))
avg.math <- rep(NA, length(clown.colleges))
avg.residual <- rep(NA, length(clown.colleges))

for(i in 1:length(clown.colleges)){
  avg.str[i] <- mean(dat$str[dat$clown.colleges==clown.colleges[i]], na.rm=T)
  avg.math[i] <- mean(dat$math[dat$clown.colleges==clown.colleges[i]], na.rm=T)
  avg.residual[i] <- mean(dat$residuals[dat$clown.colleges==clown.colleges[i]], na.rm=T)
  
}
cbind.data.frame(clown.colleges,avg.str,avg.math,avg.residual)[order(avg.residual),]

#Some random variation, but generally much closer to zero. 

#How to fix autocorrelation? 

#A few ways, but the way you should focus on is clustered standard errors: 

m <- lm(math ~ str, data=dat)

cluster.vcov <- vcovCL(m, cluster=dat$county)

coeftest(m, vcov=cluster.vcov)
summary(m)

#Here this gives us higher standard errors, which is correct. Our standard errors without accounting
#for clustering were artificially low. We ran the risk of a type I error (false positive). 
#(Note, clustered standard errors are also heteroskedastic robust!)


#A note on random effects.
#Somewhat buried in Professor Meredith's content on Random Effects was that a pre-condition
#to random effects leading to unbiased estimates is that the covariates in your model
#have to be unrelated to the systemic nature of the clusters.
#Here, this would mean that the student to teacher ratio in classrooms would 
#have to be unrelated to whatever this unmeasured thing is that makes counties
#different from one another. If counties determine the nature of schools, this seems
#highly unlikely. In the real world, this condition is rarely going to be met
#outside of situations where we are randomly assigning treatment. 
#I personally think random effects are overused because people don't understand
#how restrictive this assumption is. 

#Fixed effects models (which are covered in the week 8 lectures which you can watch
#at your convenience) are almost always a bit clearer. 

#Estimating the effect of str on math scores within counties using fixed effects:

library(lfe)

m <- felm(math ~ str | county | 0 | county, data=dat)
getfe(m)
summary(m)


#Visualization
library(stargazer)

#I cannot express to you how much easier my life became when they invented stargazer. 

#Let's consider putting several regression models in a table.
#Want to model math scores on esl, with and without covariates, 
#And then with covariates with robust, and cluster-robust standard errors.

m1 <- lm(math ~ esl, data=dat)
m2 <- lm(math ~ esl + income + lunch + calworks, data=dat)

robust.vcov <- vcovHC(m2, type="HC1")
cluster.vcov <- vcovHC(m2, cluster=dat$county)

#Need to extract the standard errors for the, normal robust and cluster-robust
#vcov matricies

#Beyond what you need to know, but the standard errors are the square route of 
#the diagonal of a vcov matrix
robust.vcov
normal.se1 <- sqrt(diag(vcov(m1)))
normal.se2 <- sqrt(diag(vcov(m2)))
robust.se <- sqrt(diag(robust.vcov))
cluster.se <- sqrt(diag(cluster.vcov))

#We want 4 models, bivariate, multivariate, multivariate w robust, multivariate with cluster-robust
#If we just put the 4 columns in we get the standard SEs by default:
stargazer(m1, m2, m2 , m2 ,type="text")

#Need to tell stargazer what SEs to use
stargazer(m1, m2, m2 , m2 ,type="text",
          se = list(normal.se1, normal.se2,robust.se,cluster.se))

#Good!

#Now let's add better variable names
stargazer(m1, m2, m2 , m2 ,type="text",
          se = list(normal.se1, normal.se2,robust.se,cluster.se),
          covariate.labels = c("Percent ESL", "Percent Subsidized Lunches",
                               "Percent CalWorks","Median Household Income (thousands)"),
          dep.var.labels = "Math Test Scores")

#And give some idea of how the models differ
stargazer(m1, m2, m2 , m2 ,type="text",
          se = list(normal.se1, normal.se2,robust.se,cluster.se),
          covariate.labels = c("Percent ESL", "Percent Subsidized Lunches",
                               "Percent CalWorks","Median Household Income (thousands)"),
          dep.var.labels = "Math Test Scores",
          column.labels = c("Bivariate","With Controls", "HC1 Robust SEs", "County Clustered SEs"),
          model.numbers = F)


#To export to latex/markdown (no type)

stargazer(m1, m2, m2 , m2 ,
          se = list(normal.se1, normal.se2,robust.se,cluster.se),
          covariate.labels = c("Percent ESL", "Percent Subsidized Lunches",
                               "Percent CalWorks","Median Household Income (thousands)"),
          dep.var.labels = "Math Test Scores",
          column.labels = c("Bivariate","With Controls", "HC1 Robust SEs", "County Clustered SEs"),
          model.numbers = F)

#To export to word: create html and then copy and paste from web browser

stargazer(m1, m2, m2 , m2 , type="html",
          se = list(normal.se1, normal.se2,robust.se,cluster.se),
          covariate.labels = c("Percent ESL", "Percent Subsidized Lunches",
                               "Percent CalWorks","Median Household Income (thousands)"),
          dep.var.labels = "Math Test Scores",
          column.labels = c("Bivariate","With Controls", "HC1 Robust SEs", "County Clustered SEs"),
          model.numbers = F,
          out="MyTable.html")

#stargazer cheatsheet



















