#1.This question will have you answer the following questions about the R code
#  "RobustStdErrors.R". This code runs a simulation designed to test the
#  performance of robust standard errors to correct for heteroscadasticity 
#  relative to the OLS standard errors that assume no heteroscadasticity or 
#  autocorrelation.

#.Lines 8-13 of the code define a number of parameters that are used within 
# simulation. Trace through the code and describe what role each of these 
# parameters plays in structuring the simulation.

#numsims is the number of simulations that the for loop is going to run.
#the job of the simulations is to essentially prove that robust standard errors
#are more accurate in increasing the standard errors to represent the standard
#errors that are supposed to be in the graph that has heteroscedasticity.
numsims <- 1000

#numobs sets the number of rows in the matrix that is created. 
#The first column of the matrix represents the x-values which are uniformly
#distributed (between 0 and 1)
#The second column of the matrix represents the error term which is the possible 
#difference in the calculated y-value and actual y-value (it is like the grey region
#in a geom_smooth() function). Represents the variance associated with x (variance of residual)
#The third column represents the y-output 
numobs <- 500

#the alpha and beta do not represent type 1 and 2 errors respectively in this
#situation. In fact, alpha represents the y-intercept while beta represents the 
#slope. We want to see if the slope (beta) falls under the confidence intervals.
#If beta does fall in the confidence intervals that means that robust standard
#errors are more accurate than OLS standard errors
alpha <- 1
beta <- 2

#We want to create a 95% confidence interval to see if there is a 95% chance that
#beta (the slope) will fall into the interval
lb <- .025
ub <- .975

#.Explain what part of the code creates heteroscadasticity in the unobserved 
# variables when running the regression on line 33.

#line 26 creates heteroscadasticity, specifically in the part "sd = data[,1]"
#because it is saying that as X increases the Standard Deviation increases which
#causes a greater residual variance which results in greater heteroscadasticity.
#line 26 is creating out error term which highlights the variance of the residual.
#adding this to our "y=mx+b" equation in line 29 would increase the distance/variance
#between the data points and regression line (aka residual)


#.Line 49 defines a variable called "withinCI". Highlight what values that this 
# variable can take on and under what circumstances it takes each of the
# potential values.

#The 'withinCI' variable can take on the confidence intervals with the OLS standard
#errors and would see if the 'beta' does not fall into the rejection region (meaning
#we failed to reject the null hypothesis). If 'withinCI' is true then that means that
#OLS standard errors are not accurate in increasing the standard errors of a regression
#that has heteroscedasticity.
withinCI <- ((lower_bound_ols < beta) & (upper_bound_ols > beta))

# .Explain what the values of "numwithinCIols" and "numwithinCIrobust" represent
#  on lines 52 and 67 of the code, respectively.

#numwithinCIols and numwithinCIrobust are both vectors that store in TRUE and FALSE
#values. 

#numwithinCIols stores in the TRUE values each time we fail to reject the null
#hypothesis of our OLS standard error 
#a FALSE value is stored each time we reject the null hypothesis of our OLS
#standard error. This means that beta is likley to be the slope. The values were
#false 95% or more of the time (our p-value is less than 0.05)

numwithinCIols <- numwithinCIols + withinCI

#numwithinCIols stores in the TRUE values each time we fail to reject the null
#hypothesis of our Robust standard error 
#a FALSE value is stored each time we reject the null hypothesis of our Robust
#standard error. This means that beta is likley to be the slope. The values were
#false 95% or more of the time (our p-value is less than 0.05)

numwithinCIrobust <- numwithinCIrobust + withinCI


#.Run the simulation. What do you conclude about the appropriateness of using
#OLS and robust standard errors when analyzing these data based on the values
#of 'numwithinCIols" and "numwithinCIrobust" that the simulation generates?

#the p-value for numwithinCIrobust is under 0.05 meaning that more than 95% of
#the time the values in the vector were false. We rejected the null hypothesis
#more than 95% of the time

#the p-value for numwithinCIols is over 0.05 meaning that less than 95% of
#the time the values in the vector were false. We rejected the null hypothesis
#less than 95% of the time

#This means that robust standard errors are more accurate in increasing the 
#standard errors to accurately represent the standard error of a regression that
#has heteroscadasticity

# .Edit the code in a way such that the unobservable variables are homoscadastic
# and rerun the simulation. Based on these results, what do you conclude about the
# best course of action when you are uncertain whether the unobserved variables
# are homoscadastic or heteroscadastic and you have a sizable amount of data?


