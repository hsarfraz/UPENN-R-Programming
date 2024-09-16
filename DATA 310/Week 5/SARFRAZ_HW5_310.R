# Problem 1
#(a) Consider a sample of 20 people for a random variable X which is a feeling
#    thermometer for President Biden. The variable can take on values 0-100,
#    where 0 is "cold" feelings and 100 is "hot". In the particular sample that
#    we received the mean of this variable is ¯x = 53 and the sample standard
#    deviation sx = 7.

#    Perform a hypothesis test for the null hypothesis that Americans on 
#    average are ambivalent about Biden, feeling neither warm nor cool on 
#    average (i.e. the population mean, µ, equals 50). In this test you should
#    perform each of the 5 steps laid out in lecture, and make use of the
#    qt()/pt() functions.

#n=20               <-(sample size)
#x=53               <-(sample mean)
#s=7                <-(sample standard deviation)
#s^2=49             <-(sample variance)
#var(x)=7/sqrt(20)  <-(standard error of mean)

#step 1 (specifying the null hypothesis): Americans on average are ambivalent
#                                         about President Biden
# mu = 50

#step 2 (specifying the alternative hypothesis): Americans on average are not
#                                                ambivalent about President
#                                                Biden
# mu != 50

#step 3 (selecting the rejection region): The null hypothesis would be rejected
#                                         if the sample mean falls in the 
#                                         rejection region
# mu<46.7239 or 53.2761<mu
50 + (7/sqrt(20)) * qt(0.975,19) #ub (upper bound)   <-53.2761
50 + (7/sqrt(20)) * qt(0.025,19) #lb (lower bound)   <-46.7239
# 50 - (7/sqrt(20)) * qt(0.975,19) #lb (lower bound) <-46.7239

#step 4 (getting the test statistic): The sample mean would be the test
#                                     statistic
# x=53

#step 5 (choose to accept the null hypothesis or reject it)

#we have failed to reject the null hypothesis (we have accepted it) because the
#sample mean does not fall in the rejection region

#(b) Next, load in the 'ACSCountyData.Rdata' dataset. Run a regression where
#    'percent.college' is the independent variable and 'median.income' is the
#    dependent variable.

load('C:/Users/hussainsarfraz/Desktop/DATA 310/ACSCountyData.Rdata')

#lm(dependant.variable~independant.variable, data = )
lm(median.income~percent.college, data = acs)

# (c) Looking at the output of the regression, first interpret what the
#estimated coefficient for percent.college means.


# (d) What null hypothesis is being tested automatically for percent.college?
#   What is the alternative hypothesis? What is the result of that hypothesis
#   test?

### finding the values through summary() function
summary(lm(median.income~percent.college, data = acs))

#we can not calculate the things manually and need to use the regression
#function since it takes into consideration the dependant and independant 
#variable when performing the calculations

#3217 =degrees of freedom
#20.51 =standard error of mean
#49.50 =sample mean

#step 3: selecting the rejection region
0 + 20.51 * qt(0.975,3217) #ub (upper bound)   40.21399
0 + 20.51 * qt(0.025,3217) #lb (lower bound)   -40.21399
# 0 - 20.51 * qt(0.975,3217) #lb (lower bound) -40.21399



### manually finding the values in the 'percent.college' column
nrow(acs) #n <- sample size
mean(acs$percent.college) #x <- mean size
sd(acs$percent.college) #s <- sample standard deviation
sd(acs$percent.college)^2 #s^2 <- sample standard variance

sd(acs$percent.college)/sqrt(nrow(acs)) #var(x) <- standard error of mean

#used to check degrees of freedom
summary(lm(median.income~percent.college, data = acs))


#(e) Using the output of the model (i.e. the coefficient, the standard error,
#    and the degrees of freedom) and the qt()/pt() functions, confirm the 
#    automatic hypothesis test is correct by completing the same 5-step 
#    hypothesis testing sequence you completed in (a).



# Problem 2
#(a)Create a new variable 'x' using the command rnorm(100, mean=0,sd=7). 
#   This creates a new sample that has an n of 100. Note that the expected
#   value of the population we are drawing from is µ = 0 and a population
#   variance ??2 = 49.

x <- rnorm(100, mean=0,sd=7)

#(b) Use the t.test function to perform a t.test on this new variable for the
#    null hypothesis that µ = 0. Report the p-value for this hypothesis test.
#    (Note, the defualt for the t.test function is to test whether µ = 0.)

t.test(x,mu=0)

#we fail to reject the null hypothesis

#(c) Next, run a loop that completes this same process 1000 times: sampling
#    from rnorm(100, mean=0, sd=1), performing a t.test, and comptuing the
#    p.value for the null hypothesis that µ = 0. Each time through the loop 
#    save the p-value that was calcualted. (To do this, I first 1 save the 
#    results of the t.test to an object, and then access the p value that's 
#    in that object: t < ??? t.test(x); t$p.value).

ttest <- rep (NULL,length(1000))

for(i in 1:1000) {
  samp <- rnorm(100,mean = 0, sd =1)
  ttest[i] <- t.test(samp)$p.value
}

#(d) How many of the 1000 p-values you calculated were "statistically
#    significant" (i.e. less than .05)? What does this tell you about ??, the
#    probability of getting a false positive?

#number of p-values that were statistically significant
table(ttest < .05)[2:2]

table(ttest < .05)[2:2]/sum(table(ttest < .05)[1:2])

#sum(as.data.frame(ttest) < .05)/1000 #another way to find percentage of statistically significant p-values



#(e) Complete the same process as above, but this time sample 1000 times from
#    rnorm(100, mean=2, sd=7). That is, sample 100 observations 1000 times
#    from the normal distribution with µ = 2 and ?? 2 = 49. You should again
#    produce 1000 p-values which test the null hypothesis that µ = 0.

second_ttest <- rep (NULL,length(1000))

for(i in 1:1000) {
  second_samp <- rnorm(100, mean=2, sd=7)
  second_ttest[i] <- t.test(second_samp)$p.value
}

#(f) How many of the 1000 p-values you calculated were NOT statistically 
#    significant (i.e. are greater than .05)? What does this tell you about ??,
#    the probability of getting a false negative.

#number of p-values that were NOT statistically significant
table(second_ttest > .05)[2:2]

table(second_ttest > .05)[2:2]/sum(table(second_ttest > .05)[1:2])

#sum(as.data.frame(second_ttest) > .05)/1000 #another way to find percentage of NOT statistically significant p-values

#(g) Run the loop one more time, but modify the sampling command 
#    (rnorm(100,mean = 2,sd=7)) in one way that results in a lower ??. Describe 
#    the change you made and why it leads to a lower false negative rate.


third_ttest <- rep (NULL,length(1000))

for(i in 1:1000) {
  third_samp <- rnorm(350,mean = 2,sd=7)
  third_ttest[i] <- t.test(third_samp)$p.value
}

#number of p-values that were NOT statistically significant
table(third_ttest > .05)[2:2]


table(third_ttest > .05)[2:2]/sum(table(third_ttest > .05)[1:2])

#sum(as.data.frame(third_ttest) > .05)/1000 #another way to find percentage of NOT statistically significant p-values

# In large samples, a substantively insignificantly deviation
# from a null hypothesis can be statistically significant
# because the sampling variance is small

# In small samples, a substantively significant deviation from
# a null hypothesis can be statistically insignificant because
# the sampling variance is large

