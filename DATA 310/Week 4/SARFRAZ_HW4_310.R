#Question 1
#(a) Consider a sample of 20 people for a random variable X which has a mean
#¯x = 4 and a sample standard deviation sx = 6. Estimate the symmetrical 
#95% confidence interval for this estimate using the t-distribution with the
#correct degrees of freedom.

#mean = 4
#sample size = 20
#standard deviation (S)=6
#sample variance (S^2)=36
#variance (standard error) = sqrt(36/20) or 6/sqrt(20)

sqrt(36/20) #1.341641
6/sqrt(20) #1.341641

pt(0.25, 19) 
pt(0.975, 19) 

4+qt(0.025, 19)*sqrt(36/20) #lb: 1.191914
4+qt(0.975, 19)*sqrt(36/20) #ub: 6.808086

#(b) Calculate the 95% confidence interval instead using a normal distribution
#centered on ¯x and a variance of s2x Is this different then what you got above?

qnorm(0.025,4, sqrt(36/20)) #lb: 1.370432
qnorm(0.975,4, sqrt(36/20)) #ub: 6.629568

#(c) What if we have two independent samples of 10 people each, where ¯x = 5 and sx = 2;
# y¯ = 6 and sy = 2. What is the 95% confidence interval around the difference in these two
# means? (You can calculate the degrees of freedom for the t distribution using the provided
#         formula, but note that it will come out to 18.)

#mean: 1(6-5)
#degree of freedom: 18(20-2)
#standard error: sqrt(4/5)[sqrt((4/10)+(4/10))]

1+qt(0.025,18)*sqrt(4/5) #lb:-0.8791218
1+qt(0.975,18)*sqrt(4/5) #ub:2.879122

# Question 2
library(tidyverse)
#(a) This question is going to have you further investigate some polling data.
#Upload "AZPollFake.Rdata" into R. Note: this file began as real polling data
#from the NYT, but I've created a fake variable "clinton.thermometer" so that
#we have a continous measure to work with. So don't take too seriously the
#conclusions that we draw from this question!

load('C:/Users/hussainsarfraz/Desktop/DATA 310/AZPollFake.Rdata')

#   (b) "clinton.thermometer" is a (again, fake) measure of how each respondent feels about Secretary Clinton, with 0 indicating that they feel very "cool" towards her, and 100 indicating
# they feel very "warm" towards her. Pretending for a moment that this is a simple random
# sample, calculate using our known equations: ; ; .
#Question #2 B

#Part i - the 95% confidence interval for "clinton.thermometer"

#n (sample size)
nrow(az)# 447
length(az$clinton.thermometer)# 447

#sample mean
mean(az$clinton.thermometer)# 43.28229

#standard deviation of sample
sd(az$clinton.thermometer)# 36.07468

#sample variance
36.07468/sqrt(447)# 1.706274
sd(az$clinton.thermometer)/sqrt(447)# 1.706274

#confidence interval
43.28229+qt(0.025,446)*(36.07468/sqrt(447))# lb=39.92895
mean(az$clinton.thermometer)+qt(0.025,446)*sd(az$clinton.thermometer)/sqrt(447)

43.28229+qt(0.975,446)*(36.07468/sqrt(447))# ub=46.63563
mean(az$clinton.thermometer)+qt(0.975,446)*sd(az$clinton.thermometer)/sqrt(447)# ub=46.63562

#Part ii (ii) the 95% confidence interval for "clinton.thermometer" among those
# voting for clinton ("clinton"==1)

#n (sample size)
az %>%
  filter(clinton==1) %>%
  nrow()
length(az$clinton.thermometer[az$clinton==1])# 259

#sample mean
mean(az$clinton.thermometer[az$clinton==1])# 61.93196

#standard deviation of sample
sd(az$clinton.thermometer[az$clinton==1])# 31.71151

#sample variance
31.71151/sqrt(259)# 1.970457
sd(az$clinton.thermometer[az$clinton==1])/sqrt(259)# 1.970458

#confidence interval
61.93196+qt(0.025,258)*(31.71151/sqrt(259))# lb=58.05173
mean(az$clinton.thermometer[az$clinton==1])+qt(0.025,258)*sd(az$clinton.thermometer[az$clinton==1])/sqrt(259)# lb=58.05173

61.93196+qt(0.975,258)*(31.71151/sqrt(259))# ub=65.81219
mean(az$clinton.thermometer[az$clinton==1])+qt(0.975,258)*sd(az$clinton.thermometer[az$clinton==1])/sqrt(259)# ub=65.81219


#Part iii - the 95% confidence interval for "clinton.thermometer"
# among those not voting for clinton ("clinton"==0)
#n (sample size)
az %>%
  filter(clinton==0) %>%
  nrow()
length(az$clinton.thermometer[az$clinton==0])# 188

#sample mean
mean(az$clinton.thermometer[az$clinton==0])# 17.58938

#standard deviation of sample
sd(az$clinton.thermometer[az$clinton==0])# 23.89595

#sample variance
23.89595/sqrt(188)# 1.742791
sd(az$clinton.thermometer[az$clinton==0])/sqrt(188)# 1.742791

#confidence interval
17.58938+qt(0.025,187)*(23.89595/sqrt(188))# lb=14.15132
mean(az$clinton.thermometer[az$clinton==0])+qt(0.025,187)*sd(az$clinton.thermometer[az$clinton==0])/sqrt(188)# lb=14.15132

17.58938+qt(0.975,187)*(23.89595/sqrt(188))# ub=21.02744
mean(az$clinton.thermometer[az$clinton==0])+qt(0.975,187)*sd(az$clinton.thermometer[az$clinton==0])/sqrt(188)# ub=21.02744

# (c) Next, using the survey package, calculate the same three confidence intervals, but this
# time applying the provided weights. How does each estimate change? What does this tell
# us about the probability of those voting for each candidate ending up in our sample?

library(survey)

az_weight <- svydesign(ids = ~1,
                       data = az, weights = az$final_weight)

#part i
az_meani <- svymean(~clinton.thermometer, az_weight)
az_meani
confint(az_meani)

#part ii

#svymean(~clinton.thermometer, az_weight[az$clinton==1])
az_meanii <- svymean(~clinton.thermometer, subset(az_weight, clinton==1))
az_meanii
confint(az_meanii)

#part iii

#svymean(~clinton.thermometer, az_weight[az$clinton==0])
az_meaniii <- svymean(~clinton.thermometer, subset(az_weight, clinton==0))
az_meaniii
confint(az_meaniii)

#   (d) Now perform a ttest using the survey package to find the confidence interval for the
# difference in means of "clinton.thermometer" for those who voted for Clinton and those who
# voted for Trump.

svyttest(clinton.thermometer~clinton, az_weight)

# (e) Confirm for yourself that using the lm() command gives you the same answer for a
# difference as means as what you got in (d). What does the estimate for "(Intercept)" mean
# in this output? What does the estimate for "clinton" mean in this output? (Do not forget
#            to include weights in the regression.)

lm(clinton.thermometer~clinton, data = az, weight = final_weight)




# (f) Create a scatterplot where clinton.thermometer is on the x-axis and final weight is on the
# y-axis. Just looking at the data, are you able to form any conclusions about the relationship
# between these two variables?

az %>%
  ggplot(aes(x = clinton.thermometer, y = final_weight)) +
  geom_point() + 
  geom_smooth(method='lm')


#   (g) Add a regression line to your plot (don't worry about weighting), does this change your
# interpretation of the relationship?


#   (h) Consider the output of the regression where clinton.thermometer is the independent
# variable and final weight is the dependent variable.(Again, don't worry about weighting.)
# What is the interpretation of the Estimate for "clinton.thermometer" from this output?
#   What is the the interpretation of "(Intercept)"? What sort of conclusion can you make
# about the sampling process for this survey based on this output?