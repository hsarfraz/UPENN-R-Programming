# R-Basics

This repository contains my R Programming assignmnets from my university classes. Along with R, I have used R Markdown (starting from DATA 101) and R Shiny (starting from DATA 410) to publish my homework. I have given a overview of each course folder and the contents it contains:

* DATA 101: An introduction to data analytics in R on how to conduct exploratory data analysis. How to run basic R commands on a dataset.
* DATA 210: Learning intermediate data analytic concepts in R
* DATA 310: Exploring statisitcal techniques and concepts that need to be implemented in Data Analytics
* DATA 410: Using R Shiny to create websites that show various data visualisations (charts, spatial, etc.)

# DATA 101: Introduction to Data Analytics

# DATA 210: Intermediate Data Analytics

# DATA 310: Introduction to Statistical Methods

## Week 1: Measurement and Description

The process of statistical data analysis
1. Collecting Data for a Variable (weeks 1 & 3)
2. Describe the values of the veriable for the observed cases (week 1)
3. Make predictions about the likely values of the variable, for the un-observed cases (weeks 2-4)
4. Make predictions about the likely values of a variable, for the un-observed cases, given the variables we have observed/gathered for those cases (weeks 4-7)
5. Understand how the value of a variable is likely to change because another variable, or variables, changed (week 8)

Vocabulary for Statistics 
* Construct of Interest: A theoretical quantity or theme which describes a characteristic/outcome and is non-measurable. An example is self-esteem
* Unit: The object on which a construct of interest is defined
* Population: The collection of units over which a construct of interest is defined
* Sample: The subset of units in the population for which we observe data
* Variable: A measure of a specific construct of interest for all sampled units
    * Dependent Variable: A variable with an outcome that the data scientist is trying to understand
    * Independent Variable: A variable with an outcome that the data scientist takes as a given that relates to the dependent variable
* Data: A collection of variables for sampled units
* Measurement: The process of translating a construct of interest into a variable

Stages of measurement
1. Specifying the information to be collected (ex. writing a survey)
2. Sampling units, from the set of cases, to collect this information from (ex. assigning the units to take the survey)
3. Recording the information available about the sampled units (ex. recording the survey responses provided by the sampled units)

Data Relibility and Validity
* Reliability: If we repeatedly measure the same thing, will we get the same (or similar) answer each time?
* Validity: Does the measure accurately capture the construct of interest?
   * A reliable and valid measurement is needed for a variable to represent the construct of interest
   * Reliability is a harder thing to address, especially when people might be changing their mind
   * Assessing validity is usually more challenging than reliability because it's often more of a theoretical than a empirical/observable question
 
Reasons why a measure might not be reliable
1. Confusing of challenging survey question
2. Survey satisficing (when respondents complete a survey fast and select what they think is the best without giving much thought)
3. Non-attitudes (when you ask repondants to fill a survey of something that they are not interested in)

Forms of Validity
* Face Validity: Does the measure appear to be measuring what we want it to measure?
* Content Validity: Does a measure include what is necessary from a theoretical standpoint?
* Construct Validity: Does the measure relate to what we would expect it to relate to from a theoretical perspective?

Measurement Error
* The phrase we use to refer the difference between a construct of interest and the variable we're using to measure the construct of interest 

Reasons why measurement error is concerning:
1. Can cause us to systematically misunderstand a construct of interest when the measurement error is systemically in one direction
3. Can make it more difficult to document a statistical relationship, even when the measurement error is arbitrary

Descriptive Statistics: The process of describing the values of observed variables in a sample of data

The two common things that we want to know about a sample
1. What is the central tendency? (measured through mean, median, mode)
2. How much variability is there from this central tendency? (looking at how the data is spread out)

Central Tendency (measured through mean, median, mode)
* The central tendency of a variable is the information about the typical value, of the a variabe, within a sample

Sample mean and weighted sample mean
* The most instinctive measure of central tendency is the **sample mean** (formula below). y is the observation for each unit, in the sample, while n is the total number of units.
  * The sample mean is a special case of a weighted sample mean (defined below) in which all units have the same weight of one. This means that all the units are equally influential because $w_1 = w_2 = ... = w_n = 1$

$$ \overline{y} = { y_1 + y_2 + ... + y_n \over n} = {1 \over n}{\sum_{i=1}^n y_i}$$

* Sometimes it is useful to think about the sample mean as a special case of a **weighted sample mean**:
   * To illustrate the effect of the weight lets suppose that $w_i=2w_j$ then the value of unit $i$ is two times more influential in determining the weighted sample mean than the value of unit $j$

$$ \overline{y} = { w_1y_1 + w_2y_2 + ... + w_ny_n \over w_1+w_2+...+w_n} = {\sum_{i=1}^n w_iy_i \over \sum_{i=1}^n w_i}$$

Nice properties of the weighted sample mean
* Intuitive: Averages are used all the time so naturally means are effective in understanding data. (ex. basket ball fans talk about the batting averages or calculating the average number of people who visit a store)
* Uses information from all observations $i$ such that $w_i > 0$
   * This means that the value of $y_i$ affects the value of the sample mean if $w_i > 0$ (the weights need to be greater than zero to see a change in the sample mean)
* Sample means have a lot of nice statistical properties that will be discussed throughout the course

Potential issues with the weighted sample mean
* Often equals a value that the variable in question could never take on
   * For example, the average roll of a dice could be 6.94 in a sample or the average number of people entering a store could be 30.4
* Issues with bottom- or top-coded data
   * Sometimes it might be unclear what values need to be assigned during the mean calculation. For example, some surveys might report that everyone with an income of more than $200 k makes $200 k+ or some people's age might be registered as 30+
   * Having bottom- or top- coded data might make it hard for a analyst to gain further insights on a particular group of units/respondents. Looking back at the previous example, it would be hard to see how much money the upper class makes or identify a relationship with the dependent variable and individuals of a specific age
* Sensitive to extreme values/outliers
   * For example the average income in a sample will change drastically if Oprah's income is included in the sample
 
A Ordered Sample & Sample Median
* A ordered sample is a sample such that if $i<j$ then $y_i \le y_j$
* In a ordered sample the subscripts now have more meaning (they take on a specific order)
* $P_x = Y_i$ is the xth percentile of a sample ${1 \over n} = {x \over 100}$
   * $i$ refers to the i-th observation of the unit, in the sample.
   * $n$ refers to the total number of units in the sample
   * $x$ refers to the unit's percentage in the sample
   * $x%$ of the values in the sample are $\le P_x$
   * $100 - x$ percent of the values are $\ge P_x$
* The 50th percentile or the **sample median**, is the observation (or observations) that falls in the middle of an ordered sample
   * Since we know the percentile $x$, and we also know the sample size $n$ we can solve for the value, of the unit $i$, and see what value is the median. The formula that we use to solve for $i$ is the same one shown above, but now the $n$ is moved to the other side of the equation (ex. $i = {x * n\over 100}$)
 
  Nice properties of the sample median
  * Pretty Intuitive
     * Half of the values are higher and half of the values are lower
  * Almost always takes on a value that the variable in question could take on
  * Often still observed in situations with bottom- or top- coded data (ex. where some values are registered as 200 k + or 60+)
  * Not affected by extreme values or miscoded values
 
  Potential issues with the sample median:
  * Doesn't use all available information
     * The same sample median occurs when the sample is -1, 5, 9 as 4, 5, 9
  * Can be uninformative in some situations (ex. binary data)
  * Computational and statistical issues that are beyond the scope of this class
 
  Outliers: Refers to an observation in a sample for which a variable takes on a value that is substantially different than normal

  Outliers matter for 2 reasons:
  1. Could be evidence for data error
  2. Inclusion/exclusion has important consequences of sample mean (deciding whether to include or remove the outliers can make a huge impact on the sample mean)
 
  Skew of a sample
  * The skew of a sample refers to whether the non-erroneous outliers tend to be larger or smaller than the sample mean
  * The **difference** between the **sample mean** and **sample median** often inform us about the skew
     * Sample mean $<$ sample median generally indicates that the distribution of a variable is left skewed (ex. the outliers are small).
     * Sample mean $>$ sample median generally indicates that the distribution of a variable is right skewed (ex. the outliers are big. Think of the income example and how including Oprah's income in the sample would skew the distribution by a lot)

    The influence of outliers on sample mean vs. sample median
    * A general principle is that sample means have a greater influence on outliers as opposed to sample medians. So when thinking about whether you should describe a variable withe a with a mean of median you should ask yourself if you want your statistic to be influenced by outliers and to what extent?
    * ex. if you want to learn more about the total income in a country then you would want outliers to be a part of your statistic (using sample mean) but if you want to obtain the income of a typical individual, then you can remove the outliers (using sample median).

   Mode: The value that occurs most commonly in the data

  Reasons why the mode is discussed less than the mean and median
  1. Not necessarily unique
  2. Relatively useless with many continous variables
 
  But mode is still a useful concept to know because
  1. It provides a sense of the a typical response. Especially when the most common values are not next to each other (ex. **bimodal variables/distributions** which have 2 peaks)
  2. 
   
    
# DATA 410: Advanced Topics in Data Analytics
