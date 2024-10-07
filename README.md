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

Central Tendency
* The central tendency of a variable is the information about the typical value, of the a variabe, within a sample
* The most instinctive measure of central tendency is the **sample mean** (formula below). y is the observation for each unit, in the sample, while n is the total number of units.
  * The sample mean is a special case of a weighted sample mean (defined below) in which all units have the same weight of one. This means that all the units are equally influential because $w_1 = w_2 = ... = w_n = 1$

$$ \overline{y} = { y_1 + y_2 + ... + y_n \over n} = {1 \over n}{\sum_{i=1}^n y_i}$$

* Sometimes it is useful to think about the sample mean as a special case of a **weighted sample mean**:

$$ \overline{y} = { w_1y_1 + w_2y_2 + ... + w_ny_n \over w_1+w_2+...+w_n} = {\sum_{i=1}^n w_iy_i \over \sum_{i=1}^n w_i}$$

# DATA 410: Advanced Topics in Data Analytics
