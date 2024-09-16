# Data used in Shiny Prediction App
load("finaldata2.rda")

# Just the columns we're interested in for this analysis
regvars<- c("ACE_BI", "Race",  "Absence", "Contact", "Homework", "Lunch", "Retained")

# Let's just take a peak at this variables
data %>% 
  select(regvars) %>% 
  head()

# Race Variable is the only variable with more than 2 levels:
table(data$Race)


