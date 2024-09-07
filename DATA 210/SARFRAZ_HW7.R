
library(anesrake)
library(weights)
library(tidyverse)

setwd("C:/Users/hussainsarfraz/Desktop/DATA 210")

load("felons.RData")

view(felons)

### Question 1 - C

#removing the 161 people in the dataset who returned to prison before the experiment was conducted
felons.tidy <- felons[felons$returntoprison==0, ] 

# felons.tidy <- felons %>%
#   filter(returntoprison==0) 

felons.tidy$returntoprison <- NULL

blocks <- unique(felons.tidy[ ,c('felony_type',"vote12")])
felons.tidy$treated.block <- F

ii <- 1

for(ii in 1:nrow(blocks)){
  
  this.block <- blocks[ii,]
  
  rows <- which(  felons.tidy$felony_type       == this.block$felony_type & 
                  felons.tidy$vote12               == this.block$vote12)
  
  felons.tidy$treated.block[sample(rows, 
                                     size = round(length(rows)/2), 
                                     replace = F)] <- T
  
}

table(felony = felons.tidy$felony_type ,
      vote = felons.tidy$vote12,
      treated = felons.tidy$treated.block)

### Question 1 - D.i

table( age = t.test(felons.tidy$age[felons.tidy$age <= 30],
       felons.tidy$age[felons.tidy$age > 30]),

days.served = t.test(felons.tidy$days_served[felons.tidy$days_served <= 500],
       felons.tidy$days_served[felons.tidy$days_served > 500]),

years_since_release = t.test(felons.tidy$yrs_since_release[felons.tidy$yrs_since_release <= 2],
       felons.tidy$yrs_since_release[felons.tidy$yrs_since_release > 1]),

vote08 = t.test(felons.tidy$vote08[felons.tidy$vote08 == 1],
       felons.tidy$vote08[felons.tidy$vote08 == 0]) )


### Question 1 - D.ii


summary(lm(felony_type ~ treated.block, data = felons.tidy))

### Question 1 - D.iii


