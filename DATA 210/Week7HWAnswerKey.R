#### Pset 6 Answer key
####



##########
# Experiments question
##########


load("felons.RData")

#######
## 1A
#######
## see Gerber et. al p 914 -- comparing former felons to people who have never served time for a felony offense is a problematic 
## research design because "the same choices and decisions that eventually led to a felony decision also reduce levels of political 
## participation", making non-felons a poor control group for understanding how being CONVICTED/INCARCERATED affect voting rates. In 
## other words as these non-felons systematically differ from felons, if we use non-felons as the control group, we cannot separate 
## out the effects of these preceeding factors from the effects of being convicted and incarcerated. In short, confounding influences 
## are not addressed with this research design. As a result, you are likely to overestimate the effect of serving prison time.

#######
## 1B
#######

# (i) What are the causal effect(s) that the authors are interested in studying?
#   Effect of informing an ex-felon that they are eligible to vote on their voter registration status and turnout in the 2012 election.
# 
# (ii) Describe the treatment and control conditions in the experiment.
# People in the control group received no mailer nor any contact at all
# People in the treatment group were randomly assigned to receive one of two mailers. The first simply informed them that 
# they were eligible to vote. The second contained the same message as the first, but provided some additional information.
# 
# (iii) Describe the randomization strategy that the authors used.
# They used a block randomized experiment approach, where the treatment was randomly assigned within blocks which were defined 
# by the type of crime that the person was convicted for. Within each block, half of people were assigned to the control group, 
# a quarter were assigned to the first treatment category, and the remaining quarter were assigned to the second treatment category.


#######
## 1C
#######

## Remove the 161 recidivists who returned to prison

felons <- felons[felons$returntoprison == 0,]

## Create 'treatment_collapsed'

felons$treatment_collapsed <- felons$treatment %in% c(2,3)



#######
## 1D.i
#######

## Check balance

# (i)

balance.table <- data.frame(attribute = c("Age","Days served in prison","Years since release from prison","Turnout 2008"),
                            mean.control = NA,
                            mean.treat = NA,
                            p.value = NA)



tt <- t.test(felons$age[!felons$treatment_collapsed], felons$age[felons$treatment_collapsed])
tt <- t.test(felons$age ~ felons$treatment_collapsed) ## You could also do it this way
balance.table$mean.control[1] <- tt$estimate[1]
balance.table$mean.treat[1] <- tt$estimate[2]
balance.table$p.value[1] <- tt$p.value

tt <- t.test(felons$days_served ~ felons$treatment_collapsed)
balance.table$mean.control[2] <- tt$estimate[1]
balance.table$mean.treat[2] <- tt$estimate[2]
balance.table$p.value[2] <- tt$p.value

tt <- t.test(felons$yrs_since_release ~ felons$treatment_collapsed)
balance.table$mean.control[3] <- tt$estimate[1]
balance.table$mean.treat[3] <- tt$estimate[2]
balance.table$p.value[3] <- tt$p.value

tt <- t.test(felons$vote08 ~ felons$treatment_collapsed)
balance.table$mean.control[4] <- tt$estimate[1]
balance.table$mean.treat[4] <- tt$estimate[2]
balance.table$p.value[4] <- tt$p.value


## There doesn't appear to be imbalance on any of the covariates. You may have noticed that
## age is statistically significant at the p<0.10 level. This isn't surprising, given the
## large number of things in our regression. In fact, we would expect 10% of our variables
## to give us a statistically significant result at the p<0.10 level, even if (in reality)
## none of the variables were actually really predictive of treatment. 


#######
## 1D.ii
#######
## Use Linear regression to assess whether the tye of crime predicts whether somebody ended 
## up in the treatment or control group  

summary(lm(treatment_collapsed ~ felony_type, data = felons))
## We fail to find evidence that felony type predicts whether somebody ended up in the treatment or control group

#######
## 1D.iii
#######


summary(lm(treatment_collapsed ~ felony_type + age + days_served + yrs_since_release + vote08, 
           data = felons))
## None of these variables are statistically significant at conventionial levels. 
## This output indicates that the treatment and control groups are balanced on these variables. 



#######
## 1E
#######

summary(lm(registered ~ treatment_collapsed, 
           data = felons)) 
## This treatment variable has a statistically significant effect on registration at the  p < .05 level, 
## with an effect size of ~ 1.8 percentage points

summary(lm(vote12 ~ treatment_collapsed, 
           data = felons)) 
## This treatment variable has a statistically significant effect on voting at the p < .05 level, with a 
## (smaller) effect size of ~ 0.9 percentage points.  

#######
## 1F
#######

summary(lm(registered ~ treatment_collapsed + vote08 + age + days_served + yrs_since_release + felony_type, 
           data = felons)) 
## The treatment remains statistically significant at the p < .05 level (effect size 2.2%)

summary(lm(vote12 ~ treatment_collapsed + vote08 + age + days_served + yrs_since_release + felony_type, 
           data = felons)) 
## Once demographic variables are added to this model, the treatment no longer has a significant effect at 
## conventional levels of statistical significance.
