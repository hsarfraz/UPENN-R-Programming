#----------------------------------------------
# DATA 101
# Homework 1: Hussain Sarfraz
#----------------------------------------------

# Please save this script as "YOUR LAST NAME_HW1.R" and upload the script to Canvas. 
# You should also upload a word document containing your write up and graphs.
# Please type your code into the sections outlined below. 

library(tidyverse) #load the tidyverse library first
mpg #load the dataset next
#----------------------------------------------
# Question 1 
mpg$cyl #Displayed all values of cyl and hwy to understand data better
mpg$hwy

ggplot(data=mpg)+
  geom_boxplot(mapping=aes(x=as.factor(cyl),y=hwy))+coord_flip()

#----------------------------------------------
# Question 2
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline() +
  coord_fixed() +
  geom_smooth()   # I added geom_smooth() to double check if my observations
                  # about geom_abline() showing a positive correlation between cty and hwy were correct

#----------------------------------------------
# Question 3

ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy,color=drv))
