Categorical variables: contain a finite number of categories or distinct groups. Categorical data might not have a logical order.
	ex. categorical predictors include gender, material type, and payment method
	type: <chr>
Continuous variables: numeric variables that have an infinite number of values between any two values. 
	ex. can be numeric or date/time
	typle: <int> <dbl>

Asthetics: Can be used to change a specific variable. The way the variable is changed depends on its type (categorical or continuous)
	1.size
		categorical: Each category is a different size 
		continuous: Each number is a different size
	2.alpha
		categorical: Each category is a different shade of black
		continuous: Each number is a different shade of black
	3.shape
		categorical: Each category is a different shape
		continuous: Error, can not be mapped
	4.color
		categorical: Each category is a different color
		continuous: A color scale is used so each category is a particular shade of a color
		
# dataset
mpg

# tells us more info about mpg
?mpg 

# rows and columns

nrow(mpg)
ncol(mpg)
glimpse(mpg)

# scatterplot
ggplot(data=mpg) +
  geom_point(mapping=aes(x=cyl,y=hwy))

# scatterplot with specific color
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")
  
# scatterplot with each class representing a color (can be changes to other aesthetics)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color=class))

  
<color=class> class is a catagorical variable and will display the points through seperate colors/groups
<color=cyl> class is a continous variable and will display the points through a scale
  




