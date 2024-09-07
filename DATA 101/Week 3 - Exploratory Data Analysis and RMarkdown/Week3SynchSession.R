# DATA 101 Class 3

#------------------------------------------

# Read in the data
# There's a lot here- i"ve only given very brief descriptions of what each
# line is doing, but for now we can just accept
# that is how we're prepping our data. 


library(tidyverse)

# read in the data from the internet
marriage <- read.csv(url("https://raw.githubusercontent.com/glago66/Final-Project/master/affairs.csv"))

# renaming column names
colnames(marriage) <- c("participant", "gender", "age", "years_married", "children", "religious", "education",
                        "occupation", "marriage_rating", "number_of_affairs")

marriage <- as_tibble(marriage)

#--------------------------------------------
# From here, we can start our Exploratory Data Analysis

# Step 1: Look at the data
head(marriage)
View(marriage)

summary(marriage)


#Let's review the commands that we learned last week, and in particular
#how to put them together

#### filter() :subset your data (remove unwanted data)
#Focus on only certain rows
filter(marriage, age>=30)


#### select() : isolate a particular variable of interest
#Focus on only certain columns

select(marriage, gender, occupation)

#### summarise() : get a summary statistic
summarise(marriage, median.education = median(education))

#### arrange() : print out your data in a certain way
arrange(marriage, marriage_rating)
arrange(marriage, desc(marriage_rating))

#### mutate(): create a new variable using other information in the dataset (feature engineering)

mutate(marriage, age_months=age*12)
#This created it but didn't save the result
#We can use select in conjunction to view this result

mutate(marriage, age_months=age*12) %>%
  select(age, age_months)


###Someone asked about transmute last week, compared to mutate. 
### Transmute is like mutate, but doesn't keep the inputs, soooooo
# BE CAREFUL WITH TRANSMUTE

#Example:

#First, let's duplicate "marriage"

marriage2<-marriage

#Now let's see transmute in action vs mutate

mutate(marriage2, age_months=age*12)

transmute(marriage2, age_months=age*12)

geom_count()
#### ifelse() : make a conditional statement (if this, then do that, if not this, then do somethine else.)

mutate(marriage, senior = ifelse(age>=65, 1, 0)) %>%
  select(age, senior) %>%
  arrange(desc(age))

summarize(marriage, maxage=max(age))

#### group_by() : apply to categorical variables to define subgroups within the variable

group_by(marriage, gender)

#This is not particularly helpful on its own...

marriage %>%
  group_by(gender)%>%
  summarise(mean(number_of_affairs))

#TGroup_by is super powerful when it comes to things like 
#integreating data into ggplot


#In the videos/handouts for this week and Using the nycflights data,
#we had this graph which looks at the day of a flight
#and the average delay
library(nycflights13)
library(lubridate)

flights %>%
  mutate(flight_day = make_datetime(year,month,day)) %>%
  group_by(flight_day) %>%
  summarise(avg_delay = mean(arr_delay, na.rm=T))%>%
  ggplot(.,mapping=aes(x=flight_day, y=avg_delay)) + 
  geom_point()+ 
  geom_smooth()

#What if We then wanted to determine if these patterns changed by airport origin.

#-------------------------------
#One might question why this isn't the answer:

flights %>%
  mutate(flight_day = make_datetime(year,month,day)) %>%
  group_by(flight_day, origin) %>%
  summarise(avg_delay = mean(arr_delay, na.rm=T))%>%
  ggplot(.,mapping=aes(x=flight_day, y=avg_delay, color=origin)) + 
  geom_point()+ 
  geom_smooth()

#It seems that we want to do is to use the color option to group the points by
#their airport of origin, but this doesn't work....

#We've got to think through what new datasets are being created in the pipe. 

#Let's think this through step by ste


flights %>%
  mutate(flight_day = make_datetime(year,month,day))

#Creates a new dataset, identical to flights, but with the new variable
#flight day included

#We can view
flights %>%
  mutate(flight_day = make_datetime(year,month,day))%>%
  select(year,month,day,flight_day)


#Next we tell R that we want to group by this new variable:
flights %>%
  mutate(flight_day = make_datetime(year,month,day)) %>%
  group_by(flight_day)

#Which we learn has 365 entries (which make sense)  

#Now we use the summarise command. If we use summarise after group_by
#the summaries will be for each group. 

flights %>%
  mutate(flight_day = make_datetime(year,month,day)) %>%
  group_by(flight_day) %>%
  summarise(avg_delay = mean(arr_delay, na.rm=T))

#Look at the dimensions of our tibble now. It is 365 rows (one for each day)
#and has a variable for day of the year and the average delay for that day


#This is the dataset we are graphing. There is *no* variable 
#called origin in this dataset.

flights %>%
  mutate(flight_day = make_datetime(year,month,day)) %>%
  group_by(flight_day) %>%
  summarise(avg_delay = mean(arr_delay, na.rm=T))%>%
  ggplot(.,mapping=aes(x=flight_day, y=avg_delay)) + 
  geom_point()+ 
  geom_smooth()

#So how can we make sure the dataset we port into R has origin in it?

#What happens when we add origin to our group_by function?
flights %>%
  mutate(flight_day = make_datetime(year,month,day)) %>%
  group_by(flight_day,origin)

#Notice now the number of groups is 1095. 
#There are 3 origin airports and 365 days
3*365

#So when we add groups to the group_by function we start getting all the unique combinations.

#Let's see what the resulting dataset looks like when we summarise with our new group structure

flights %>%
  mutate(flight_day = make_datetime(year,month,day)) %>%
  group_by(flight_day, origin) %>%
  summarise(avg_delay = mean(arr_delay, na.rm=T))

#Now we have three entries for january first with the average delay for each origin 
#airport, three for january 2nd, three for january 3rd... etc.

#Now that we have a dataset with both day and origin, we can port into ggplot

flights %>%
  mutate(flight_day = make_datetime(year,month,day)) %>%
  group_by(flight_day, origin) %>%
  summarise(avg_delay = mean(arr_delay, na.rm=T))%>%
  ggplot(.,mapping=aes(x=flight_day, y=avg_delay, color=origin)) + 
  geom_point(alpha=.5)+ 
  geom_smooth()


#We always have the option of breaking these long strings of piped together commands
#into their constituent parts and examining what is happening within them. 

#TidyR is an amazing way of coding that opens up many possibilities. 
#My major critique of it is that so much happens invisibly that it can be hard to keep
#track of what sort of data is getting piped to the next command. But, if you are going
#to spend time learning it's best to learn TidyR, because it's now the standard. 
#It's way harder to learn base R and then try to re-learn everything in TidyR. (Believe me!)


##Let's think more about exploratory data analysis with the marriage 
#data

# With exploratory analyis we start out pretty general
# and get more granular. 

# One variable analysis - really you're just getting
# feel for the dataset
ggplot(marriage)+
  geom_bar(aes(x=children))

ggplot(marriage)+
  geom_bar(aes(x=religious))

ggplot(marriage)+
  geom_bar(aes(x=number_of_affairs))


ggplot(marriage)+
  geom_bar(aes(x=gender))

ggplot(marriage)+
  geom_bar(aes(x=education))

ggplot(marriage)+
  geom_bar(aes(x=marriage_rating))


# Two variables
# Covariance - now we start getting into some write-up
# worthy analysis!



# How would you display the spread of the 
# number of affairs by gender?

#Let's think through both variables
#Gender is a categorical variable with two categories
#Number of affairs is an ordinal categorical variable

#What can we do with two variables like this?

#Scatterplot? 
#Not with categorical variables...

#Boxplot by gender?
#Technically, yes. But it might misconstrue the nature of the number of affairs variable

ggplot(marriage)+
  geom_boxplot(aes(y=number_of_affairs, x=gender))

#There isn't much spread, and a lot of outliers. More so it makes it look like there could 
#by 1.3 affairs, when that's not the case...



#Better is to use a bar plot:
# here's one example
ggplot(marriage)+
  geom_bar(aes(x=number_of_affairs, fill=gender), position="dodge")

#with stacked bars
ggplot(marriage)+
  geom_bar(aes(x=number_of_affairs, fill=gender), position="stack")

#For what it's worth, I find these extremely hard to read....and they get worse as you add more levels


# Some slight differences by gender, but not really all that much
# let's see if digging in deeper reveals any other correlations

# what about looking at how education might affect this relationship?

# We have two continuous-ish variables, and one categorical,
# so lets try a scatter plot
ggplot(marriage)+
  geom_point(aes(x=education, y=number_of_affairs, color=gender))

# I wouldn't recommend turning in a scatterplot that looks like this, but
# it's a helpful way to get a quick look at your data and see if any patterns emerge

# What can we tell from this plot?
# it looks like of the people who had 12 affairs, the men have higher education levels than the women
# but also this could be a lot more useful if we had a sense of scale. When we 
#make a scatterplot with categorical variables points are getting stacked on top of each other
#so there could be one person at the coordinates of (12,19) or one thousand.  
table(marriage$education)
table(marriage$number_of_affairs)
table(marriage$age)
# Let's try another geometry
ggplot(marriage)+
  geom_count(aes(x=education, y=number_of_affairs, color=gender))

#here, at least, we get a sense of how many observations are in each area. 


# Can we rephrase this question to make it clearer? Oftentimes we can
# reduce what we need to do by simplify a variable in our question.
# Is there a difference in number of affairs between genders among those with
# and without a college degree? How would we proceed?


# Let's assume that those with at least 16 years of education
#have a 4-year degree in this dataset

# At the end we want 4 numbers, the average number of affairs for
#men with college degree
#men without college degree
#women with college degree
#women without college degree

#Is there a variable that tells us if someone has a college degree or not right now?
marriage

#No, so we'll have to create that.

marriage %>%
  mutate(college = ifelse(education > 15,"College Degree","No College Degree"))
  
#If we want stats for these two groups, gender and college, how do we do that?

#group_by()!
marriage %>%
  mutate(college = ifelse(education > 15,"College Degree","No College Degree")) %>%
  group_by(gender, college) 

#We now have the 4 groups we want. So applying summarise to this
#will give us a statistic for these 4 groups

# then look at that average for each group
marriage %>%
  mutate(college = ifelse(education > 15,"College Degree","No College Degree")) %>%
  group_by(gender, college) %>%
  summarise(avg_affairs = mean(number_of_affairs))

#Porting this into ggplot:

# awesome - now we're going to take this data we just put together and put it in a chart
marriage %>%
  mutate(college = ifelse(education > 15,"College Degree","No College Degree")) %>%
  group_by(gender, college) %>%
  summarise(avg_affairs = mean(number_of_affairs)) %>%
  ggplot(.) +
  geom_bar(aes(x=gender,y=avg_affairs,fill=college), position="dodge", stat="identity")

#Interestingly, for women having a college degree makes them less likely to cheat, but men having a college
#degree makes them more likely to cheat.
#Not going to read into this too much.

# notice here I use stat="identity" in my chart
# usually geom_bar() will count your observations, but that's not always what you want to display
# I have already calculated the number I want to graph here, so how do I make it just show me that?
# that's where stat="identity" comes in - you're telling R that the stat you are trying to show is what you've already defined

# just for fun, let's see what happens if we leave that out...
marriage %>%
  mutate(college = ifelse(education > 15,"College Degree","No College Degree")) %>%
  group_by(gender, college) %>%
  summarise(avg_affairs = mean(number_of_affairs)) %>%
  ggplot() +
  geom_bar(aes(x=college,y=avg_affairs,fill=gender), position="dodge")
# we see this message:
# Error: stat_count() must not be used with a y aesthetic.
# if you're defining a y variable for a bar chart, you have to do a little extra work to tell R how to handle it
# otherwise, it tries to default to counting, which does not work well here



# Let's make our graphs of interest print-ready!
# What should we do to make our plot more attractive?

#Better labels, a title, better legend

library(scales)
marriage %>%
  mutate(college = ifelse(education > 15,"College Degree","No College Degree")) %>%
  group_by(gender, college) %>%
  summarise(avg_affairs = mean(number_of_affairs)) %>%
  ggplot(.) +
  geom_bar(aes(x=gender,y=avg_affairs,fill=college), position="dodge", stat="identity")+
  ggtitle("Relationship between College Education and Affairs, by Gender")+
  ylab("Average Number of Affairs") + 
  xlab("") +
  scale_x_discrete(labels=c("Female", "Male"))+
  scale_fill_discrete(name="")

#Looks good!





