### Module 3 - Exploratory Data Analysis

library(tidyverse)

# we'll be using a dataset called 'diamonds' which contains informatino about 50,000 diamonds and their carat, cut, color, clarity, price, length (x), width (y), and depth (z)
summary(diamonds)

# make a plot of the distribution of the price variable
ggplot(data = diamonds) +
  geom_histogram(aes(x = price))
# notice the warning message that pops up below - this means the data is being divided into 30 'bins' to display in the chart and R is prompting you that you probably need more

# let's try using 250 bins to display the daa instead of the default 30
ggplot(data = diamonds) + 
  geom_histogram(aes(x = price), bins = 250)

# now let's take a look at the distribution of carat (size) of the diamonds
ggplot(data = diamonds) +
  geom_histogram(aes(x = carat), bins = 250)

# notice here you see some spikes around relatively round numbers (0.75, 1, 1.5, 2, etc.)
# let's look more closely at the data around that 1 carat spike
diamonds %>%
  filter(carat > 0.89, carat <= 1.1) %>%    # this selects all the records with more than 0.89 carats and less than or equal to 1.1 carats
  count(carat) %>%   # counting the number of diamonds by carat
  print(n = 22)  # printing all 22 records instead of the default 10 so we can see everything

# now let's look at the relationship between carat and price
ggplot(diamonds) +
  geom_point(mapping=aes(x = carat, y = price))
# definitely a relationship here - as carats get larger, price gets higher

# let's add color to the plot - we're going to use the color aesthetic to display the color variable - don't get confused by the color = color in the command below!
ggplot(data = diamonds) +
  geom_point(aes(x = carat, y = price, color = color), alpha = 0.5)
# we're using alpha again here, which you saw last week, to make the dots a little more transparent
# D here means the least color, and J is the most color

# now let's investigate the different cuts of the diamonds in the dataset
ggplot(data = diamonds) +
  geom_bar(aes(x = cut))
# you see here that we have lots of ideal cut diamonds in our dataset

# let's look at a boxplot of price, grouped by different cuts
ggplot(data = diamonds) +
  geom_boxplot(aes(x = cut, y = price)) +
  coord_flip()
# not a ton of variation here, so let's look at a scatterplot and see what that tells us
ggplot(data = diamonds) +
  geom_point(aes(x = carat, y = price, color = cut), alpha = 0.5)
# things are a little bit more all over the place here than in the graph showing color and price, so you might say the relationship between cut and price isn't quite as strong

# we can also look at the relationship between two categorical variables in a geom_count graph, which can be helpful
ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))
# here you can see that the largest number of diamonds in our dataset are ideal cut diamonds with coloration G

# after taking a look at our data, we've seen that carat seems to be the most important indicator of a diamon's price
# you'll explore more of this dataset in the homework!

# one last thing to show you here - here's another line of code that makes the same graph you just made above 
ggplot(diamonds) +
  geom_count(aes(x = cut, y = color))
# notice it doesn't include "data =" or "mapping ="
# we've included those in all the examples up until now for teaching purposes, but they are not actually necessary here
# if you look at the help for the ggplot command...
?ggplot
# you'll see under "Usage" that the first thing you are supposed to define is the data
# generally, you do not have to use "whatever =" for the first thing you put into an R command because R knows the first thing you have to put in here is the dataset
# so if you just put ggplot(diamonds) instead of ggplot(data = diamonds), R understands what you're trying to do - it assumes the first thing you put in is the data
# there is no right or wrong way to do this - you can use "data =" forever or you can never use it again. do whatever makes you feel most comfortable and whatever helps you best understand the code you are writing!




# ok, now let's go back to the flights dataset and explore that data some more!
# now we'll do some more work to try to figure out when we're most likely to see delays

# let's start by thinking about seasonality and if that matters for flight delays
# now group the data by day and calculate the average arrival delay by day
flights %>%
  group_by(month, day) %>%  # grouping by month and day, but not year because we want to look at March 8 in every year combined
  summarise(avg_delay = mean(arr_delay, na.rm=T)) %>%    # saying na.rm=T is the same as saying na.rm=TRUE - you can use T as shorthand for TRUE and F as shorthand for FALSE in R commands
  ungroup() %>%  # ungroup here to pull back from the month grouping to look at days
  arrange(-avg_delay) %>%   # arrange by average delay descending
  head(10)  # the head() command just pulls the top x number of rows from the dataset

# remember that this datset includes negative delays, which makes it harder to understand the data. let's get rid of those
flights %>%
  filter(arr_delay >= 0) %>%
  group_by(month, day) %>%
  summarise(avg_delay = mean(arr_delay, na.rm=T)) %>%
  ungroup() %>%
  arrange(-avg_delay) %>%
  head(10)
# now we see there are three days where the average delay was more than 100 minutes!
# moving forward, we'll keep the negative delays in the dataset, but it's good to see how taking them out changed things!

# now we'll load a new library called lubridate, which we'll cover later
# for now all you need to know is that dates are actually a special type of data
# if you have data formatted as dates, R will understand that March 26, 2020 is after August 23, 2019, and September 27, 2019 is between the two, etc.
# remember if you're using a package for the first time, you have to install it with install.packages() before loading it with library()
install.packages("lubridate")
library(lubridate)

flights %>%
  mutate(flight_day = make_datetime(year, month, day)) %>%  # the make_datetime() command is from the lubridate package, and is turning your year, month, and day fields into a date type variable so R understands it better and we can more easily plot things over time!
  group_by(flight_day) %>%
  summarise(avg_delay = mean(arr_delay, na.rm=T)) %>%
  ggplot(., mapping = aes(x = flight_day, y = avg_delay)) +   # remember the . at the beginning here means we're taking everything above and putting that in as the data to use for the plot
  geom_point() +
  geom_smooth()
# you can see on this plot that delays seem to be higher in the summer (around July) and lower around October


# now let's look at how to clean up our plots a little
# what you've made so far is fine for taking a look at the data, but you'll generally want a nicer-looking plot if you're presenting to someone or turning something in for homework (hint hint)

# one trick you can use to avoid having to repeat lots of code is saving your data to an object so you don't have to group and summarise data again every time you're making a graph
# i'm going to save our edited data to an object called flight_graph to use as the data in the plots below
flight_graph <- flights %>%
                  mutate(flight_day = make_datetime(year, month, day)) %>%
                  group_by(flight_day, origin) %>%   # grouping by flight_day AND origin, which you would have done in the test yourself questions before this - if you haven't gotten here yet, go do those now! :)
                  summarise(avg_delay = mean(arr_delay, na.rm=T))

# we're starting with this graph
ggplot(flight_graph, mapping = aes(x = flight_day, y = avg_delay)) +
  geom_point(mapping = aes(color = origin)) +
  geom_smooth()

# now we're going to install another package called scales to help us make this all a little nicer
install.packages("scales")
library(scales)
# and now add some more lines to our graph from above to make things nicer!
ggplot(flight_graph, mapping = aes(x = flight_day, y = avg_delay)) +
  geom_point(mapping = aes(color = origin)) +
  geom_smooth() +
  ylab("Average Arrival Delay (Minutes)") +   # putting a label on the y axis
  xlab("") +   # this removes the label on the x axis, which we don't really need here since it's clear we're looking at dates
  scale_x_datetime(date_breaks = "3 months", labels = date_format("%b")) +  # making the x axis scale prettier - run "?scale_x_datetime" to see how this works, look at this site to see what the "%b" means here: https://www.statmethods.net/input/dates.html 
  scale_y_continuous(breaks = c(0,30,60,90,120)) +  # defining the breaks to use on the y axis
  ggtitle("Average Daily Flight Delay by NYC Airport, 2013")  # adding a title for the overall graph
  
# what if we want to center the title? add in the last line of code here
ggplot(flight_graph, mapping = aes(x = flight_day, y = avg_delay)) +
  geom_point(mapping = aes(color = origin)) +
  geom_smooth() +
  ylab("Average Arrival Delay (Minutes)") +
  xlab("") +
  scale_x_datetime(date_breaks="3 months", labels = date_format("%b")) +
  scale_y_continuous(breaks = c(0,30,60,90,120)) +
  ggtitle("Average Daily Flight Delay by NYC Airport, 2013") +
  theme(plot.title = element_text(hjust = 0.5))

# you can do a lot with the theme command that we added above, take a look at the help file if you're curious!
?theme

# now let's make the legend a bit more readable for people who don't know the airport codes by adding another line of code
ggplot(flight_graph, mapping = aes(x = flight_day, y = avg_delay)) +
  geom_point(mapping = aes(color = origin)) +
  geom_smooth() +
  ylab("Average Arrival Delay (Minutes)") +
  xlab("") +
  scale_x_datetime(date_breaks="3 months", labels = date_format("%b")) +
  scale_y_continuous(breaks = c(0,30,60,90,120)) +
  ggtitle("Average Daily Flight Delay by NYC Airport, 2013") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(name = "Originating Airport", labels = c("Newark","Kennedy","LaGuardia"))

# what if we want to move the legend? sometimes we don't want it to be over on the right side squishing the graph
# i'm adding another option to the theme() command below, take a look at what happens when that's added
ggplot(flight_graph, mapping = aes(x = flight_day, y = avg_delay)) +
  geom_point(mapping = aes(color = origin)) +
  geom_smooth() +
  ylab("Average Arrival Delay (Minutes)") +
  xlab("") +
  scale_x_datetime(date_breaks="3 months", labels = date_format("%b")) +
  scale_y_continuous(breaks = c(0,30,60,90,120)) +
  ggtitle("Average Daily Flight Delay by NYC Airport, 2013") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  scale_color_discrete(name = "Originating Airport", labels = c("Newark","Kennedy","LaGuardia"))

# now what if we hate the ggplot standard theme with the gray background? let's try adding a new line to change the overall theme of the chart
# notice i put the theme_bw() command before i messed with the legend position. if you put theme_bw() at the end, the legend position goes back to where it was before.
ggplot(flight_graph, mapping = aes(x = flight_day, y = avg_delay)) +
  geom_point(mapping = aes(color = origin)) +
  geom_smooth() +
  ylab("Average Arrival Delay (Minutes)") +
  xlab("") +
  scale_x_datetime(date_breaks="3 months", labels = date_format("%b")) +
  scale_y_continuous(breaks = c(0,30,60,90,120)) +
  ggtitle("Average Daily Flight Delay by NYC Airport, 2013") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  scale_color_discrete(name = "Originating Airport", labels = c("Newark","Kennedy","LaGuardia"))

# there are a lot of fun things you can do with themes if you want to do some googling
# for example, there's a theme you can use that makes your charts look like charts in The Economist
install.packages("ggthemes")
library(ggthemes)

ggplot(flight_graph, mapping = aes(x = flight_day, y = avg_delay)) +
  geom_point(mapping = aes(color = origin)) +
  geom_smooth(color="#4a4a4a") +
  ylab("Average Arrival Delay (Minutes)") +
  xlab("") +
  scale_x_datetime(date_breaks="3 months", labels = date_format("%b")) +
  scale_y_continuous(breaks = c(0,30,60,90,120)) +
  ggtitle("Average Daily Flight Delay by NYC Airport, 2013") +
  theme_economist() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  scale_color_economist(name = "Originating Airport", labels = c("Newark","Kennedy","LaGuardia"))
# notice i changed the scale_color_discrete on the last line here to scale_color_economist to get the economist colors here
# things like this may vary by theme, you can google to find out more
# i also changed the color of the geom_smooth() line by putting in a hex code for a dark gray color
# you can use standard colors like "red" or "blue" or "gray" or whatever here OR you can get more specific using hex codes. you can explore those here: https://www.w3schools.com/colors/colors_picker.asp
# you can also explore all some more specific color names here: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf 

