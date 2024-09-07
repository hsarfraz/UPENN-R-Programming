---
title: "SARFRAZ_HW6"
author: "Hussain Sarfraz"
date: "10/12/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(RColorBrewer)
library(maps)
library(mapdata)
library(tidyverse)
library(scales)
library(readr)
```


## Question 1

To start off, I need to load the **readr** library since I want to load a csv file (**county_returns.csv**). To do this I need to establish my working directory by writing down the file path of where my file is stored. I establish the file path through the **setwd()** function and create a object **election_dataset** to store the information in the **county_returns.csv** dataset.

```{r}
setwd("C:/Users/hussainsarfraz/Desktop/DATA 101/Week 6 - Mapping")
election_dataset <-  read_csv('county_returns.csv')
```

### Question 1: Part A

I would need to launch the **maps** and **mapdata** libraries to merge the county-level mapping data with the electoral data that I have. After I do that I would need to create a object that stores the data of the counties (I do this by creating the object **counties**). 

So now I have a object that has the county data, but there is one problem with the county data. It does not have a **fips** code and this is important for my to have because I will be using the **fips** code to join the elections dataset with the county-level dataset. To solve this problem I will create another object that contains the **fips** dataset (I do this by creating the **county_fips** object). 

I now need to merge the **counties** and **county_fips** datasets so the **counties** dataset can have a fips code column in it. I do this by using the **mutate()** function to create a common column between the **counties** and **county_fips** datasets. I then use **left_join()** to merge the two datasets together. I then create a object called **fullcounty_dataset** which stores the dataset of counties with a fips code.

Now that I have a dataset of counties with a fips code I would need to merge the data with the election dataset. I do this by using **left_join()** and use the fips code to join the **election_dataset** and **fullcounty_dataset** together. The **fullelection_datset** object stores the dataset of the election and county data.

```{r}
counties <- map_data("county")

county_fips <- county.fips 

fullcounty_dataset <- counties %>%
  mutate(polyname = paste(region,subregion,sep=",")) %>%
  left_join(county_fips, by="polyname") 
 
fullelection_datset <- election_dataset %>%
  left_join(fullcounty_dataset, by="fips")
```

### Question 1: Part B

Here is how I created each new variable:

1. **clinton_prop**: I calculated the proportion of votes won by Hillary Clinton in the 2016 election by dividing the number of votes Hillary got (the **clinton** variable in the dataset represented this) over the total number of votes in 2016 ( the **total_votes_2016** variable represents this information). I then multiplied this number by 100 to put the proportion in a 0-100 scale instead of a 0-1 scale.
2. **trump_prop**: I calculated the proportion of votes won by Donald Trump in the 2016 election by dividing the number of votes Trump got (the **trump** variable in the dataset represented this) over the total number of votes in 2016 ( the **total_votes_2016** variable represents this information). I then multiplied this number by 100 to put the proportion in a 0-100 scale instead of a 0-1 scale.
3. **clinton_won**: To see if Hillary Clinton won majority of the votes I decided to compare the proportion of votes Hillary won with the proportion of votes Donald Trump won. I did this by using the **>** operator. The **ifelse()** function was used to show that if Hillary won majority of the votes for each county then a 1 would appear, but if Hillary lost then a 0 would appear in the dataset.

```{r}
HandT_election_results <- fullelection_datset %>%
  mutate(clinton_prop = (clinton/total_votes_2016)*100,
         trump_prop = (trump/total_votes_2016)*100,
         clinton_won = ifelse(clinton_prop > trump_prop ,1,0))
```

## Question 2

For this question I have divided my answer in two parts. The first part will display the map county-level map of the election results of Hillary and Trump. The Second part will show a state-level map of the election results of Hillary and Trump.

#### Part 1: county-level map of the Hillary and Trump election results

So when making these two maps I realized that R would display my colors as a gradient and I did not want that since I am only using 2 colors (red and blue) in this map. That is why I decided to do some additional research and realized that I can create my own color palette in R! I did not create my own color palette from scratch but got to choose which colors I wanted from the already existing color	schemes created by Cynthia Brewer,	a	Professor	of	Geography	at	Penn	State	University. This is why I installed and launched the **RColorBrewer** library. The **brewer.pal()** function was used to let R know which color scheme I wanted to pick my colors from and I then picked my colors based of the position they had in the **RdYlBu** color scheme. In this example I picked Blue and Red.

When plotting my map I needed to let R know that the values in the **clinton_won** column are not numeric/continuous and are instead non-numeric/categorical. I used the **as.factor()** function to do this. I needed to make this change because since I was not using the gradient color scheme and wanted to represent the colors in separate boxes, R needed to know that the values in the **clinton_won** column can actually be grouped into separate color boxes instead of a gradient. 

**NOTE**: The function **as.factor()** would not be needed if I were going to be using a gradient color scheme/key on my map

I then added a separate **geom_polygon()** to make the county outlines. I also had to add a **county** object to use as my reference data when creating the map borders. I wrote **fill=NA** for the **geom_polygon()** to not overlap the data that I have already displayed in the first **geom_polygon()**. I made the outline color grey and made the line size super thin so the outlines do not override the colors that represent the election results. When using the zoom feature in R I noticed that the county outlines did not cause that much problem with visualization. 

I used the **scale_fill_manual()** function to add in my custom color palette **election_palette** and then named the color legend/key that I would be using and specified what each color meant. 

```{r}
election_palette <- c(brewer.pal(n = 11, name = "RdYlBu")[c(2,10)])
county <- map_data("county")

ggplot(data=HandT_election_results) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(clinton_won)))+
  geom_polygon(data=county,
               aes(x=long, y=lat, group=group), fill=NA, 
               col="grey", lwd=0.0001) +
  coord_quickmap() +
  scale_fill_manual(values = election_palette,
                    name="Voting Key",
                    labels=c("Trump Votes",
                             "Clinton Votes")) +
  coord_quickmap() +
  xlab("Longitude ") + 
  ylab("Latitude") +
  ggtitle("2016 Election Results by each County") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 
```

#### Part 2: state-level map of the Hillary and Trump election results

I added a separate **geom_polygon()** to make the state outlines. I also had to add a **states** object to use as my reference data when creating the map borders. I wrote **fill=NA** for the **geom_polygon()** to not overlap the data that I have already displayed in the first **geom_polygon()**. 

I used the **scale_fill_manual()** function to add in my custom color palette **election_palette** and then named the color legend/key that I would be using and specified what each color meant. 

```{r}
states <- map_data("state")

ggplot(data=HandT_election_results) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(clinton_won)))+
  geom_polygon(data=states,
               aes(x=long, y=lat, group=group), fill=NA, 
                col="grey", lwd=0.0001) +
  coord_quickmap() +
  scale_fill_manual(values = election_palette,
                    name="Voting Key",
                    labels=c("Trump Votes",
                             "Clinton Votes")) +
  coord_quickmap() +
  xlab("Longitude ") + 
  ylab("Latitude") +
  ggtitle("2016 Election Results by each State") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 
```

Looking at both maps it can be concluded that Donald Trump won over Hillary Clinton by many states and counties. There is a lot more red appearing than blue. 

## Question 3

For this question I need to display the proportion of votes won by Hillary Clinton which means that I have to use the **clinton_prop** column to fill in the map data. In question 2 I used the column **clinton_won** because I wanted to display the counties where Hillary Clinton won and the counties that Donald Trump won. 

I also got the **scale_fill_gradient2()** function to let R know that the high values in my gradient color bar should represent blue colors and the low values in my gradient color bar should represent red colors. I then said that the middle values in my gradient color bar should be white and I specified that the middle value is 50.  

**NOTE**: I do not use **as.factor()** here because I am using a gradient color scheme in this map because I want to see the change of Hillary Clinton's winning proportion across the United States and do not want to see a select amount of color boxes to display this change. Also, the values in **clinton_prop** are constantly changing which means that a gradient color scheme would be best to represent this type of data.

**NOTE**: In both maps, I added a separate **geom_polygon()** to make the state outlines. I made the outline color **grey** and made the line size **super thin** so the outlines do not override the colors that represent the election results. When using the zoom feature in R I noticed that the county outlines did not cause that much problem with visualization.

```{r}
ggplot(data=HandT_election_results) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=clinton_prop))+
  geom_polygon(data=county,
               aes(x=long, y=lat, group=group), fill=NA, 
               col="grey", lwd=0.0001) +
  scale_fill_gradient2( high = "blue",mid = "white", low = "red", midpoint = 50) +
  coord_quickmap() +
  xlab("Longitude ") + 
  ylab("Latitude") +
  ggtitle("2016 Election Results by each County") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
```

After seeing this map I can conclude that in the Northern and Middle region there were many Trump supporters and less Hillary supporters since there is mostly dark red. The south-west and south-east parts of America had more Hillary supporters since those regions were either blue or white. This was surprising to see since I assumed that the northern areas in America would have more Hillary supporters (more blue and white) and the southern areas would have more Trump supports (red).

### Question 3: Part A

I did not do much here. Just changed the middle color in my gradient color bar to be purple (I did this through the **scale_fill_gradient2()** function).

```{r}
ggplot(data=HandT_election_results) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=clinton_prop))+
  geom_polygon(data=county,
               aes(x=long, y=lat, group=group), fill=NA, 
               col="grey", lwd=0.0001) +
  scale_fill_gradient2( high = "blue",mid = "purple", low = "red", midpoint = 50) +
  coord_quickmap() +
  xlab("Longitude ") + 
  ylab("Latitude") +
  ggtitle("2016 Election Results by each County") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
```

I like the 2nd map with the blue-purple-red combination since I could see the trends more easily as compared to the first map and I believe it has to do with the color combination. Blue and red just go well with purple while white is a bit more abrupt. I believe because of this reason the blue-purple-red combination displays the different shades better as well (this is how I felt what I saw the map). But as mentioned before in both maps I could understand the trends. I believe it has to do with personal preference. 

## Question 4

To answer this question I needed to create a dataset that had the proportion of votes Obama won and the proportion of votes Hillary Clinton won. I then stored this dataset in the object **HandO_election_results**.

**NOTE**: The reason why I am adding the **clinton_prop** column again is because I am using the **fullelection_datset** dataset which does not have the **clinton_prop** column. I added the **clinton_prop** column in the **HandT_election_results** dataset which I am not using here (look at question 1: part B so see where I use this). 

**NOTE**: I did not use the county-level border because I felt it was easier to use the data by using the state-level border. Although I could have seen the map (with the county-level borders) through the zoom feature in R it could not be used in R markdown and the size of the map (in R Markdown) is not as big as the one displayed through the zoom feature. Because of this I decided to add state-level borders in the map instead of county-level borders.

```{r}
HandO_election_results <- fullelection_datset %>%
  mutate(clinton_prop = (clinton/total_votes_2016)*100,
         obama_prop = (obama/total_votes_2012)*100,
         HandO_difference = clinton_prop - obama_prop)

ggplot(data=HandO_election_results) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=HandO_difference))+
    geom_polygon(data=states,
               aes(x=long, y=lat, group=group), fill=NA, 
                col="grey", lwd=0.0001) +
  scale_fill_gradient( high = "blue", low = "red") +
  coord_quickmap() +
  xlab("Longitude ") + 
  ylab("Latitude") +
  ggtitle("Clinton and Obamas Election Results Comparison") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
```

Hillary Clinton did not improve that much as compared to Obama since most of the shades on the map are dark/light red. The northern and eastern part of America has a lot of red states and counties. 

However, there are some purple and blue states in the southern and west part of the map. There is one county in Texas that is bright blue which means Hillary exceeded Obama in terms of election votes (for that specific county in Texas). 

Overall, there are many less countys and states where Hillary Clinton showed improvement in terms of election votes. barak Obama exceeded Hilary Clinton in terms of votes. 

## Bonus Question

#### Part 1: Setting the Working Directory and Join datasets

I just loaded the excel file **va_2020_dem_primary.csv** because the working directory was already established in question 1. The object **virginia_county_dataset** holds the information of the csv file.

Now when I initially joined the **virginia_county_dataset** with the **fullcounty_dataset** I realised that there was one row missing. Because of this I created a object called **virginia_mapping** which added the missing information in the **fullcounty_dataset**. 

After doing this I decided to perform a **left_join()** on the **virginia_county_dataset** and **virginia_mapping** dataset. I created the object **full_virginia_county_dataset** to store the dataset that was created from this join.

```{r}
virginia_county_dataset <-  read_csv('va_2020_dem_primary.csv')

virginia_mapping <- fullcounty_dataset %>%
  filter(region == "virginia") %>%
  mutate(fips = ifelse(polyname=="virginia,accomack",51001,fips)) 

full_virginia_county_dataset <- virginia_county_dataset %>%
  left_join(virginia_mapping, by="fips") 
```

#### Part 2: Calculating the proportions and creating color palettes

I then created a object **BandS_election_results** which stored the winning proportions of Bennet, Steyer, and Williamson. The dataset also included the columns that indicated if Bennet won against Steyer and Williamson (The number 1 in the columns **bennet_won** and **bennet_won2** indicate that Bennet won the Virginia elections. A zero indicates that the opposing party/person won and in this case it is Seyer and Williamson).

I then created 2 separate color palettes to use in my two different maps (one map shows the voting results for Bennet and Steyer and the other map shows the voting results for Bennet and Williamson). **Note** that in both color palettes I keep the color 7 (light blue) and that is because light blue would represent Bennets wins in the map.

```{r}
BandS_election_results <- full_virginia_county_dataset  %>%
  mutate(bennet_prop = (Bennet/ total_votes)*100,
         steyer_prop = (Steyer/total_votes)*100,
         williamson_prop = (Williamson/total_votes)*100,
         bennet_won = ifelse(bennet_prop > steyer_prop ,1,0),
         bennet_won2 = ifelse(bennet_prop > williamson_prop ,1,0))

virginia_palette <- c(brewer.pal(n = 11, name = "RdYlBu")[c(4,7)])
virginia_palette2 <- c(brewer.pal(n = 11, name = "RdYlBu")[c(2,7)])
```

#### Part 3: Map of Bennet vs. Steyer election results

I added the object **virginia_subdivisons** to let R know that I want only counties in Virginia to be displayed in my map. I then used the spatial data stored in this object as a data reference in the second **geom_polygon()** that I use when making the map

**NOTE**: I knew which color represents Bennet because I said in the **ifelse()** function that is Bennet wins then the column value under **bennet_won** should be 1. Now before I added color and labels in the graph I wanted to see what number the labels show by default so I only ran the code till **coord_quickmap()**. Doing that showed me that the second color box represents 1 which told me that when labeling the boxes the second box needs to be called **Bennet Votes**. I also used made sure that the color I chose for Bennet (position 7 in color scheme **RdYlBu**) would always be second in the list/order I specified)

```{r}
virginia_subdivisons <-  filter(county, region %in%
                       c("virginia"))

ggplot(data=BandS_election_results) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(bennet_won)))+
  geom_polygon(data=virginia_subdivisons,
               aes(x=long, y=lat, group=group), fill=NA, 
               col="black", lwd=0.0001) +
  coord_quickmap() +
  scale_fill_manual(values = virginia_palette,
                    name="Voting Key",
                    labels=c("Steyer Votes",
                             "Bennet Votes")) +
  coord_quickmap() +
  xlab("Longitude ") + 
  ylab("Latitude") +
  ggtitle("2020 Virginia Democratic Presidential Results") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 
```

#### Part 4: Map of Bennet vs. Williamson election results

```{r}
# Bennet vs. Williamson
ggplot(data=BandS_election_results) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(bennet_won2)))+
    geom_polygon(data=virginia_subdivisons,
               aes(x=long, y=lat, group=group), fill=NA, 
               col="black", lwd=0.0001) +
  coord_quickmap() +
  scale_fill_manual(values = virginia_palette2,
                    name="Voting Key",
                    labels=c("Williamson Votes",
                             "Bennet Votes")) +
  coord_quickmap() +
  xlab("Longitude ") + 
  ylab("Latitude") +
  ggtitle("2020 Virginia Democratic Presidential Results") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 
```

#### Part 5: Conclusion

My conclusions and findings from the maps are that Bennet lost the vote against Steyer. In the first map there was way more orange than light blue which means that Bennet might be a weak candidate for this election. Bennet got the most votes in the south-east region of Virginia so does that mean he is not that popular in the Northern and Western side of Virginia. Why is this so? Does Bennet hold conflicting beliefs that majoritiy of the people in the west part of virginia do not believe with?

My guess turned out to be inaccurate since the second map showed that Bennet won the vote against Williamson. In the second map there is way more light blue than red which means that Bennet might not be a really weak candidate for this election. Also, Bennet had his votes equally distributed across all parts of Virginia (mainly the west and east). This means that my answer to the previous assumption about Bennet holding conflicting belifes is not true. I guess the reason Bennet did not preform well against Steyer could be because of popularity or reputation. 

Seeing these results has sparked my curiosity to explore the election data further and to see the overall distribution of the data across all the canidates. 



