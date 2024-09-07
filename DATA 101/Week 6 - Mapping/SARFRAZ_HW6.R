install.packages("RColorBrewer")
library(RColorBrewer)

library(maps)
library(mapdata)
library(tidyverse)
library(scales)

# Question 1

install.packages("readr")
library(readr)
setwd("C:/Users/hussainsarfraz/Desktop/DATA 101/Week 6 - Mapping")
election_dataset <-  read_csv('county_returns.csv')
view(election_dataset)
# Part A

counties <- map_data("county")
view(counties)

election_dataset %>%
  mutate(lowercase_countyname = str_to_lower(Geographic.Name)) %>%
  left_join(counties, by=c('lowercase_countyname'="subregion"))

county_fips <- county.fips 
view(county_fips)

fullcounty_dataset <- counties %>%
  mutate(polyname = paste(region,subregion,sep=",")) %>%
  left_join(county_fips, by="polyname") 

fullelection_datset <- election_dataset %>%
  left_join(fullcounty_dataset, by="fips")


# Part B

HandT_election_results <- fullelection_datset %>%
  mutate(clinton_prop = (clinton/total_votes_2016)*100,
         trump_prop = (trump/total_votes_2016)*100,
         clinton_won = ifelse(clinton_prop > trump_prop ,1,0),
         trump_won = ifelse(trump_prop >clinton_prop,1,0))

# Question 2

election_palette <- c(brewer.pal(n = 11, name = "RdYlBu")[c(2,10)])

# Hilary and Trump Results for county level

ggplot(data=HandT_election_results) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(clinton_won)))+
  geom_polygon(data=counties,
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

# Hilary and Trump Results for state level

states <- map_data("state") %>%
  mutate(lower_state_name = str_to_lower(region)) %>%
  select(-region)

view(HandT_election_results)

HandT_election_results %>%
  group_by(state.name) %>%
  summarize(clinton_total_prop = sum(clinton_won),
            trump_total_prop = sum(trump_won)) %>%
  mutate(clinton_state_win = ifelse((clinton_total_prop>trump_total_prop),1,0)) %>% 
  left_join(states,by=c('state.name'='lower_state_name')) %>%
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(clinton_state_win)))+
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


# Question 3

?scale_fill_gradient2 

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


# Question 3 Part A

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


# Question 4

HandO_election_results <- fullelection_datset %>%
  mutate(clinton_prop = (clinton/total_votes_2016)*100,
         obama_prop = (obama/total_votes_2012)*100,
         HandO_difference = clinton_prop - obama_prop)

view(fullelection_datset)
view(HandO_election_results)

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

# Bonus Question

# Part 1: Load County Dataset
virginia_county_dataset <-  read_csv('va_2020_dem_primary.csv')
view(virginia_county_dataset)

virginia_mapping <- fullcounty_dataset %>%
  filter(region == "virginia") %>%
  mutate(fips = ifelse(polyname=="virginia,accomack",51001,fips)) 

full_virginia_county_dataset <- virginia_county_dataset %>%
  left_join(virginia_mapping, by="fips") 



BandS_election_results <- full_virginia_county_dataset  %>%
  mutate(bennet_prop = (Bennet/ total_votes)*100,
         steyer_prop = (Steyer/total_votes)*100,
         williamson_prop = (Williamson/total_votes)*100,
         bennet_won = ifelse(bennet_prop > steyer_prop ,1,0),
         bennet_won2 = ifelse(bennet_prop > williamson_prop ,1,0))

virginia_palette <- c(brewer.pal(n = 11, name = "RdYlBu")[c(4,7)])
virginia_palette2 <- c(brewer.pal(n = 11, name = "RdYlBu")[c(2,7)])

virginia_subdivisons <- filter(states, region %in%
                                 c("california","oregon","washington"))

# Bennet vs. Steyer
ggplot(data=BandS_election_results) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(bennet_won)))+
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

# Bennet vs. Williamson
ggplot(data=BandS_election_results) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(bennet_won2)))+
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

west_coast <- filter(county, region %in%
                       c("virginia"))
virginia_subdivisons <-  map_data("state") %>%
  filter(c(virginia))
