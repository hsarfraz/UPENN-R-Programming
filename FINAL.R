library(readr)
library(tidyverse)

#1. First, import this data using this link. Download the spreadsheet as a csv 
#   and load the election results into R. 
setwd("C:/Users/hussainsarfraz/Desktop/DATA 101")
senate_races <-  read_csv('1976-2020-senate.csv')

#2. Our first step to clean this data is removing non-substantive columns. 
#   Keep only the variables
#   year, state, state_po, stage, candidate, party_detailed, candidatevotes, totalvotes. 
view(senate_races)

updated_senate_races <- senate_races %>%
  transmute(year = year,
            state = state,
            state_po = state_po,
            stage = stage,
            candidate = candidate,
            party_detailed = party_detailed,
            candidatevotes = candidatevotes,
            totalvotes = totalvotes)

view(updated_senate_races)
#3. Next, we're going to remove rows with some incomplete data.
#   Remove any rows that have missing data in the "candidate" or "party" columns. 

filtered_updated_senate_races <- updated_senate_races %>%
  filter(!is.na(candidate))

view(filtered_updated_senate_races)

######  checking if the filter command worked
filtered_updated_senate_races %>%
  filter(is.na(candidate))

#4. Next, create a new variable, "party3" which recodes the "party" column 
#   into "D" for Democrats, "R" for Republicans, and "I" for all other parties.
#   (Hint: you may want to first create this column so that all rows equal "I".
#    Then use the ifelse() function to recode to "R" if the row represents a Republican
#   and otherwise stays equal to its current value.) 

categorized_senate_dataset <- filtered_updated_senate_races %>%
  mutate( 
    party3 = ifelse((party_detailed == 'REPUBLICAN'),'R','I'),
    party3 = ifelse((party_detailed == 'DEMOCRAT'),'D',party3)
  ) 

#5. How many Democrats are in this dataset?
#   How many Republicans? How many Independents?

categorized_senate_dataset %>%
  count(party3)


###   This is to CHECK SUMS -NOT ANSWER
filtered_updated_senate_races %>%
  mutate( 
    partyR = ifelse((party_detailed == 'REPUBLICAN'),1,0),
    partyD = ifelse((party_detailed == 'DEMOCRAT'),1,0),
    partyI = ifelse((party_detailed != 'DEMOCRAT'&party_detailed != 'REPUBLICAN'),1,0)
  ) %>%
  transmute(totalR = sum(partyR, na.rm=T),
            totalD = sum(partyD,na.rm=T),
            totalI = sum(partyI,na.rm=T)) %>%
  view()


#6. Now let's look at the 2-party vote in these data.
#   First, remove the independent candidates from the data.
#   Next, remove all the rows where "stage" is not equal to "gen".
#   This ensures that we only get results from the general election.

two_party_vote <- categorized_senate_dataset %>%
  filter(party3 != 'I',
         stage == 'gen') 
view(two_party_vote)
#7. How many races were contested between more than two candidates?
#   Which state had the most of these races? 

# 23 races because of 23 rows
senate_races <- two_party_vote %>%
  group_by(year,state) %>%
  summarize(candidate_count = n()) %>%
  filter(candidate_count>2)

senate_races %>%
  group_by(state) %>%
  summarize(race_count = n()) %>%
  arrange(desc(race_count)) %>%
  head(1)


#this WORKS (I am counting canidates but the column name is n which is why I used the code above)
two_party_vote %>%
  group_by(year,state) %>%
  count() %>%
  view()

#8 For Democratic and Republican candidates create a figure that displays year on the
#  x axis and each candidate's percent of the vote on the y-axis.
#  Be sure to color code each candidate by their respective party.
#  Add two lines - one for each party - that represents the trend in that parties'
#  support overtime. 

updated_two_party_vote <-two_party_vote %>%
  mutate(candidate_vote_percentage = (candidatevotes/totalvotes)*100)

R_avetrends <- updated_two_party_vote %>%
  filter(party3 == 'R') %>%
  group_by(year) %>%
  summarize(total_candidatevotes = sum(candidatevotes),
            total_oftotalvotes = sum(totalvotes),
            overall_total_percentage=(total_candidatevotes/total_oftotalvotes)*100) 

D_avetrends <- updated_two_party_vote %>%
  filter(party3 == 'D') %>%
  group_by(year) %>%
  summarize(total_candidatevotes = sum(candidatevotes),
            total_oftotalvotes = sum(totalvotes),
            overall_total_percentage=(total_candidatevotes/total_oftotalvotes)*100) 

view(updated_two_party_vote)

updated_two_party_vote %>%
  ggplot() +
  geom_point( mapping = aes(x=year,y=candidate_vote_percentage, color=party3)) +
  geom_line(R_avetrends, mapping = aes(x=year,y=overall_total_percentage,color='red_L')) +
  geom_line(D_avetrends, mapping = aes(x=year,y=overall_total_percentage,color='blue_L')) +
  xlab("Years") + 
  ylab("Candidate Vote Percentage") +
  scale_x_continuous(breaks=c(1975,1980,1985,1990,1995,2000,2005,2010,2015,2020)) + 
  ggtitle("Yearly Voting Percentage of Senators") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 10)) +
  scale_color_manual(values = c('blue','#9ae9fd', '#fd9a9a','red'),
                     name="Legend", 
                     labels=c(
                       'blue_L' = 'Democrats Average',
                       "D" = 'Democrats',
                       "R" = 'Republican',
                       'red_L' = 'Republican Average'))

#9. Let's take a look at the races from 2012. 
#   Filter your dataset so that it only contains the results for 2012, 
#   and only the columns year, state, party3, and the candidate percent you calculated
#   in the previous question. Reshape this data so that there is only one row per state,
#   and two columns that represent the percent of the vote won by the Republican 
#   candidate and the percent of the vote won by the Democratic candidate. 
#   Note that you will not have 50 rows because not all states have a Senate election in
#   an election year. 

senate_votes_2012 <- updated_two_party_vote %>%
  filter(year == 2012) %>%
  select(year, state, party3, candidate_vote_percentage) %>%
  pivot_wider(names_from = party3, values_from = candidate_vote_percentage)

#10. Create a variable "demwin" that records if the Democrat received a higher vote
#    share than the Republican in each race in 2012. 
view(senate_votes_2012)

senate_votes_2012 %>%
  mutate(demwin = ifelse((D>R),1,0))


#11. Create a variable "demdiff" that records the difference between the Democratic and
#    Republican share of the vote in each race in 2012. 

updated_senate_votes_2012 <- senate_votes_2012 %>%
  mutate(demwin = ifelse((D>R),1,0),
         demdiff = D-R)

#12. Next, we're going to do some analysis to map this data. Load in the state-level
#    mapping data that we've worked with from the package mapdata 

library(maps)
library(mapdata)

view(updated_senate_votes_2012)

states <- map_data("state") %>%
  mutate(upper_state = str_to_upper(region)) %>%
  select(-subregion,-region)


#13. Join the 2012 Senate election data to this mapping data.
#    Be cautious about the format of the state names! 
view(states)
view(updated_senate_votes_2012)

state_and_senate_dataset <- updated_senate_votes_2012 %>%
  left_join(states, by=c("state"="upper_state"))

view(state_and_senate_dataset)


#14. Create a map that shows the winner of each Senate contest in 2012,
#    with Democrats in blue and Republicans in red. If there was no Senate contest
#    in a state (or if a party other than Democrats or Republicans won the seat),
#    leave the state blank. 

election_palette <- c(brewer.pal(n = 11, name = "RdYlBu")[c(2,10)])

ggplot(data=state_and_senate_dataset) +
  geom_polygon(data=states,
               aes(x=long, y=lat, group=group), fill='#7f7f7f') +
  geom_polygon(aes(x=long, y=lat, group=group, fill=as.factor(demwin)))+
  geom_polygon(data=states,
               aes(x=long, y=lat, group=group), fill=NA, 
               col="grey", lwd=0.0001) +
  coord_quickmap() +
  scale_fill_manual(values = election_palette,
                    name="Voting Key",
                    labels=c("Republican Win",
                             "Democrat Win",
                             "No Elections/Other Party Win")) +
  xlab("Longitude ") + 
  ylab("Latitude") +
  ggtitle("2012 Senate Election Results for Democrats and Republicans") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 


#15. Create a map that shades each state by the Democratic vote difference you created
#    above. Again, If there was no Senate contest in a state (or if a party other than
#    Democrats or Republicans won the seat), leave the state blank. 

ggplot(data=state_and_senate_dataset) +
  geom_polygon(data=states,
               aes(x=long, y=lat, group=group), fill='#7f7f7f') +
  geom_polygon(aes(x=long, y=lat, group=group, fill=demdiff))+
  geom_polygon(data=states,
               aes(x=long, y=lat, group=group), fill=NA, 
               col="grey", lwd=0.0001) +
  scale_fill_gradient2( name = "Winning Scale",
                        high = "blue",mid = "purple", low = "red", midpoint = -5.96933) +
  coord_quickmap() +
  xlab("Longitude ") + 
  ylab("Latitude") +
  ggtitle("2012 Senate Election Results for Democrats and Republicans") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 