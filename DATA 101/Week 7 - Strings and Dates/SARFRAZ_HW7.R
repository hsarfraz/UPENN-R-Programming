install.packages('babynames')
library(babynames)
library(tidyverse)
library(htmlwidgets)
library(stringr)

view(babynames)

#1. filtered babynames of kids born in 2017 
babynames2017 <- babynames %>%
  filter(year==2017) %>%
  mutate(name_lowercase = str_to_lower(name))


#   a.	Which vowel is the most common in the first letter of names from 2017?
babynames2017 %>%
  mutate(
    a_vowel = str_count(name_lowercase,"^[a]"),
    e_vowel = str_count(name_lowercase,"^[e]"),
    i_vowel = str_count(name_lowercase,"^[i]"),
    o_vowel = str_count(name_lowercase,"^[o]"),
    u_vowel = str_count(name_lowercase,"^[u]")) %>%
  transmute(
    total_a_vowel = sum(a_vowel),
    total_e_vowel = sum(e_vowel),
    total_i_vowel = sum(i_vowel),
    total_o_vowel = sum(o_vowel),
    total_u_vowel = sum(u_vowel)) %>%
  head(1) 

# b.	Do more people name their children names that are longer than 5 letters or shorter
#     than 5 letters?
babynames2017 %>%
  mutate(
    short5letter = ifelse((str_length(name_lowercase) < 5),1,0),
    long5letter = ifelse((str_length(name_lowercase) > 5),1,0)) %>%
  transmute(
    total_short5letter = sum(short5letter),
    total_long5letter = sum(long5letter),
    ) %>%
  head(1) 


# c.	What are the names that have at least 14 letters? Don't use str_length().
babynames2017 %>%
  filter(
    str_detect(name, "..............")
  ) 

# d.	What names have more than two vowels in a row? 
#     Are there any names with more than three vowels in a row?

#2 or more vowels
str_subset(babynames2017$name_lowercase,"[aeiou]{2,}") %>%
  as_tibble() %>%
  view()

#3 or more vowels
str_subset(babynames2017$name_lowercase,"[aeiou]{3,}") %>%
  as_tibble() %>%
  view()

# e.	What names contain the longest sequential string of consonants?
str_view(babynames2017$name_lowercase,"[^aeiou]{2,}", match=T)
str_view(babynames2017$name_lowercase,"[^aeiou]{3,}", match=T)
str_view(babynames2017$name_lowercase,"[^aeiou]{4,}", match=T)
str_view(babynames2017$name_lowercase,"[^aeiou]{5,}", match=T)
str_view(babynames2017$name_lowercase,"[^aeiou]{6,}", match=T)
str_view(babynames2017$name_lowercase,"[^aeiou]{7,}", match=T)
str_view(babynames2017$name_lowercase,"[^aeiou]{8,}", match=T)


#2.	In the most recent year recorded in the dataset (2017), what are the 10 most popular names that start with Z? What about the letter Q?

#z names
babynames2017 %>%
  mutate(z_names = str_count(name_lowercase,"^[z]"),
         percentage = prop*100) %>%
  filter(z_names == 1) %>%
  arrange(desc(percentage)) %>%
  head(10) 

#q names
babynames2017 %>%
  mutate(q_names = str_count(name_lowercase,"^[q]"),
         percentage = prop*100) %>%
  filter(q_names == 1) %>%
  arrange(desc(percentage)) %>%
  head(10) 

#3.	What names from 2017 have the highest number of vowels? 
#   How many names end in vowels?

babynames2017 %>%
  mutate(long_vowel = str_count(name_lowercase,"[aeiou]")) %>%
  arrange(desc(long_vowel)) %>%
  head(1) 

#name ending in vowels
babynames2017 %>%
  mutate(end_vowel = str_count(name_lowercase,"[aeiou]$")) %>%
  transmute(total_end_vowel = sum(end_vowel)) %>%
  head(1) 

#4.	The name "Mary" was the most popular girls name from 1880-1946. 
#   Track the popularity of the name overtime using a line graph. 
#   Hint: filter for only female instances of Mary for a clearer graph. What do you notice?

babynames %>%
  mutate(name_lowercase = str_to_lower(name),
         percentage = prop*100) %>%
  filter(name_lowercase == 'mary', sex == 'F') %>%
  ggplot() +
  geom_line( mapping = aes(x=year,y=percentage)) +
  xlab("Years") + 
  ylab("Percentage") +
  scale_x_continuous(breaks=c(1880,1900,1920,1940,1960,1980,2000,2017)) + 
  ggtitle("Yearly Percentage of Girls named Mary") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 10)) 

#   a.	What is the most popular female name in 2017?
#       Track that name's popularity over time.

babynames2017 %>%
  mutate(percentage = prop*100) %>%
  filter(sex == 'F') %>%
  arrange(desc(percentage))

babynames %>%
  mutate(name_lowercase = str_to_lower(name),
         percentage = prop*100) %>%
  filter(name_lowercase == 'emma', sex == 'F') %>%
  ggplot() +
  geom_line( mapping = aes(x=year,y=percentage)) +
  xlab("Years") + 
  ylab("Percentage") +
  scale_x_continuous(breaks=c(1880,1900,1920,1940,1960,1980,2000,2017)) + 
  ggtitle("Yearly Percentage of Girls named Emma") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 10)) 


# 5.	Peyton is a name that is used for both girls and boys.
#     Plot the popularity of the name by gender. What do you notice?

babynames %>%
  mutate(name_lowercase = str_to_lower(name)) %>%
  filter(name_lowercase == 'peyton') %>%
  ggplot() +
  geom_line( mapping = aes(x=year,y=prop, color=sex)) +
  xlab("Years") + 
  ylab("Percentage") +
  scale_x_continuous(breaks=c(1880,1900,1920,1940,1960,1980,2000,2017)) + 
  ggtitle("Yearly Percentage of Girls named Peyton") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 10)) +
  scale_color_manual(values = c('red', 'blue'),
    name="Line Legend", 
                       labels=c(
                         "F" = 'Girls',
                         "M" = 'Male'))


