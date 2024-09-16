#-------------------------------------------
# SQL HW
#-------------------------------------------

# We will use occupational employment statistics data
# The file is somewhat large (N>430,000)

# For details, see http://www.bls.gov/oes/current/oes_stru.htm

library(sqldf)
library(ggplot2)
library(dplyr)
library(tidyverse)
setwd("C:/Users/hussainsarfraz/Desktop/DATA 410/Week 7 - 410")

# Looking at first few rows of dataset 
read.table("oesm.csv", sep=";", header = TRUE, fill = TRUE, nrows = 5)

# 1. Begin SQL processing by creating a new database called 'conemp'
# Then add a table called 'oesm' (A best practice here is to use an
# if statement to delete a table called 'oesm' if one already exists.)
{
  reqphl <- dbConnect(SQLite(), dbname = "conemp.db")
  
  if (dbExistsTable(reqphl, "oesm"))
    dbRemoveTable(reqphl, "oesm")
  
  dbWriteTable(
    reqphl,
    "oesm",
    "oesm.csv",
    sep = ";",
    header = TRUE,
    row.names = FALSE
  ) 
  dbDisconnect(reqphl)
}

# 2. Display the first 10 rows of all of the data in your table.
reqphl.two <- dbConnect(SQLite(), dbname = "conemp.db")
res <- dbSendQuery(reqphl.two, "
                   SELECT *
                   FROM oesm")
fetch(res, n = 10)
dbClearResult(res)

# 3. Select a subset of columns, 'occtitle', 'h_mean', 'area_title' for 
# all of the observations where the grouping variable is 'major.'
res <- dbSendQuery(reqphl.two,
                   "
                   SELECT occtitle , h_mean , area_title
                   FROM oesm
                   WHERE (grouping=='major')")
fetch(res, n = 10)
dbClearResult(res)


# 4. Display all of the unique values of the variable 'naics'
res <- dbSendQuery(reqphl.two, "
                   SELECT DISTINCT naics
                   FROM oesm")
fetch(res, n = -1)
dbClearResult(res)

res <- dbSendQuery(reqphl.two, "
                   SELECT count(DISTINCT naics)
                   FROM oesm")
fetch(res, n = -1)
dbClearResult(res)

# 5. Count all of the observations for each year in the dataset.
yearcount <- dbSendQuery(reqphl.two, "
                     SELECT year,COUNT(*)
                     FROM oesm
                     GROUP BY year")

fetch(yearcount, n = 10)
dbClearResult(yearcount)


# 6. Find minimum and maximum of median annual salary (a_mean) for
# each year
salary.min.max <- dbSendQuery(reqphl.two, "
                     SELECT year, MIN(a_mean), MAX(CAST(a_mean as INT))
                     FROM oesm
                     WHERE (a_mean IS NOT 'NA')
                     GROUP BY year
                                 ")

fetch(salary.min.max, n = -1)
dbClearResult(salary.min.max)

# 7. Create a new table from the same database that includes
# the unique values of occcode and occtitle  

#removing the table before doing the code as best practice
{
if (dbExistsTable(reqphl.two, "tabletwo"))
  dbRemoveTable(reqphl.two, "tabletwo")
}

#creating a new table in the 'reqphl.two' database
dbSendQuery(reqphl.two, "
                   CREATE TABLE tabletwo AS
                   SELECT DISTINCT occcode, occtitle
                   FROM oesm
                   ")

#displaying the table
newcopy <- dbSendQuery(reqphl.two, "
                   SELECT *
                   FROM tabletwo
                   ")

fetch(newcopy, n = 10)
dbClearResult(newcopy)

#Getting the count of the newly created table
newcopy <- dbSendQuery(reqphl.two, "
                   SELECT COUNT(*)
                   FROM tabletwo
                   ")

fetch(newcopy, n = -1)
dbClearResult(newcopy)

# 8. Using your new SQL skills, pull some information from either table
# and create a visualization (of any kind) with your extracted data. In 
# one or two sentences, briefly comment on your visual. 
options(scipen=999)
median.salary <- dbSendQuery(reqphl.two, "
                   SELECT year, occtitle , sum(a_mean)
                   FROM oesm
                   WHERE (a_mean IS NOT 'NA')
                   GROUP BY year , occtitle
                                 ")

median.salaryDB <- as.data.frame(fetch(median.salary, n = -1))

top.ten.2015 <- median.salaryDB %>%
  filter(year == 2015) %>%
  arrange(desc(`sum(a_mean)`)) %>%
  transmute(top.ten.careers = occtitle) %>%
  head(10)

median.salaryDB %>%
  filter(occtitle == top.ten.2015[1,]|
         occtitle == top.ten.2015[2,]|
         occtitle == top.ten.2015[3,]|
         occtitle == top.ten.2015[4,]|
         occtitle == top.ten.2015[5,]|
         occtitle == top.ten.2015[6,]|
         occtitle == top.ten.2015[7,]|
         occtitle == top.ten.2015[8,]|
         occtitle == top.ten.2015[9,]|
         occtitle == top.ten.2015[10,]
         ) %>%
  mutate(`sum(a_mean)` = `sum(a_mean)`/1000000) %>%
  pivot_wider(names_from = occtitle, values_from = `sum(a_mean)`) %>%
    tidyr::gather("id", "value", 2:11) %>% 
  ggplot(., aes(year, value))+
  geom_point()+
  xlab("Year") +
  ylab("Median Annual Salary (millions)") +
  ggtitle("Top 10 jobs that have the highest median salaries") +
  scale_x_continuous(breaks=c(2014,2015)) +
  geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(~id) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 10,hjust = 0.5),
        strip.text = element_text(size=8)
        ) 

top.less.2015 <- median.salaryDB %>%
  filter(year == 2015) %>%
  arrange(`sum(a_mean)`) %>%
  transmute(top.ten.careers = occtitle) %>%
  head(10)

median.salaryDB %>%
  filter(occtitle == top.less.2015[1,]|
           occtitle == top.less.2015[2,]|
           occtitle == top.less.2015[3,]|
           occtitle == top.less.2015[4,]|
           occtitle == top.less.2015[5,]|
           occtitle == top.less.2015[6,]|
           occtitle == top.less.2015[7,]|
           occtitle == top.less.2015[8,]|
           occtitle == top.less.2015[9,]|
           occtitle == top.less.2015[10,]
  ) %>%
  mutate(`sum(a_mean)` = `sum(a_mean)`/100000) %>%
  pivot_wider(names_from = occtitle, values_from = `sum(a_mean)`) %>%
  tidyr::gather("id", "value", 2:11) %>% 
  ggplot(., aes(year, value))+
  geom_point()+
  xlab("Year") +
  ylab("Median Annual Salary (100,000's)") +
  ggtitle("Top 10 jobs that have the lowest median salaries") +
  scale_x_continuous(breaks=c(2014,2015)) +
  geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(~id) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 10,hjust = 0.5),
        strip.text = element_text(size=8)) 