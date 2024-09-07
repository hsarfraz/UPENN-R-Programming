##############################
## DATA 3
## Merging data
## Stephen Pettigrew
##############################

setwd("d:/Dropbox (PORES)/LPS-data301/Week 4/")



#####################################

## Today we're going to work with county-level election results from the 2016
## election. Our ultimate goal will be to build a dataset where the units of observation
## are counties, and each county has a column with the percent of the vote
## that the Republican candidate received in the presidential, Senate, and governor
## elections.


## We'll first read in the data. I'm setting the 'stringsAsFactors' argument
## because I find it easier to deal with variables as character strings rather
## than factors. But you don't have to do it that way when you read CSVs into R.

pres <- read.csv("president-2016.csv", stringsAsFactors = F)
sen <- read.csv("senate-2016.csv", stringsAsFactors = F)
gov <- read.csv("governor-2016.csv", stringsAsFactors = F)





######################################

## First off, let's look at our data:

head(pres)
head(gov)
head(sen)







## Notice that the senate and governor files have almost identical formatting.
## Also notice that the number of rows in each dataset are different. Why is
## this the case?

nrow(pres)
nrow(sen)
nrow(gov)
























## Because the presidential election was conducted in every county in the country
## (unlike senate or governor races), we'll start with that file.



########################

## Our ultimate goal is to have a dataset with the Republican vote percentage
## in each county. Let's begin by creating that within the 'pres' dataframe.

## We could wait to create the variable until after we've merged everything together.

head(pres)

pres$rep.pct <- pres$trump / pres$totalvotes


## That variable is the overall percentage that Trump received in the election.
## Let's also create a variable that is the percentage of votes Trump received
## when we only consider his and Clinton's votes. This is often refered to as the 
## 'two-party' vote and is often what we use when we analyze election returns


pres$rep.pct.two.party <- pres$trump / (pres$clinton + pres$trump)

## we could also do this with the rowSums() function, which can be useful
## if you have more than two columns you want to add together:

pres$rep.pct.two.party <- pres$trump / rowSums(pres[,c("clinton","trump")])


## Now let's get rid of some of the columns from the dataset, since
## we won't want those in the final, merged data.

pres$clinton <- pres$trump <- pres$johnson <- pres$stein <- pres$other <- NULL

head(pres)



##########################


## Now let's do the same with the senate and governor election results. We'll
## create columns with the Republican vote percentage and then drop columns
## we don't want:

sen$rep.pct <- sen$votes.rep / sen$totalvotes
sen$rep.pct.two.party <- sen$votes.rep / rowSums(sen[,c("votes.dem","votes.rep")])
sen$votes.dem <- sen$votes.rep <- NULL
head(sen)



gov$rep.pct <- gov$votes.rep / gov$totalvotes
gov$rep.pct.two.party <- gov$votes.rep / rowSums(gov[,c("votes.dem","votes.rep")])
gov$votes.dem <- gov$votes.rep <- NULL
head(gov)


#############################

## It's worth noting that the two chunks of code above were a case where
## we performed the exact same lines of code on two different datasets. This is
## an instance where we might want to write our own function to perform this
## task. With three or four lines of code it's not hard to copy-paste, but if
## we had dozens of lines or code, or wanted to replicate it on more than two
## datasets then we'd probably want to write a function.
## We'll talk about writing our own functions in a couple weeks











#########
## Merging data
#########

## Remember from the video about the principles of data merging, that we need
## to ask ourselves two questions when merging data:

## What variable will we use to pair up observations in each dataset?
## Which rows do we want to keep and which do we want to drop?



## Let's look at our data again to figure out what we could merge on:

View(pres)
View(gov)

## Take a minute to think about what variable we might want to use to perform this merge...


























## We need a variable (or combination of variables) that would uniquely
## identify each unique observation in our dataset. The clear candidate
## seems to be 'jurisdiction'.

## The problem is that there are 21 counties called 'Washington'
## and 20 counties called 'Jefferson' in the US, so the 'jurisdiction'
## variable doesn't uniquely distinguish between each county.

head(sort(table(pres$jurisdiction), decreasing = T))



##############################



## A better option would be to use the 'fipscode' variable. Fipscodes
## are numeric codes that are assigned to each state, county, city, etc.
## in the country. 

## Let's check to see if every row has a unique fipscode

length(unique(sen$fipscode)) == nrow(sen) ## yes
length(unique(gov$fipscode)) == nrow(gov) ## yes
length(unique(pres$fipscode)) == nrow(pres) ## no





## The presidential results apparently have something weird with the
## fipscode variable. It looks like the data from DC don't have
## fipscodes assigned to them:

pres[is.na(pres$fipscode),]
## note: the is.na() function is insanely useful for exploring missing data

## So if we want to use the fipscode variable to do the merge, and we don't
## fix those NA's, then the data for DC won't merge the way we want it to.




#########################

## In this case though, our best option would be to not use the fipscode variable
## at all. Instead, we can merge on two variables (state and jurisdiction).

nrow(unique(sen[,c("jurisdiction", "state")])) == nrow(sen) ## yes
nrow(unique(gov[,c("jurisdiction", "state")])) == nrow(gov) ## yes
nrow(unique(pres[,c("jurisdiction", "state")])) == nrow(pres) ## yes






## So we've figured out the variable(s) we're going to use to perform the
## merge. Now we need to decide what type of merge we want to do.



## Let's look at how all the different types of merges work in practice,
## using the gov and sen datasets:

## Inner join. Only keep full pairs:

inner <- merge(x = sen, 
               y = gov, 
               by = c("state","jurisdiction"))




## Outer join. Keep all rows, regardless if they match or not:

outer <- merge(x = sen, 
               y = gov, 
               by = c("state","jurisdiction"),
               all = T)



## Left join. Keep everything in 'sen':

left <- merge(x = sen, 
              y = gov, 
              by = c("state","jurisdiction"),
              all.x = T)




## right join. Keep everything in 'gov':

right <- merge(x = sen, 
               y = gov, 
               by = c("state","jurisdiction"),
               all.y = T)




#################

## Notice that they've all got a different number of rows:

## Original data had 825 rows in 'gov' and 2329 rows in 'sen'

nrow(inner) # 711
nrow(outer) # 2443
nrow(right) # 825
nrow(left) # 2329


## Also, notice that in 'right' the rows that appeared in gov, but not in sen,
## have NAs substituted into the sen variables. Something similar happened in
## 'right' and 'outer'

View(right)



## So if we're trying to merge together everything in pres, sen, and gov
## what type of merge would we probably want to use?


























## We want to do two outer merges, so that we don't lose any of our data.



#####

## Let's give it a shot. We'll use the suffixes argument to specify
## how we want to rename columns that have the same name in both
## of our datasets:

elections <- merge(gov, sen,
                   by = c("state","jurisdiction"),
                   all = T,
                   suffixes  = c(".gov",".sen"))

## Now we'll merge in the presidential data:

elections <- merge(elections, pres,
                   by = c("state","jurisdiction"),
                   all = T,
                   suffixes  = c("",".pres"))


View(elections) 
## notice that our columns names still need a bit of work, so that would be
## our next step in the cleaning process



####

## It's generally a good idea to play around with your data after you've done the
## merge. We know that the vote percentages for the three offices should
## be relatively similar within each county, so we can make a simple graph
## to double check that:

plot(elections$rep.pct.two.party, # presidential vote
     elections$rep.pct.two.party.sen) # senate vote

plot(elections$rep.pct.two.party.gov, # governor vote
     elections$rep.pct.two.party.sen) # senate vote

plot(elections$rep.pct.two.party.gov, # governor vote
     elections$rep.pct.two.party) # presidential vote





