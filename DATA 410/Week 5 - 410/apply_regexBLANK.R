#-------------------------
# Apply Regex
#-------------------------

#Let's use regular expressions in a dataset

#Import Clery crime log data from UOregon
# and examine the variables
setwd("C:/Users/hussainsarfraz/Desktop/DATA 410/Week 5 - 410")
df <- read.csv("clerylog.csv", header = TRUE, sep=",")
View(df)

########Problem 1:Clean up classifications in the nature variable
########          Remove the level of offenses (ie. the numbers 1, 2)

##looking at all the offences that have a number
grep('[0-9]',df$Nature, value = TRUE)

##all the cases of Marijuana
grep('1 oz',df$Nature, value = TRUE)
df$Nature <- gsub("1 oz","one oz", df$Nature)
grep('one',df$Nature, value = TRUE) #checking if replace was successful

##removing all offence levels now
df$Nature <- gsub("[0-9]","", df$Nature)
df$Nature


########Problem 2: 
######## A. Create a dummy variable that indicates if each row is a theft or not. 
df$Nature
##lists all theft-related crimes
grep('theft',df$Nature, value = TRUE, ignore.case = TRUE)
#grep('\\btheft\\b',df$Nature, value = TRUE, ignore.case = TRUE)

##creating dummy variable
df$theft.dummy<- grepl('theft',df$Nature, ignore.case = TRUE)
df$theft.dummy

######## B. Create a dummy variable that indicates if each row is a DUI
##lists all DUI's
grep('dui',df$Nature, value = TRUE, ignore.case = TRUE)

##creating dummy variable
df$DUI.dummy<- grepl('dui',df$Nature, ignore.case = TRUE)
df$DUI.dummy

######## C. Create a dummy variable that indicates if each row is a drug offense
##lists all drug-related crimes
grep('drug|marijuana|heroin|meth|cocaine',df$Nature, value = TRUE, ignore.case = TRUE)

##creating dummy variable
df$drug.dummy<- grepl('drug|marijuana|heroin|meth|cocaine',df$Nature, ignore.case = TRUE)
df$drug.dummy

########Problem  3: Use gregexpr() to  find observations that are associated
########            with either heroin, marijuana, or meth

drug.matches <- gregexpr('drug|marijuana|heroin|meth|cocaine',df$Nature, ignore.case = TRUE)
df$three.drug <- regmatches(df$Nature, drug.matches)

df$threecheck.drug <- ifelse((df$three.drug != 'character(0)'),TRUE,FALSE)


########Problem 4: Create a new column with JUST the date (or date range) the crime occurred
df$date.occured.only <- df$Date.Time.Occurred

#removing time
grep(' [0-9]{4}',df$date.occured.only, value = TRUE)
df$date.occured.only <- gsub(' [0-9]{4}','',df$date.occured.only)

grep(' [0-9]{2}:[0-9]{2}| [0-9]{1}:[0-9]{2}',df$date.occured.only, value = TRUE)
df$date.occured.only <- gsub(' [0-9]{2}:[0-9]{2}| [0-9]{1}:[0-9]{2}','',df$date.occured.only)

#removing dash
grep('-',df$date.occured.only, value = TRUE)

#dates with a range
grep(' - ',df$date.occured.only, value = TRUE)
df$date.occured.only <- gsub(' - ','@',df$date.occured.only)

#dates without a range
grep(' -',df$date.occured.only, value = TRUE)
df$date.occured.only <- gsub(' -','',df$date.occured.only)

#adding a '-' to dates with a range
df$date.occured.only <- gsub('@','-',df$date.occured.only)
df$date.occured.only <- gsub('- ','-',df$date.occured.only)

#checking and removing 'report pending' status
grep('[a-z]',df$date.occured.only, value = TRUE,ignore.case = TRUE)
df$date.occured.only <- gsub('[a-z]',NA,df$date.occured.only,ignore.case = TRUE)

#now 2014 is a year but no specifc date was given. I left it in since the year is a part of the date
#here would have been the code if I needed to remove it
# grep('[0-9]{3}',df$date.occured.only, value = TRUE,ignore.case = TRUE)
# table(nchar(df$date.occured.only) == 4)
# 
# date.character.count <- nchar(df$date.occured.only) == 4
# df$date.occured.only <- ifelse((date.character.count == TRUE),NA,df$date.occured.only)
