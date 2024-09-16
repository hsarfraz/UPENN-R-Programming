#-----------------------------------#
# Regular Expressions
#-----------------------------------#
# Welcome back! In today’s lecture we’re going to learn about 
# Regular Expressions. A regular expression is a set of characters 
# that make up a “search pattern.” Basically, they’re a group of
# characters or symbols that you can use to search your data for 
# a matching sequence within your data. 
# Regular expressions are commonly used as the tool behind a search
# engine – a user inputs some information into a search bar, like 
# in our Presidents App data table search bar, we could enter in 
# the sequence J E F F – and the table returns the information 
# associated with “THOMAS JEFFERSON”
# Regular Expressions are also super useful in cleaning data. 
# Up until this point, when I or other professors have talked 
# about cleaning data – they’ve been referring to making 
# variable names more useful and restructuring the dataset 
# to be ready for your analysis. But regular expressions 
# assist us with another layer. 
# 
# Data that is not prepared by a well-funded survey, or 
# from someone who works with data fairly often, is 
# going to have some issues for analysis based on how the
# data was entered. For example, before you started 
# working with R, would you have recognized how important
# a  difference between writing a variable as “High School”
# with a capital H and S,  and “High school” with just a 
# capital H ? Now we know that computer languages are 
# going to think those two words are very different 
# things. Regular expressions can be used to find 
# where we have such inconsistencies in our data 
# and correct them. 
# 
# If you like puzzles- this should be pretty fun. 
# If you don’t – you will still find them incredibly 
# useful for making your data submit to your will. 
# 
# Regular Expressions are not just unique to R- in 
# fact all programming languages treat regular 
# expressions fairly similarly. 
# 
# You can use a regular expression function in R to 
# FIND a matching pattern in any data, and then either 
# return information about that match, or edit the 
# string the match is found within in a number of 
# different ways. So let’s go ahead and get into the code. 

text.data <- c("data science",
               "public policy",
               "social policy",
               "Fels Mansion",
               "UPenn",
               "Pennsylvania",
               "policy",
               "data",
               "science",
               "19104",
               "3814 Walnut St",
               "Philadelphia, PA",
               "Philly",
               "Phil",
               "Phila",
               "Natalie",
               "number 1",
               "19104-6286",
               "19081-1731",
               "(215)746-5555",
               "215-746-5555",
               "2157465555")

#-----------------------------------
# GREP
#-----------------------------------
# Notice that I have some strings that contain the same
# word – for instance I have the string “data science” 
# and then later on the string of just “data” 
# 
# And also some strings that contain partial matching 
# patterns – like PHIL in Philadelphia, PA, Philly, 
# And Phil, and so on. 
# 
# And I also have some strings that are just numbers. 
# Notice I have a couple of zip codes and then also a 
# phone number written in 3 different formats. 
# 
# 
# We’ll start with the grep function. We can use grep to 
# find which strings in our vector contain a matching character. 


grep("a", text.data)

# By default, grep outputs the index of where the 
# match occurs. In this first example, we’re using 
# grep to find all of the strings that contain the 
# character “a” in the text.data object. 
# *RUN CODE*
#   
#   Grep outputs the index, or address, of each match. 
# According to the output, it looks like the letter a
# appears in our first string, as well as a fair few others. 
# 
# Sometimes, it will be more helpful for grep to output
# the actual matching string, instead of its index. We
# can use the “value” argument to return the entire 
# string instead of just its location. 
# 
# When we run grep with value = TRUE, we see that
# the same strings are returned. 

# value = FALSE will return the index of elements (it is the default)
grep("a", text.data, value = TRUE)


# Now that we have an idea of how grep works, let’s 
# see how we can create different conditions to be 
# met to return a string. 
# 
# We can use bracket notation inside of our first 
# argument to indicate that we want to grab any match
# from a list of characters. 
# 
# Here, I’ve put all of the numbers from 0 through 9 into s
# quare brackets to indicate that I would like an entire 
# string to be returned as a match if that string 
# contains any of these numbers. 

grep("[0123456789]", text.data, value = TRUE)

# The same principle will hodl true for letters as well.
# We can use grep with bracket notation, to return any 
# string that contains any of the letters inside the 
# bracket notation – that is, either the string contains 
# a OR the string contains T, OR the string could contain
# both A AND T
grep("[at]", text.data, value = TRUE)


# But if we want to be more specific. Let’s say we’re 
# interested in finding strings that contain at least 3 
# numbers next to each other. We can use brackets to 
# define our “slots” – we want 3, and then what we want 
# in each of those slots – in this case, any
# number from 0 through 9. 


grep("[0-9][0-9][0-9]", text.data, value = TRUE)
# Notice, instead of typing out all of the numbers, I’m writing 
# our number condition as 0 DASH 9 – which will function the 
# same way as if I had written out all of the numbers 0 through 9. 

# I could also make this line of code a little shorter by
# using braces to indicate the number of brackets I want. 
grep("[0-9]{3}", text.data, value = TRUE)

# Notice that if I change the condition of 3 numbers found 
# together to 6 numbers found together – the only string 
# that is returned is the completely unformatted phpone 
# number. All other numbers in this list are either shorter,
# OR have some symbol that separates them, in the case of 
# phone numbers either a dash or a parenthesis that keep 
# the string from meeting this condition. 
grep("[0-9]{6}", text.data, value = TRUE)

# 
# The same is going to be true for letters. 
# Notice that when I put just the letter a inside a bracket 
# and then a second bracket with just the letter T, I will 
# only return words that contain the letter A 
# directly followed by the letter T. Notice here 
# that the string containing the word “WALNUT” does 
# not appear, because even though there is an A followed 
# eventually by a “T” – they aren’t directly next to 
# each other so the word wont meet the condition. 
grep("[a][t]", text.data, value = TRUE)


# Its important to note that the brackets are case sensitive. 
# If we want to indicate that we don’t care if a match is upper 
# or lower case, we need to include that in the code. We can
# do this in one of two ways
# 
# We can either, specifically reference the capital letters
# A through Z, notice I’m again using that dash notation 
# that we used for numbers above, and also the lower 
# case letters a through z. 

grep("[A-Za-z]", text.data, value = TRUE)

# We could also just refer to the lower case letters in the 
# condition argument, but add in the argument “ignore.case equals TRUE” 
grep("[a-z]", text.data, value = TRUE, ignore.case = TRUE)

# 
# We can also specify that we are looking for
# a string that BEGINS with a certain character. 
# We use the caret symbol at the FRONT of our 
# condition to denote this. In this next line, 
# I’m asking grep to return any string that 
# begins with at least 5 numbers. 

grep("^[0-9]{5}", text.data, value = TRUE)

# We can similarly ask grep to return any string that ends
# with particular characters by using the “dollar sign” 
# symbol. In this line, I’m asking for grep to return 
# any string that ends with the letter “N”
grep("[n]$", text.data, value = TRUE)

# We can also use the or logical operator – the vertical 
# line, as we would in any logical statement insdie of 
# the condition argument. In this line, we’re asking 
# grep to return a string that contains the phrase 
# “public policy” or “social policy”
grep("(public|social) policy", text.data, value = TRUE)


# The question mark is another useful feature of 
# regular expressions. It allows us to find a 
# string that matches the condition but allow
# some some flexibility in that match. 
# 
# For example, I’m first going to search 
# for the word, “Philadelphia” and grep 
# will only return one string – just the 
# one with the entire word “Philadelphia” within the string. 

grep("Philadelphia", text.data, value = TRUE)

# But what if I know that sometimes, philly folks abbreviate 
# the word to “PHIL”  I could tell grep that I want 
# the word Philadelphia but that the “adelphia” part 
# is actually optional by including the “?” right 
# after “adelphia” that is enclosed by parenthesis. 

grep("Phil(adelphia)?", text.data, value = TRUE)

# Now, you might be thinking to yourself, but, 
# wouldn’t just searching “PHIL” return the
# same strings? Let’s try it. 
grep("Phil", text.data, value = TRUE)

# 
# Yes, in fact it would. To really understand how 
# useful the “question mark” can be – we need 
# to introduce a second topic, a “BOUNDARY”
# 
# 
# Regular Expressions use something called a 
# “boundary” to allow us to create even more 
# specific requests. Let’s say we’re just looking 
# for the string with JUST “Phil” and we DO NOT
# want “philly” or “philadelphia” to return 
# We can wrap “PHIL” in a boundary – 
# using a \\b in the beginning of the 
# word and at the end – to say we want this
# to be the whole word. 
# 
# But the boundary is doing more than just 
# looking for uninterrupted, exact matches. 
# It’s actually looking only at strings that
# contain the condition, in this case PHIL, where 
# PHIL does not touch, on either side, 
# a letter or a number. 

grep("\\bPhil\\b", text.data, value = TRUE)

# 
# If, we did have “PHIL-89” in our list, our grep
# would still return this string. Because, PHIL 
# is intact, it just touches a ‘non’ letter or number character. 
# If we had “PH-IL” in our object, our grep would 
# NOT return that string. 
# 
# The boundary is making whatever is inside it a 
# distinct piece that cannot be separated, and cannot
# be touching a letter or number on either side. 
# 
# But let’s have you test this – try adding the 
# strings “PHIL-89” and “PH-IL” to our text object and 
# running that boundaried grep yourself to see what happens. 



# Now that we understand better what boundaries are 
# doing, let’s go ahead and see them in action with 
# a more complicated string. We’ll also return to 
# that “question Mark” symbol and see how useful that can be. 
# 
# Let’s start by remembering what options we have 
# for strings that contain numbers. 
# Notice we have some zip codes, in two different 
# formats, some phone numbers in different formats, 
# a street address, and a string that just 
# randomly contains the number 1. 
# 
# Suppose that I am interested in pulling out only 
# the zip codes. Now, most of the time zip codes can
# be written with only 5 numbers, but sometimes there’s
# an optional 4 digit number tacked onto the end with a dash. 
# 
# Let’s see if I can pull out just the zip codes
# with this in mind using boundaries. 
# In this line, I am looking for a 5 digit number,
# followed by a ‘dash’ and then a 4 digit number. 

grep("[0-9]", text.data, value = TRUE)

# Just the zips
grep("\\b[0-9]{5}-[0-9]{4}\\b", text.data, value=TRUE)

# using the question mark to make the 4 digits optional
grep("\\b[0-9]{5}(-[0-9]{4})?\\b", text.data, value=TRUE)


# 
# Importantly, regular expressions can be used 
# to identify more than just numbers and letters. 
# We can also interact with any symbol, but some 
# symbols need to be “protected” in the code. By 
# ‘protected’ I mean that we need to prevent R from
# misinterpreting them as part of the syntax of a
# function. We just want r to recognize them as simply 
# character text. To do this, we need to include 
# two slashes \\ before the symbol any time it appears. 
# These are the symbols this rule applies to:
#   
#   \ ^ $ {} [] () <> . * + ? |  &
#   
#   
#   This rule is particularly important for 
# working with phone numbers. 
# 
# In this line, I am asking grep to return a string
# where there is a parenthesis, followed by a 3 digit 
# number, and then a closing parenthesis. Then a 3 
# digit number and then a “dash” and then a 4 digit number. 


grep("\\([0-9]{3}\\)[0-9]{3}-[0-9]{4}", text.data, value=TRUE)

# And we can see this does return our one phone number that is formatted in this way. 
# 
# 
# Now that we feel comfortable with grep, let’s move on to GSUB
# 

#-----------------------------------
# GSUB
#-----------------------------------

# The Gsub function allows us to remove or substitute
# content from a string in question. 
# All of the rules we’ve just learned for writing
# conditions inside of grep, will apply to gsub. 
# 
# So let’s try using gsub to remove every symbol
# in each one of our strings other than numbers
# – so, we’ll just be keeping numbers. 
# We can write that line of code using the “NOT” caret. 
# 
# Notice here that the caret is inside of 
# the bracket – when the caret is outside of
# the bracket, it means “starts with” -when 
# it is inside the bracket “it means NOT”
# 
# Which I agree is confusing and it shouldn’t be 
# that way, but I don’t make the rules. 
# 
# In this line, we’re saying, Find every character 
# within our strings that is NOT the 
# numbers 0 through 9, and then replace those
# matching characters, with NOThing – that’s coming
# from the empty quotation marks, 

gsub("[^0-9]", "", text.data)


# We can also quickly remove all of the SPACEs in our 
# data by giving gsub just a “space” inside of the condition quotes, 
gsub(" ", "", text.data)


# Instead of removing elements, let’s try adding some in. in 
# this line, I am asking gsub to find everywhere the letters 
# “ST” exist together in my strings and replace them with 
# the word “STREET” 
gsub("[S][t]\\b", "Street", text.data)


# We can do even more with replacement – I think 
# this next section is actually really cool. 

# We can use parenthesis in conjunction with “\\” a number, to 
# save parts of our condition to use later on in the function. 
# 
# For example, in this line, I’ve given the condition as
# the word “Natalie” – and I’ve wrapped it in parenthesis. 
# It’s the only thing wrapped, so its reference number
# is going to be \\1. 
# 
# Next, in the replacement argument, I’ve written Ms.
# And then \\1 – which is going to refer to whatever is
# wrapped in parenthesis in the condition argument – 
# and then the word Smith

gsub("(Natalie)","Ms. \\1 Smith",text.data)

# When we run this – we see that the function found 
# where the word ‘Natalie appears” and then replaced
# that string with “Ms. THEN WHAT WE TOLD THE FUNCTION
# TO REMEMBER, which was Natalie, and then Smith. So now,
# Instead of Natalie, we have Ms. Natalie Smith. 
# 
# Let’s see a second example just to be sure we’re 
# comfortable with this – in this line, I’m creating
# two separate parts for the function to remember, 
# first just the letter N and then “atalie” in the 
# replacement section – I’m saying I want the word 
# Ms. Followed by our first item, which is N, then 
# our first and second item inside of parenthesis, 
# followed by Smith
# Which will produce exactly that 

gsub("(N)(atalie)","Ms. \\1. (\\1\\2) Smith",text.data)


# Now let’s try using grep and gsub together. I’d like to
# pull out all of my zip codes into a new object and 
# reformat them so that they all show just the 5 digit
# code most people are familiar with. 
# I’ll use grep to pull out the zipcodes by re-using a 
# line from earlier in our script – so grep, the boundaried
# text, a 5 digit number followed optionally by “dash” 
# a four digit number. 
# And we’re putting that inside the object “Zip”

zip<-grep("\\b[0-9]{5}(-[0-9]{4})?\\b",text.data,value=TRUE)

# Next, we’ll use gsub to keep only the first 5 digit number. 
# We’ll tell gsub to find those zipcodes where there is
# a 5 digit number, then a dash, and then a 4 digit
# number. We’ve wrapped the 5 digits in 
# parenthesis, and the -4digits in another set. 
# 
# In the second argument, we’ll tell gsub to
# keep just \\1 which is our 5 digit number

gsub("([0-9]{5})(-[0-9]{4})","\\1",zip)


#-----------------------------------
# Matches
#-----------------------------------

# Lastly, we’re going to cover the GREP-L function and regEXPR.  
# 
# The Grep-L function is very similar to
# Grep, except that instead of returning either 
# the string where a match is found, or the index of
# that string, grepL returns whether or not you have 
# found a match by returning either TRUE, if a match
# is found, or FALSE, if it is not. 
# 
# In this first example, we’re going to look for the
# capital letter P followed by and E and then an n and another n 

grepl("[P][e][n][n]", text.data)

# grepL returns a vector that returns FALSE for strings 
# that don’t contain the word or word segment “PENN” 
# and true if the string does contain it. 


# The regEXPR function is closely related. This function will 
# return whether or not a match is present in a string as
# well as how many characters, into the string the match 
# appears. AND also the length of all of the matches in
# another vector. 
regexpr("[P][e][n][n]", text.data)


# If we were to save our output from regExpr into an object, 
# we can use the regMATCHES function to have the actual 
# matching text printed to our console. 

matches <-regexpr("[P][e][n][n]", text.data)
regmatches(text.data, matches)

# A closely related function worth mentioning is GregEXPR. 
# This function performs a similar operation to regEXPR, 
# however the output is formatted differently. 
gregexpr("[P][e][n][n]", text.data)

# Notice if we ran Gregexpr on the same condition, of PENN, 
# instead of returning a vector for the match, and another 
# vector for length, GREgExpr returns a list where each 
# element in the list relates to the index of the text 
# data that was given. 

match<-gregexpr("[P][e][n][n]", text.data)
regmatches(text.data, match)

# And if we place the output of GregExpr into regmatches, 
# we get a similar list returned where regmatches will 
# eturn “character of length zero” which is just an empty 
# element, for nonmatches, and the text of the match
# where there are matches. 


# And that ends our lecture on regular expressions!
#   Regular expressions can be tricky at first because
# there so many new rules to learn. My advice to you is
# to first start with the functions themselves:
# grep, gsub, grepL, regExpr, and Gregexpr, and make sure
# you’re comfortable articulating the differences between
# the functions -and can think of a scenario where you would use each one.
# 
# Once you’re comfortable with the differences between the functions,
# then go ahead and start working on becoming comfortable
# with the Rules used to create the patterns.
# To help you remember the rules for conditions, the best
# thing you can do is PRACTICE, PRACTICE, PRACTICE.
# The lab was designed with this in mind.
# When I work with regular expressions, I like to have a
# little cheat sheet handy that has written out the most
# common rules written out in my own words.
# I find this really useful and recommend you do the same.
# 
# And also - Remember, you can always look up things like –
# should the caret be inside or outside the bracket and
# ‘how do I include capital letters in my search’ but
# remembering what tools are available to you, given
# a particular circumstance, will go a long way to
# making you into an excellent data scientist.

