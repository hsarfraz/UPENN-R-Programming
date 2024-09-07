install.packages('stringr')
install.packages('htmlwidgets')

library(RColorBrewer)
library(maps)
library(mapdata)
library(tidyverse)
library(scales)
library(readr)
library(htmlwidgets)

str_length(x) #counts string length

str_c(x,y) #combines string with no spaces

str_c(x,y, sep=". ") #combines strings with space

str_c(c(x,y),collapse=". ") #puts two strings in one

str_sub(x,1,5) #gets a part of a string. Here the start is 1

#str_to_lower:	converts	a	string	to	all	lower-case	letters
#str_to_upper:	converts	a	string	to	all	upper-case	letters
#str_trim:	trims	white	space	from	a	string
#str_pad:	adds	white	space	to	a	string
#str_sort:	sorts	strings	lexically	(i.e.,	in	alphabetical	order)

#>> Pattern Matching
#creating a word list
word_list <- c("apple", "banana", "grape","orange","pear","pineapple") 
#code that tells us which word has 'an' in it 
str_view(word_list, "an")

#R finds words that have the letter 'a' in the middle and does not look at ends/beginning
str_view(word_list, ".a.")
#'match+T' only shows the word list that have the 'a' in the middle 
str_view(word_list, ".a.", match=T)
#to match a period you need to use it as a special character
str_view(c("abc","a.b.c","a.c","def"), "a\\.b")

#matches the	start	of	a	string
str_view(word_list, "^a") 
#matches the	end	of	a	string
str_view(word_list,"e$")
#gives a more exact match by using 'a' and 'e' in the ends to filter
str_view(word_list, "^apple$")

#matches anything but the charecters listed (in this case 'aeiou')
str_view(words, "^[^aeiou]", match=T)

#checks to see which strings have 'cie' in them 
str_view(words, "cie", match=T) 
#checks to see if 'ei' comes after any letter but 'c'
str_view(words, "[^c]ei", match=T) 

#[a-z] matches	any	lower-case	character	between	a	and	z	(i.e.,	the	English	alphabet)
#[A-Z] matches	any	upper-case	character	between	a	and	z
#[:alpha:]:	matches	any	letter
#[:lower:]:	matches	any	lower-case	letter
#[:upper:]:	matches	any	upper-case	letter
#[:digit:]:	matches	any	digit
#[:alnum:]:	matches	any	letter	or	number

#picks characters that begin with 'i' and end with 'ng' or 'se'
str_view(words, "i(ng|se)$", match=T)

#finds words that begin with 'ab' or 'ad'
str_view(words, "^ab|^ad", match=T)