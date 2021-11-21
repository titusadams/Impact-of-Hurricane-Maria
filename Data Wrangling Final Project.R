# Comprehensive Project For Data Wrangling
# Looking closely at Hurricane Maria in 2017 where hurricane support was
# drastically underfunded due to the undercount in deaths directly related
# to the hurricane.

# Use data wrangling to extract the actual daily mortaility data and investigate
# hurricane marias immediate effect on the daily mortality


#-------------------------------------------------------------------------------
# Loading all the packages I may need
library(tidyverse)
library(rvest) # For web scraping if necessary
library(dslabs) # contains the much of the raw data I may use
library(ggplot2) # for visualization needs
library(lubridate) # For dealing with dates
library(tidyr) # for beautification, may be contained in tidyverse
library(scales) # for visual aesthetics
library(pdftools) #
options(digits = 3) # 3 digits for numerics
#-------------------------------------------------------------------------------
# Importaing daily mortality data for Puerto Rico from 1/1/2015 to 5/31/2018

# fn is the raw data pdf file downloaded from the dslabs package 
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")

#system2("open", args = fn) #pdf contains data sorted by month and then year

# read the pdf document and store the text in the text variable
text <- pdf_text(fn)
is.list(text)
class(text)

# extract the 9th page and save the page into a variable named x, split the spaces
# with the stringr function str_split where each new line is split "\n" = new line
# this creates a list object
x <- text[9] %>% str_split("\n")
length(x)

is.list(x) # the stringer function converts the character vector into a list
s <- x[[1]]
length(s)
class(s) 

p <- unlist(x)
length(p)

# We use stringr to remove the whitespace from both sides of each text string in s
s <- str_trim(s, side="both")

# we want to save the column that contains the header using 2015 as our key pattern
# i'll use the str_which formula, which takes a string and a pattern and finds the 
# indexes of strings that contain a match 

str_which(s,"2015") #index 3 and 25 contain matching values
header_index <- str_which(s,"2015") %>% first()

s[header_index] 
# we can see that this gives us the header for the table on the 9th page containing
# SEP 2015 2016 2017 2018 
# we want to extract both the month and the header which are the years

header <- s[header_index]   
header <- str_split_fixed(header, " ",2)
month <- header[1]
years <- header[2]

tail_index <- str_which(s,"Total") # The index for the last row of the table

# Now i'll use the str_count tool to count the number of rows that have only one number in them
# This corresponds to areas of the pdf that have a graph

pattern <- "\\d+"
str_count(s, pattern) %>% str_count("1") %>% sum() # total number of lines with 1 number
str_count(s, pattern) %>% str_which("1") # indicies of the lines to be removed


# Now I need to create a new version of s where we:
# remove everything before the header_index
# remove every entry where n = 1
# remove tail_index and everything after it

#s <- str_count(s,pattern) > 1

# remove everything up to  header_index
s <- s[-(1:header_index)]

length(s) # testing the new length of the vector (should be less)

#-------------------------------------------------------------------------------
# DEBUGGING SECTION ------------------------------------------------------------
##------------------------------------------------------------------------------
# I need to remove everything from tail index on
# this original code has an issue where it only removes the last 3 items

#  tail_index was 36 when it should be 33
tail_index <- str_which(s,"Total")
tail_index # should be 33
length(s) #should be 38

s <- s[-(tail_index:length(s))] # remove everything after tail_index
length(s) # testing the new length of the vector (should be less)


# Solution: The tail_index was calculated prior to the new s that was created
# after removing everything up to header_index. It needed to be recalculated 
# after this to gather the correct index
#------------------------------------------------------------------------------
#DEBUGGING COMPLETE------------------------------------------------------------
#------------------------------------------------------------------------------

# remove any entry where the number of characters is only 1
s <- s[str_count(s, pattern) > 1]

length(s) # testing the new length of the vector (should be less)

# remove all test that is not a digit or a space
# learned that ^ in regex inside the brackets means "not" like how ! means not 
# I also recall ^ outside the brackets in regex stands for the beggining of a string

s <- str_remove_all(s, "[^\\d\\s]")

s

# The project gave assistance with how to use str_split_fixed to split the vector of strings
# into a matrix of sub-strings. The splitting occurs at the pattern match

s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
class(s) # we see that s is now a matrix class isntead of a vector of characters


# Organizing the newly created matrix
s
s <- s %>% data.frame()

tab <- mutate(s, 
       day = as.numeric(X1),
       "2015" = as.numeric(X2),
       "2016" = as.numeric(X3),
       "2017" = as.numeric(X4),
       "2018" = as.numeric(X5))[,6:10]

mean(tab$'2015') # mean deaths in 2015
mean(tab$'2016') # mean deaths in 2016

# Average number of deaths per day in september 2017 prior to the hurricane
pre_hurricane <- tab %>% filter(day <= 19)
mean(pre_hurricane$'2017')

# Average number of deaths per day in september 2017 post hurricane
post_hurricane <- tab %>% filter(day >=20)
mean(post_hurricane$'2017')

# Use 'gather' to create a long column of deaths per day per year
tab <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))
tab

tab %>% filter(year == 2015 | year == 2016 | year == 2017) %>%
  ggplot(aes(x = day, y = deaths, col = year)) + geom_point() +
  labs(title = "Puerto Rico Deaths Per Day Per Year",
       x = "Day",
       y = "Deaths") + geom_vline(aes(xintercept = 20)) +
  geom_hline(aes(yintercept = 100))
  

