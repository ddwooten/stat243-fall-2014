#!/usr/bin/Rscript
#Creator: Daniel Wooten
#Class STATS243
#PS3

#The first line makes this file callable from the command 
#line,assuming you've given it executable permissions
#Comments will explain how this code works

#These lines simply demarcate the beginning of the prog
cat("*************************************************\n")
cat("BEGIN EXECUTION\n") 

#This line loads the required libraries and packages that
# we want to use
library(XML)
library(stringr)

#This line uses scrapeR's retreival function to store
#the websites html file in an R object
#orginal_html <- scrape(url='www.presidency.ucb.edu/sou.php#axzz265cEKp1a'+
#,) 

#The first part of this code will not use functions as the 
#retrieval of the webpage, well, it doesn't make sense
#to "vectorize" that.
home_page <- readLines('http://www.presidency.ucsb.edu/sou.php#axzz265cEKp1a')

#This is where we store the pattern to search for the links
#to the president's speeches. This could be a more elegant
#pattern, but c'mon, elegance takes times. This gets the
#job done. Besides, many programs are paid for by lines
#of code, so lets maximize profits, right?
link_pattern <- '"http://www\\.presidency\\.ucsb\\.edu/ws/index\\.php\\?pid=[[:digit:]]{1,6}"'
#Here we store our speech search pattern
speech_pattern <-'<span class="displaytext">.*?</span>'

#Here we employ stringr to extract from the html
#the lines in which our pattern is found. It also returns
#the pattern.
speech_links <- str_extract(home_page, link_pattern)

#********************************************************
#Here we use two for loops, as well as two pattern matches
#to go through speech_links and check for duplicates
#essentially deleting them should we find them

#for(i in 1:length(speech_links))
#{
#	speech_id <- str_extract(speech_links[i],
#		'[[:digit:]]{1,6}')
#	for(j in (i+1):length(speech_links))
#	{
#		search_id <- str_extract(speech_links[j],
#			'[[:digit:]]{1,6}')
#		if(identical(speech_id,search_id))
#		{
#			speech_links[j] = 'NA'
#		}
#	}
#}
#*******************************************************

#Here we use a for loop to count up the number of speech
#links. This will be handy. We use a dummy counter variable
#, count

count = 0

for(i in 1:length(speech_links))
{
	if(!is.na(speech_links[i])) count = count + 1
}

cat(count)
cat("\n")

#Here we create a function to "unpack" our speech links"

unpack <- function(speech_link,speech_number){
	speech_page <- readLines(speech_link)
		
#These lines simply tell us that we have finished
cat("END EXECUTION\n")
cat("*************************************************\n")
