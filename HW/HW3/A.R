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

#********************************************************
#This line provides us the ability to capture output,
#very useful for development
debug <- 1
if(debug == 1)
{
	system('rm -f output.txt')
	system('touch output.txt')
	Rout <- file('output.txt','w')
}
#This function will simplify our life
write <- function(x)
{
	y<-as.character(x)
	writeLines(y,Rout,sep="\n")
}
#This function just makes for prettier output
cep <- function()
{
	writeLines('*********************************************************',Rout,sep='\n')
}
#********************************************************

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
home_page <- readLines('st.txt')

#This is where we store the pattern to search for the links
#to the president's speeches. This could be a more elegant
#pattern, but c'mon, elegance takes times. This gets the
#job done. Besides, many programs are paid for by lines
#of code, so lets maximize profits, right?
link_pattern <- '"http://www\\.presidency\\.ucsb\\.edu/ws/index\\.php\\?pid=[[:digit:]]{1,6}"'
#Here we store our speech search pattern
speech_pattern <-'<span class="displaytext">.*?</span>'
#Here we store out citation search pattern
cit_pattern <- '<strong>Citation:'
#Here we store our name pattern
name_pattern <- 'Citation:&nbsp;</strong></span><span class="ver10">.*?:'
#Here we store our year pattern
year_pattern <- '[[:digit:]], [[:digit:]]{4,4}'

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
	if(!is.na(speech_links[i]))
	{
		count = count + 1
		if(debug==1)
		{
			if(count==1)
			{
				cep()
				write('The speech links are					')
			}
			write(speech_links[i])
		}
	}
}
if(debug==1)
{
	cep()
	write("The number of speeches found is")
	write(count)
	cep()
}

#Here we create a function to "unpack" our speech links"

unpack <- function(speech_path)
{
#This stores the html given by the link we will supply later
	speech_page <- readLines(speech_path)
#This uses our speech search pattern to pull out the speech
#This grabs the line where the speech lives
	line_number <- grep(speech_pattern,speech_page)
#This is the string with our search criteria in it
	uncleaned_string <- str_extract(speech_page[line_number],
		speech_pattern)
#This is our string without the search criteria in it
	cleaned_string <- substr(uncleaned_string,27,
		nchar(uncleaned_string)-7)
#This will deal with encoding issues
	ideal_string <- iconv(cleaned_string,'','ASCII','???')
#Here we assign the cleaned string to the dynamic variable
	return(ideal_string)
}
#Because I desire to work with R's ridiculous data
#structures as little as possible, we create an
#additional function to retrieve the name and year
#of each speech
get_data <- function(speech_path,speech_index)
{
#Get the file (this is a repeat of above but oh well)
	file <- readLines(speech_path)
#Get line of citation
	cit_line <- grep(cit_pattern,file)
#Get uncleaned string of name
	uc_name <- str_extract(file[cit_line],
		name_pattern)
#Get uncleaned year
	uc_year <- str_extract(file[cit_line],
		year_pattern)
#Clean name
	name <- substr(uc_name,52,nchar(uc_name)-1)
#Clean year
	year <- substr(uc_year,4,nchar(uc_year))
#Cat into a vector
	output <- c(speech_index,name,year)
#return vector
	return(output)
}

#Here we create a function to make the text "human"
#readable

readable <- function(string_path)
{
#This will replace all the html <p>s with \n's
	new_1 <- str_replace_all(string_path,'<p>','\n')
#This will remove all emotion tags
	new_2 <- str_replace_all(new_1,'\\[.*?\\]','')
#This will remove all html tags
	new_3 <- str_replace_all(new_2,'<.*?>','')
#This will return our value
	return(new_3)
}

#This function (which should be called before, by
#preference really) the readable function, counts 
# the emotion tags

emotion_count <- function(string_path,speech_index)
{
#This one counts the laughs
	laugh_count <- str_count(string_path,
		ignore.case('\\[.*?aug.*?\\]'))
#This one couts the applause
	applause_count <- str_count(string_path,
		ignore.case('\\[.*?app.*?\\]'))
#Return the value as a vector
	output <- c(speech_index,laugh_count,applause_count)
	return(output) 
}

word_vectorize <- function(string_path,speech_index)
{
#First we will strip the \n s that are still hanging around
	clean_string <- str_replace_all(string_path,
		'\\n','')
#This regex word count simply counts the number of non-word
#characters which should approximately equal the number of words
#as punctuation and spaces are non-word characters. It will
#double count hyphenated words but lets not be picky.
	word_number <- str_count(clean_string,'\\W+')
#Here, to make our life easier, we directly write out of this
#function to the parent environement. While this does defeat
#some of the point of functions, it prevents our simple
#character vector from becoming complicated
	word_counts[speech_index] <<- word_number	
#Here we create a vector to hold our words, of approp length
	long_vec <- vector('character',word_number)
#This str_extract here will pull out all the words followed
#by a space or punctuation. The fault here is that the
#punctuation will be included with the word, but we can't
#be perfect
	word_vector <- str_extract_all(clean_string,
		'[[:alpha:]]*? |[[:alpha:]]*?[[:punct:]]')
#Here we return our word_vector
	return(word_vector)
}
 
#This is a blank list where we will store the unaltered speeches
unaltered_speeches <- vector('list',count)
#This is a data frame  where we will store the speech data
listed_data <- data.frame(matrix(ncol=3,nrow=count))
#This list will store the "human readable" speeches
hreadable <- vector('list',count)
#This data frame will store the emotion counts for each speech
emotion_store <- data.frame(matrix(ncol=3,nrow=count))
#This vector  will store the word_counts for each speech
word_counts <- vector('numeric',count)
#This list will store our incredibly long character vectors
#Its indexing will be ahead of all others by 1 as the only good
#way I can find to attach vectors of unknown length to a list is
#to append them.
word_vectors <- list(0)
#This is an indexing variable
speech_number <-1 

#This for loop will go through our speech_links and
#initiate function calls and variable assignments to our
#liking
for(i in 1:length(speech_links))
{
#This if statement causes us to execute only if we have a
#valid speech link
	if(!is.na(speech_links[i]))
	{
		trimmed_path = substr(speech_links[i],2,
			nchar(speech_links[i])-1)
		unaltered_speeches[speech_number]<-unpack(
			trimmed_path)
		listed_data[speech_number,] <- get_data(
			trimmed_path,speech_number)
		the_speech <- unlist(unaltered_speeches[
			speech_number])
		human_speech <- readable(the_speech)
		hreadable[speech_number] <- human_speech 
		emotion_store[speech_number,] <- emotion_count(
			the_speech,speech_number)
		word_vectors[[length(word_vectors)
			+1]] <- word_vectorize(human_speech,
			speech_number)
		speech_number <- speech_number + 1
		if(debug==1)
		{
			dfrow <- c(listed_data[speech_number-1,1
				],listed_data[speech_number-1,2
				],listed_data[speech_number-1,3
				])
			write(dfrow)
			cep()
			write(unaltered_speeches[speech_number
				-1])
			cep()
			write('The emotion counts are')
			dcounts <-c('Laughter',emotion_store[
				speech_number-1,2],'Applause',
				emotion_store[speech_number
				-1,3])
			write(dcounts)
			cep()
			write('The human readable speech')
			cep()
			write('Word count')
			write(word_counts[speech_number-1])
			write('Length word vector')
			write(length(word_vectors[[
				speech_number]][[1]]))
			cep()	
			write(hreadable[speech_number-1])
			cep()
		}
	}
}
if(debug==1)
{
	cat(unlist(word_vectors[[2]]))
	cep()
}
	
#These lines simply tell us that we have finished
cat("END EXECUTION\n")
cat("*************************************************\n")
