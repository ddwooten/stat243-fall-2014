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
	system('rm -f output.dat')
	system('touch output.dat')
	system('rm -f error.out')
	system('touch error.out')
	sink('error.out', append=TRUE)
	Rout <- file('output.dat','w')
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
home_page <- readLines('http://www.presidency.ucsb.edu/sou.php#axzz265cEKp1a')
#This line is to allow for easy switching to special
#input for debugging purposes.
#home_page <- readLines('test.txt')

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
#This will deal with encoding issues
	speech_page <- iconv(readLines(speech_path),'','ASCII','???')
#This uses our speech search pattern to pull out the speech
#This grabs the line where the speech lives
	line_number <- grep(speech_pattern,speech_page)
#This is the string with our search criteria in it
	uncleaned_string <- str_extract(speech_page[line_number],
		speech_pattern)
#This is our string without the search criteria in it
	cleaned_string <- substr(uncleaned_string,27,
		nchar(uncleaned_string)-7)
#Here we assign the cleaned string to the dynamic variable
	return(cleaned_string)
}
#Because I desire to work with R's ridiculous data
#structures as little as possible, we create an
#additional function to retrieve the name and year
#of each speech
get_data <- function(speech_path,speech_index)
{
#Get the file (this is a repeat of above but oh well)
	file <- iconv(readLines(speech_path),'','ASCII','???')
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
	output <- c(as.character(speech_index),name,year)
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
		'[[:alpha:]]*? |[[:alpha:]]*?[.\\?!]')
#Here we return our word_vector
	return(word_vector)
}

sentence_vectorize <- function(string_path,speech_index)
{
#First we will strip the \n s that are still hanging around
	clean_string <- str_replace_all(string_path,
		'\\n','')
#This pattern (which I'm quite proud of) extracts sentences
	sent_pat <- '[[:upper:]][[:alpha:]]*?( |\\.|,|;|:|-).*?([[:lower:]][\\.\\?!])'
	sentence_number <- str_count(clean_string,sent_pat)
#Here, to make our life easier, we directly write out of this
#function to the parent environement. While this does defeat
#some of the point of functions, it prevents our simple
#character vector from becoming complicated
	sentence_counts[speech_index] <<- sentence_number	
#Here we create a vector to hold our sentences, of approp length
	long_vec <- vector('character',sentence_number)
#This str_extract here will pull out all the sentences
	sent_vector <- str_extract_all(clean_string,sent_pat)
#Here we return our word_vector
	return(sent_vector)
}

#This function will allow us to search the speech data to find
#the index by which we may reference that speech in the various
#data structures
find_index <- function(query,type,df)
{
	for(i in 1:count)
	{
		if(identical(df[i,type],query))
		{
			output <- as.numeric(df[i,1])
			break
		}
	}
	return(output)	
}

#This function will return a data frame of the occurances in 
#a speech that we desire to know
phrase_counter <- function(string_path)
{
#This, and the following lines, extract what we desire
	I_count <- str_count(string_path,' I[ [:punct:]]')
	we_count <- str_count(string_path,ignore.case(
		' (we)[ [:punct:]]'))
	American_count <- str_count(string_path,ignore.case(
		' (america|american)[ [:punct:]]'))
	democracy_count <- str_count(string_path,
		' (democracy|democratic)[ [:punct:]]')
	Republic_count <- str_count(string_path,ignore.case(
		' republic[ [:punct:]]'))
	Democratic_count <- str_count(string_path,
		' (Democrat|Democratic)[ [:punct:]]')
	Republican_count <- str_count(string_path,
		' Republican[ [:punct:]]')
	freedom_count <- str_count(string_path,ignore.case(
		' (free|freedom)[ [:punct:]]'))
	war_count <- str_count(string_path, ignore.case(
		' war[ [:punct:]]'))
	God_count <- str_count(string_path,ignore.case(
		' god[ [:punct:]^( b)]'))
	Godbless_count <- str_count(string_path,ignore.case(
		' god bless[ [:punct:]]'))
	Christian_count <- str_count(string_path,ignore.case(
		' (Jesus|Christ|Christian)[ [:punct:]]'))
#This will cat all of these to a vector for output
	output <- c(I_count,we_count,American_count,
			democracy_count,Republic_count,
			Democratic_count,Republican_count,
			freedom_count,
			war_count,God_count,Godbless_count,
			Christian_count)
	return(output)
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
#This vector will store our sentence counts
sentence_counts <- vector('numeric',count)
#This list will store, in a similar fashion to the above,
#our setence vectors
sentence_vectors <- list(0)
#This dataframe, like the frame above it, will hold our occurance
#data frames
rhetoric_list <- data.frame(matrix(ncol=12,nrow=count))
#This gives our data.frame columns names
colnames(rhetoric_list) <- c("I","we","America{,n}","democra{cy,tic}","republic","Democrat{,ic}","Republican","free{,dom}","war","God","God Bless","{Jesus,Christ,Christian}")
#This is an indexing variable
speech_number <-1 

#This for loop will go through our speech_links and
#initiate function calls and variable assignments to our
#liking
for(i in 1:length(speech_links))
{
#This if statement causes us to execute only if we have a
#valid speech link. All that is performed are various
#function calls to populate the data structures asked
#for
	if(!is.na(speech_links[i]))
	{
		trimmed_path = substr(speech_links[i],2,
			nchar(speech_links[i])-1)
		write(trimmed_path)
		cep()
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
		sentence_vectors[[length(sentence_vectors)
			+1]] <- sentence_vectorize(human_speech,
			speech_number)
		rhetoric_list[speech_number,] <- phrase_counter(
			human_speech)
		speech_number <- speech_number + 1
		if(debug==1)
		{
			dfrow <- c(listed_data[speech_number-1,1
				],listed_data[speech_number-1,2
				],listed_data[speech_number-1,3
				])
			write(dfrow)
			cep()
			write(trimmed_path)
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
			write("Sentence count")
			write(length(sentence_vectors[[
				speech_number]][[1]]))
			cep()
			write("The occurance of key phrases is")
			cep()
			write("I count")
			write(rhetoric_list[speech_number-1,1]) 	
			write("We count")
			write(rhetoric_list[speech_number-1,2]) 	
			write("American count")
			write(rhetoric_list[speech_number-1,3]) 
			write("Democracy count")	
			write(rhetoric_list[speech_number-1,4]) 	
			write("Republic count")
			write(rhetoric_list[speech_number-1,5]) 	
			write("Democrat count")
			write(rhetoric_list[speech_number-1,6]) 	
			write("Republican count")
			write(rhetoric_list[speech_number-1,7]) 	
			write("Freedom count")
			write(rhetoric_list[speech_number-1,8]) 	
			write("War count")
			write(rhetoric_list[speech_number-1,9]) 	
			write("God count")
			write(rhetoric_list[speech_number-1,10]) 	
			write("God Bless count")
			write(rhetoric_list[speech_number-1,11]) 	
			write("Christianity count")
			write(rhetoric_list[speech_number-1,12]) 	
			cep()
			write(hreadable[speech_number-1])
			cep()
		}
	}
}
if(debug==1)
{
#	cat(unlist(word_vectors[[2]]))
#	cat(unlist(sentence_vectors[[2]]))
#	yr <- find_index("2014",3,listed_data)
#	cat(yr)
	cat('\n')
	cep()
}
	
#These lines simply tell us that we have finished
cat("END EXECUTION\n")
cat("*************************************************\n")
