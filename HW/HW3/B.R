% #!/usr/bin/Rscript
% #Creator: Daniel Wooten
% #Class STATS243
% #PS3
% 
% #The first line makes this file callable from the command 
% #line,assuming you've given it executable permissions
% #Comments will explain how this code works
% 
% #These lines simply demarcate the beginning of the prog
% cat("*************************************************\n")
% cat("BEGIN EXECUTION\n") 
% 
% #********************************************************
% #This line provides us the ability to capture output,
% #very useful for development
% debug <- 0 
% if(debug == 1)
% {
% 	system('rm -f output.dat')
% 	system('touch output.dat')
% 	Rout <- file('output.dat','w')
% }
% #This function will simplify our life
% write <- function(x)
% {
% 	y<-as.character(x)
% 	writeLines(y,Rout,sep="\n")
% }
% #This function just makes for prettier output
% cep <- function()
% {
% 	writeLines('*********************************************************',Rout,sep='\n')
% }
% #********************************************************
% 
% #This line loads the required libraries and packages that
% # we want to use
% library(XML)
% library(stringr)
% 
% #The first part of this code will not use functions as the 
% #retrieval of the webpage, well, it doesn't make sense
% #to "vectorize" that.
% home_page <- readLines('http://www.presidency.ucsb.edu/sou.php#axzz265cEKp1a')
% 
% #This is where we store the pattern to search for the links
% #to the president's speeches. This could be a more elegant
% #pattern, but c'mon, elegance takes times. This gets the
% #job done. Besides, many programs are paid for by lines
% #of code, so lets maximize profits, right?
% link_pattern <- '"http://www\\.presidency\\.ucsb\\.edu/ws/index\\.php\\?pid=[[:digit:]]{1,6}"'
% #Here we store our speech search pattern
% speech_pattern <-'<span class="displaytext">.*?</span>'
% #Here we store out citation search pattern
% cit_pattern <- '<strong>Citation:'
% #Here we store our name pattern
% name_pattern <- 'Citation:&nbsp;</strong></span><span class="ver10">.*?:'
% #Here we store our year pattern
% year_pattern <- '[[:digit:]], [[:digit:]]{4,4}'
% 
% #Here we employ stringr to extract from the html
% #the lines in which our pattern is found. It also returns
% #the pattern.
% speech_links <- str_extract(home_page, link_pattern)
% 
% #Here we use a for loop to count up the number of speech
% #links. This will be handy. We use a dummy counter variable
% #, count
% 
% count = 0
% 
% for(i in 1:length(speech_links))
% {
% 	if(!is.na(speech_links[i]))
% 	{
% 		count = count + 1
% 		if(debug==1)
% 		{
% 			if(count==1)
% 			{
% 				cep()
% 				write('The speech links are					')
% 			}
% 			write(speech_links[i])
% 		}
% 	}
% }
% if(debug==1)
% {
% 	cep()
% 	write("The number of speeches found is")
% 	write(count)
% 	cep()
% }
% 
% #Here we create a function to "unpack" our speech links"
% 
% unpack <- function(speech_path)
% {
% #This stores the html given by the link we will supply later
% #This will deal with encoding issues
% 	speech_page <- iconv(readLines(speech_path),'','ASCII','???')
% #This uses our speech search pattern to pull out the speech
% #This grabs the line where the speech lives
% 	line_number <- grep(speech_pattern,speech_page)
% #This is the string with our search criteria in it
% 	uncleaned_string <- str_extract(speech_page[line_number],
% 		speech_pattern)
% #This is our string without the search criteria in it
% 	cleaned_string <- substr(uncleaned_string,27,
% 		nchar(uncleaned_string)-7)
% #Here we assign the cleaned string to the dynamic variable
% 	return(cleaned_string)
% }
% #Because I desire to work with R's ridiculous data
% #structures as little as possible, we create an
% #additional function to retrieve the name and year
% #of each speech
% get_data <- function(speech_path,speech_index)
% {
% #Get the file (this is a repeat of above but oh well)
% 	file <- iconv(readLines(speech_path),'','ASCII','???')
% #Get line of citation
% 	cit_line <- grep(cit_pattern,file)
% #Get uncleaned string of name
% 	uc_name <- str_extract(file[cit_line],
% 		name_pattern)
% #Get uncleaned year
% 	uc_year <- str_extract(file[cit_line],
% 		year_pattern)
% #Clean name
% 	name <- substr(uc_name,52,nchar(uc_name)-1)
% #Clean year
% 	year <- substr(uc_year,4,nchar(uc_year))
% #Cat into a vector
% 	output <- c(as.character(speech_index),name,year)
% #return vector
% 	return(output)
% }
% 
% #Here we create a function to make the text "human"
% #readable
% 
% readable <- function(string_path)
% {
% #This will replace all the html <p>s with \n's
% 	new_1 <- str_replace_all(string_path,'<p>','\n')
% #This will remove all emotion tags
% 	new_2 <- str_replace_all(new_1,'\\[.*?\\]','')
% #This will remove all html tags
% 	new_3 <- str_replace_all(new_2,'<.*?>','')
% #This will return our value
% 	return(new_3)
% }
% 
% #This function (which should be called before, by
% #preference really) the readable function, counts 
% # the emotion tags
% 
% emotion_count <- function(string_path,speech_index)
% {
% #This one counts the laughs
% 	laugh_count <- str_count(string_path,
% 		ignore.case('\\[.*?aug.*?\\]'))
% #This one couts the applause
% 	applause_count <- str_count(string_path,
% 		ignore.case('\\[.*?app.*?\\]'))
% #Return the value as a vector
% 	output <- c(speech_index,laugh_count,applause_count)
% 	return(output) 
% }
% 
% word_vectorize <- function(string_path,speech_index)
% {
% #First we will strip the \n s that are still hanging around
% 	clean_string <- str_replace_all(string_path,
% 		'\\n','')
% #This regex word count simply counts the number of non-word
% #characters which should approximately equal the number of words
% #as punctuation and spaces are non-word characters. It will
% #double count hyphenated words but lets not be picky.
% 	word_number <- str_count(clean_string,'\\W+')
% #Here, to make our life easier, we directly write out of this
% #function to the parent environement. While this does defeat
% #some of the point of functions, it prevents our simple
% #character vector from becoming complicated
% 	word_counts[speech_index] <<- word_number	
% #Here we create a vector to hold our words, of approp length
% 	long_vec <- vector('character',word_number)
% #This str_extract here will pull out all the words followed
% #by a space or punctuation. The fault here is that the
% #punctuation will be included with the word, but we can't
% #be perfect
% 	word_vector <- str_extract_all(clean_string,
% 		'[[:alpha:]]*? |[[:alpha:]]*?[.\\?!]')
% #Here we return our word_vector
% 	return(word_vector)
% }
% 
% sentence_vectorize <- function(string_path,speech_index)
% {
% #First we will strip the \n s that are still hanging around
% 	clean_string <- str_replace_all(string_path,
% 		'\\n','')
% #This pattern (which I'm quite proud of) extracts sentences
% 	sent_pat <- '[[:upper:]][[:alpha:]]*?( |\\.|,|;|:|-).*?([[:lower:]][\\.\\?!])'
% 	sentence_number <- str_count(clean_string,sent_pat)
% #Here, to make our life easier, we directly write out of this
% #function to the parent environement. While this does defeat
% #some of the point of functions, it prevents our simple
% #character vector from becoming complicated
% 	sentence_counts[speech_index] <<- sentence_number	
% #Here we create a vector to hold our sentences, of approp length
% 	long_vec <- vector('character',sentence_number)
% #This str_extract here will pull out all the sentences
% 	sent_vector <- str_extract_all(clean_string,sent_pat)
% #Here we return our word_vector
% 	return(sent_vector)
% }
% 
% #This function will allow us to search the speech data to find
% #the index by which we may reference that speech in the various
% #data structures
% find_index <- function(query,type,df)
% {
% 	for(i in 1:count)
% 	{
% 		if(identical(df[i,type],query))
% 		{
% 			output <- as.numeric(df[i,1])
% 			break
% 		}
% 	}
% 	return(output)	
% }
% 
% #This function will return a data frame of the occurances in 
% #a speech that we desire to know
% phrase_counter <- function(string_path)
% {
% #This, and the following lines, extract what we desire
% 	I_count <- str_count(string_path,' I[ [:punct:]]')
% 	we_count <- str_count(string_path,ignore.case(
% 		' (we)[ [:punct:]]'))
% 	American_count <- str_count(string_path,ignore.case(
% 		' (america|american)[ [:punct:]]'))
% 	democracy_count <- str_count(string_path,
% 		' (democracy|democratic)[ [:punct:]]')
% 	Republic_count <- str_count(string_path,ignore.case(
% 		' republic[ [:punct:]]'))
% 	Democratic_count <- str_count(string_path,
% 		' (Democrat|Democratic)[ [:punct:]]')
% 	Republican_count <- str_count(string_path,
% 		' Republican[ [:punct:]]')
% 	freedom_count <- str_count(string_path,ignore.case(
% 		' (free|freedom)[ [:punct:]]'))
% 	war_count <- str_count(string_path, ignore.case(
% 		' war[ [:punct:]]'))
% 	God_count <- str_count(string_path,ignore.case(
% 		' god[ [:punct:]^( b)]'))
% 	Godbless_count <- str_count(string_path,ignore.case(
% 		' god bless[ [:punct:]]'))
% 	Christian_count <- str_count(string_path,ignore.case(
% 		' (Jesus|Christ|Christian)[ [:punct:]]'))
% #This will cat all of these to a vector for output
% 	output <- c(I_count,we_count,American_count,
% 			democracy_count,Republic_count,
% 			Democratic_count,Republican_count,
% 			freedom_count,
% 			war_count,God_count,Godbless_count,
% 			Christian_count)
% 	return(output)
% }
% 	
% 		
%  
% #This is a blank list where we will store the unaltered speeches
% unaltered_speeches <- vector('list',count)
% #This is a data frame  where we will store the speech data
% listed_data <- data.frame(matrix(ncol=3,nrow=count))
% #This list will store the "human readable" speeches
% hreadable <- vector('list',count)
% #This data frame will store the emotion counts for each speech
% emotion_store <- data.frame(matrix(ncol=3,nrow=count))
% #This vector  will store the word_counts for each speech
% word_counts <- vector('numeric',count)
% #This list will store our incredibly long character vectors
% #Its indexing will be ahead of all others by 1 as the only good
% #way I can find to attach vectors of unknown length to a list is
% #to append them.
% word_vectors <- list(0)
% #This vector will store our sentence counts
% sentence_counts <- vector('numeric',count)
% #This list will store, in a similar fashion to the above,
% #our setence vectors
% sentence_vectors <- list(0)
% #This dataframe, like the frame above it, will hold our occurance
% #data frames
% rhetoric_list <- data.frame(matrix(ncol=12,nrow=count))
% #This gives our data.frame columns names
% colnames(rhetoric_list) <- c("I","we","America{,n}","democra{cy,tic}","republic","Democrat{,ic}","Republican","free{,dom}","war","God","God Bless","{Jesus,Christ,Christian}")
% #This is an indexing variable
% speech_number <-1 
% 
% #This for loop will go through our speech_links and
% #initiate function calls and variable assignments to our
% #liking
% for(i in 1:length(speech_links))
% {
% #This if statement causes us to execute only if we have a
% #valid speech link. All that is performed are various
% #function calls to populate the data structures asked
% #for
% 	if(!is.na(speech_links[i]))
% 	{
% 		trimmed_path = substr(speech_links[i],2,
% 			nchar(speech_links[i])-1)
% 		write(trimmed_path)
% 		cep()
% 		unaltered_speeches[speech_number]<-unpack(
% 			trimmed_path)
% 		listed_data[speech_number,] <- get_data(
% 			trimmed_path,speech_number)
% 		the_speech <- unlist(unaltered_speeches[
% 			speech_number])
% 		human_speech <- readable(the_speech)
% 		hreadable[speech_number] <- human_speech 
% 		emotion_store[speech_number,] <- emotion_count(
% 			the_speech,speech_number)
% 		word_vectors[[length(word_vectors)
% 			+1]] <- word_vectorize(human_speech,
% 			speech_number)
% 		sentence_vectors[[length(sentence_vectors)
% 			+1]] <- sentence_vectorize(human_speech,
% 			speech_number)
% 		rhetoric_list[speech_number,] <- phrase_counter(
% 			human_speech)
% 		speech_number <- speech_number + 1
% #This is all for debugging purposes
% 		if(debug==1)
% 		{
% 			dfrow <- c(listed_data[speech_number-1,1
% 				],listed_data[speech_number-1,2
% 				],listed_data[speech_number-1,3
% 				])
% 			write(dfrow)
% 			cep()
% 			write(trimmed_path)
% 			cep()
% 			write(unaltered_speeches[speech_number
% 				-1])
% 			cep()
% 			write('The emotion counts are')
% 			dcounts <-c('Laughter',emotion_store[
% 				speech_number-1,2],'Applause',
% 				emotion_store[speech_number
% 				-1,3])
% 			write(dcounts)
% 			cep()
% 			write('The human readable speech')
% 			cep()
% 			write('Word count')
% 			write(word_counts[speech_number-1])
% 			write('Length word vector')
% 			write(length(word_vectors[[
% 				speech_number]][[1]]))
% 			cep()
% 			write("Sentence count")
% 			write(length(sentence_vectors[[
% 				speech_number]][[1]]))
% 			cep()
% 			write("The occurance of key phrases is")
% 			cep()
% 			write("I count")
% 			write(rhetoric_list[speech_number-1,1]) 	
% 			write("We count")
% 			write(rhetoric_list[speech_number-1,2]) 	
% 			write("American count")
% 			write(rhetoric_list[speech_number-1,3]) 
% 			write("Democracy count")	
% 			write(rhetoric_list[speech_number-1,4]) 	
% 			write("Republic count")
% 			write(rhetoric_list[speech_number-1,5]) 	
% 			write("Democrat count")
% 			write(rhetoric_list[speech_number-1,6]) 	
% 			write("Republican count")
% 			write(rhetoric_list[speech_number-1,7]) 	
% 			write("Freedom count")
% 			write(rhetoric_list[speech_number-1,8]) 	
% 			write("War count")
% 			write(rhetoric_list[speech_number-1,9]) 	
% 			write("God count")
% 			write(rhetoric_list[speech_number-1,10]) 	
% 			write("God Bless count")
% 			write(rhetoric_list[speech_number-1,11]) 	
% 			write("Christianity count")
% 			write(rhetoric_list[speech_number-1,12]) 	
% 			cep()
% 			write(hreadable[speech_number-1])
% 			cep()
% 		}
% 	}
% }
% 
% #Here we pre-allocate some space for
% #vectors we will need
% year_vec <- vector('numeric',count)
% laugh_vec <- vector('numeric',count)
% applause_vec <- vector('numeric',count)
% avg_word <- vector('numeric',count)
% avg_sent <- vector('numeric',count)
% df1 <- vector('numeric',count)
% df2 <- vector('numeric',count)
% df3 <- vector('numeric',count)
% df4 <- vector('numeric',count)
% df5 <- vector('numeric',count)
% df6 <- vector('numeric',count)
% df7 <- vector('numeric',count)
% df8 <- vector('numeric',count)
% df9 <- vector('numeric',count)
% df10 <- vector('numeric',count)
% df11 <- vector('numeric',count)
% df12 <- vector('numeric',count)
% #We now manually define the vectors indexing
% #the speeches by party
% dem <- c(1,2,3,4,5,6,15,16,17,18,19,20,21,
% 22,35,36,37,38,39,40,41,53,54,55,56,57,58,
% 59,60,61,72,73,74,75,76,77,78,79,80,81,82,
% 83,84,85,86,87,88,89,90,91,92)
% rep <- c(7,8,9,10,11,12,13,14,23,24,25,26,
% 27,28,29,30,31,32,33,34,42,43,44,45,46,47,
% 48,49,50,51,52,62,63,64,65,66,67,68,69,70,
% 71)
% #This for loop is going to, with brute force
% #create some numeric vectors that we need for
% #plotting
% for(i in 1:count)
% {
% 	year_vec[i] <- as.numeric(
% 		listed_data[i,3])
% 	laugh_vec[i] <- as.numeric(
% 		emotion_store[i,2])
% 	applause_vec[i] <- as.numeric(
% 		emotion_store[i,3])
% 	avg_word[i] <- mean(nchar(
% 		word_vectors[[i]]))
% 	avg_sent[i] <- mean(nchar(
% 		sentence_vectors[[
% 		i]]))
% 	df1[i] <- rhetoric_list[i,1]
% 	df2[i] <- rhetoric_list[i,2]
% 	df3[i] <- rhetoric_list[i,3]
% 	df4[i] <- rhetoric_list[i,4]
% 	df5[i] <- rhetoric_list[i,5]
% 	df6[i] <- rhetoric_list[i,6]
% 	df7[i] <- rhetoric_list[i,7]
% 	df8[i] <- rhetoric_list[i,8]
% 	df9[i] <- rhetoric_list[i,9]
% 	df10[i] <- rhetoric_list[i,10]
% 	df11[i] <- rhetoric_list[i,11]
% 	df12[i] <- rhetoric_list[i,12]
% }
% 
% #Here we direct R to make a pdf in
% #which we will store our plots
% pdf('ps3plots.pdf')
% plot(year_vec,laugh_vec,xlab='Year',ylab='Laughter Count',
% 	main='Laugh Count per Address')
% plot(year_vec,applause_vec,xlab='Year',ylab='Applause Count',
% 	main='Applause Count per Address')
% plot(year_vec,word_counts,xlab='Year',ylab='Word Count',
% 	main='Words per Address')
% plot(year_vec,sentence_counts,xlab='Year',ylab='Sentence Count',
% 	main='Sentences per Address')
% plot(year_vec,avg_word,xlab='Year',ylab='Average Word Length',
% 	main='Average Word Length per Address')
% plot(year_vec,avg_sent,xlab='Year',ylab='Aveage Sentence Length'
% 	,main='Average Sentence Length per Address')
% plot(year_vec,df1,xlab='Year',ylab='Occurance of "I"',
% 	main='Occurance of "I" per Address')
% plot(year_vec,df2,xlab='Year',ylab='Occurance of "we"',
% 	main='Occurance of "we" per Address')
% plot(year_vec,df3,xlab='Year',ylab='Occurance of "America{,n}"',
% 	main='Occurance of "America{,n}" per Address')
% plot(year_vec,df4,xlab='Year',ylab='Occurance of "democra{cy,tic}"',
% 	main='Occurance of "democra{cy,tic}" per Address')
% plot(year_vec,df5,xlab='Year',ylab='Occurance of "republic"',
% 	main='Occurance of "republic" per Address')
% plot(year_vec,df6,xlab='Year',ylab='Occurance of "Democrat{,ic}"',
% 	main='Occurance of "Democrat{,ic}" per Address')
% plot(year_vec,df7,xlab='Year',ylab='Occurance of "Republican"',
% 	main='Occurance of "Republican" per Address')
% plot(year_vec,df8,xlab='Year',ylab='Occurance of "free{,dom}"',
% 	main='Occurance of "free{,dom}" per Address')
% plot(year_vec,df9,xlab='Year',ylab='Occurance of "war"',
% 	main='Occurance of "war" per Address')
% plot(year_vec,df10,xlab='Year',ylab='Occurance of "God"',
% 	main='Occurance of "God" per Address')
% plot(year_vec,df11,xlab='Year',ylab='Occurance of "God Bless"',
% 	main='Occurance of "God Bless" per Address')
% plot(year_vec,df12,xlab='Year',ylab='Occurance of Christian Themes',
% 	main='Occurance of Christian Themes per Address')
% plot(year_vec[dem],laugh_vec[dem],xlab='Year',ylab='Laughter Count',
% 	main='Laugh Count per Democratic Address')
% plot(year_vec[dem],applause_vec[dem],xlab='Year',ylab='Applause Count',
% 	main='Applause Count per Democratic Address')
% plot(year_vec[dem],word_counts[dem],xlab='Year',ylab='Word Count',
% 	main='Words per Democratic Address')
% plot(year_vec[dem],sentence_counts[dem],xlab='Year',ylab='Sentence Count',
% 	main='Sentences per Democractic Address')
% plot(year_vec[dem],avg_word[dem],xlab='Year',ylab='Average Word Length',
% 	main='Average Word Length per Democratic Address')
% plot(year_vec[dem],avg_sent[dem],xlab='Year',ylab='Aveage Sentence Length'
% 	,main='Average Sentence Length per Democractic Address')
% plot(year_vec[dem],df1[dem],xlab='Year',ylab='Occurance of "I"',
% 	main='Occurance of "I" per Democractic Address')
% plot(year_vec[dem],df2[dem],xlab='Year',ylab='Occurance of "we"',
% 	main='Occurance of "we" per Democratic Address')
% plot(year_vec[dem],df3[dem],xlab='Year',ylab='Occurance of "America{,n}"',
% 	main='Occurance of "America{,n}" per Democratic Address')
% plot(year_vec[dem],df4[dem],xlab='Year',ylab='Occurance of "democra{cy,tic}"',
% 	main='Occurance of "democra{cy,tic}" per Democratic Address')
% plot(year_vec[dem],df5[dem],xlab='Year',ylab='Occurance of "republic"',
% 	main='Occurance of "republic" per Democractic Address')
% plot(year_vec[dem],df6[dem],xlab='Year',ylab='Occurance of "Democrat{,ic}"',
% 	main='Occurance of "Democrat{,ic}" per Democratic Address')
% plot(year_vec[dem],df7[dem],xlab='Year',ylab='Occurance of "Republican"',
% 	main='Occurance of "Republican" per Democratic Address')
% plot(year_vec[dem],df8[dem],xlab='Year',ylab='Occurance of "free{,dom}"',
% 	main='Occurance of "free{,dom}" per Democratic Address')
% plot(year_vec[dem],df9[dem],xlab='Year',ylab='Occurance of "war"',
% 	main='Occurance of "war" per Democractic Address')
% plot(year_vec[dem],df10[dem],xlab='Year',ylab='Occurance of "God"',
% 	main='Occurance of "God" per Democratic Address')
% plot(year_vec[dem],df11[dem],xlab='Year',ylab='Occurance of "God Bless"',
% 	main='Occurance of "God Bless" per Democractic Address')
% plot(year_vec[dem],df12[dem],xlab='Year',ylab='Occurance of Christian Themes',
% 	main='Occurance of Christian Themes per Democratic Address')
% plot(year_vec[rep],laugh_vec[rep],xlab='Year',ylab='Laughter Count',
% 	main='Laugh Count per Republican Address')
% plot(year_vec[rep],applause_vec[rep],xlab='Year',ylab='Applause Count',
% 	main='Applause Count per Republican Address')
% plot(year_vec[rep],word_counts[rep],xlab='Year',ylab='Word Count',
% 	main='Words per Republican Address')
% plot(year_vec[rep],sentence_counts[rep],xlab='Year',ylab='Sentence Count',
% 	main='Sentences per Republican Address')
% plot(year_vec[rep],avg_word[rep],xlab='Year',ylab='Average Word Length',
% 	main='Average Word Length per Republican Address')
% plot(year_vec[rep],avg_sent[rep],xlab='Year',ylab='Aveage Sentence Length'
% 	,main='Average Sentence Length per Republican Address')
% plot(year_vec[rep],df1[rep],xlab='Year',ylab='Occurance of "I"',
% 	main='Occurance of "I" per Republican Address')
% plot(year_vec[rep],df2[rep],xlab='Year',ylab='Occurance of "we"',
% 	main='Occurance of "we" per Republican Address')
% plot(year_vec[rep],df3[rep],xlab='Year',ylab='Occurance of "America{,n}"',
% 	main='Occurance of "America{,n}" per Republican Address')
% plot(year_vec[rep],df4[rep],xlab='Year',ylab='Occurance of "democra{cy,tic}"',
% 	main='Occurance of "democra{cy,tic}" per Republican Address')
% plot(year_vec[rep],df5[rep],xlab='Year',ylab='Occurance of "republic"',
% 	main='Occurance of "republic" per Republican Address')
% plot(year_vec[rep],df6[rep],xlab='Year',ylab='Occurance of "Democrat{,ic}"',
% 	main='Occurance of "Democrat{,ic}" per Republican Address')
% plot(year_vec[rep],df7[rep],xlab='Year',ylab='Occurance of "Republican"',
% 	main='Occurance of "Republican" per Republican Address')
% plot(year_vec[rep],df8[rep],xlab='Year',ylab='Occurance of "free{,dom}"',
% 	main='Occurance of "free{,dom}" per Republican Address')
% plot(year_vec[rep],df9[rep],xlab='Year',ylab='Occurance of "war"',
% 	main='Occurance of "war" per Republican Address')
% plot(year_vec[rep],df10[rep],xlab='Year',ylab='Occurance of "God"',
% 	main='Occurance of "God" per Republican Address')
% plot(year_vec[rep],df11[rep],xlab='Year',ylab='Occurance of "God Bless"',
% 	main='Occurance of "God Bless" per Republican Address')
% plot(year_vec[rep],df12[rep],xlab='Year',ylab='Occurance of Christian Themes',
% 	main='Occurance of Christian Themes per Republican Address')
% dev.off()
% 	
% #These lines simply tell us that we have finished
% cat("END EXECUTION\n")
% cat("*************************************************\n")
