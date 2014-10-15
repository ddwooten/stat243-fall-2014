% #!/usr/bin/Rscript
% #Creator: Daniel Wooten
% #Class STATS243
% #PS4
% 
% # A function to make prettier output
% cep <- function()
% {
% 	cat('********************************************************** \n')
% }
% 
% cep()
% cat('BEGIN EXECUTION \n')
% 
% #Collect the function arguments
% cat('Enter the number of steps: \n')
% num <- readLines(file("stdin"),1)
% cat('Enter 1 for whole walk or 0 for just the end position\n')
% YesWalk <- readLines(file('stdin'),1)
% 
% #Initialize the random walk function
% Praise_Be_To_The_RNG <- function(N,WholeWalk=NULL,SX=NULL,SY=NULL)
% {
% #Try to cast inpput as integer
% 	n <- try(as.integer(N))
% #If input can be cast as integer proceed
% 	{
% 	if(!class(n) == 'try-error')
% 	{
% #Check to make sure N is actually an integer
% 		if(!is.integer(n) | n <= 0 | is.na(n) )
% 		{
% #If N is not an integer, bork and exit with message
% 			cat('ERROR!!: First argument n must
% 				be of type positive and 
% 				non-zero integer\n')
% 			exit_code <- '# steps not positive integer'
% 			return(exit_code)
% 		}
% 	}
% 	else
% 	{
% 		cat('ERROR!!: Step-number input could
% 			not be cast as integer.\n')
% 		exit_code <- 'Step input unable to cast as integer'
% 		return(exit_code)
% 	}
% 	}
% #Check if WholeWalk was given
% 	{
% 	if(is.null(WholeWalk))
% 	{
% #If whole walk was not give, set default vale of no walk
% #output
% 		WholeWalk=0
% 	}
% #If wholewalk was given, check for correct value. If not,
% # fail.
% 	else
% 	{
% # Convert WholeWalk to integer just to be sure it passes check
% 		WholeWalk <- try(as.integer(WholeWalk))
% #If WholeWalk is not needed input, error and bork
% 		{
% 		if(!class(WholeWalk) == 'try-error')
% 		{	
% 			if(WholeWalk != 0 & WholeWalk != 1)
% 			{
% 				cat('ERROR!!: Input to select
% 					output of entire
% 					walk must be of form
% 					0 (no walk) or
% 					1 (whole walk)\n')
% 				exit_code <- 'Walk select not 0 or 1 or blank'
% 				return(exit_code)
% 			}
% 		}
% 		else
% 		{
% 			cat('ERROR!!: Input for walk-output form
% 				could not be cast as a 0 or 1 
% 				integer (required form)')
% 			exit_code <- 'Walk select could not be cast as 0 or 1'
% 			return(exit_code)
% 		}
% 		}	
% 	}
% 	}
% #These are the possible x steps we can take
% 	xsteps <- c(-1,1,0,0)
% #These are the possible y steps we can take
% 	ysteps <- c(0,0,-1,1)
% #This vector represents the steps (or their correlation
% # to our steps vectors) that we will take
% 	steps <- sample(1:4,n,replace=T)
% #Start positions for the x and y coords
% 	sx <- 0
% 	sy <- 0
% # This if statement lets us reset the start if we'd like
% 	if(!is.null(SX)) sx <- SX
% 	if(!is.null(SY)) sy <- SY
% #These next two lines will create vectors of the new
% # x and y position at each step
% 	posx <- c(0,cumsum(xsteps[steps]))
% 	posy <- c(0,cumsum(ysteps[steps]))
% #Shift the steps taken by their start value
% 	shifted_xsteps <- posx + sx
% 	shifted_ysteps <- posy + sy
% #This if statement will handle the two cases
% #whole walk, or just the final position
% 	{
% 	if(WholeWalk == 1)
% 	{
% 		walk_map <- matrix(ncol = n + 1, nrow = 2 )
% 		walk_map[1,] <- shifted_xsteps 
% 		walk_map[2,] <- shifted_ysteps 
% 	}
% 	else
% 	{
% 		walk_map <- vector('numeric',2)
% 		walk_map[1] <- shifted_xsteps[n+1]
% 		walk_map[2] <- shifted_ysteps[n+1]
% 	}
% 	}
% 	return(walk_map)
% }
% 
% # Call the function
% RND_Walk <- Praise_Be_To_The_RNG(num,YesWalk)
% # Print the result based on the output
% cep()
% cat('The result\n')
% cep()
% {
% if(is.matrix(RND_Walk))
% {
% 	cat('X steps: ')
% 	cat(RND_Walk[1,])
% 	cat('\n')
% 	cat('Y steps: ')
% 	cat(RND_Walk[2,])
% 	cat('\n')
% 	cep()
% }
% else
% {
% 
% 	cat('Final position (x,y): ')
% 	cat(RND_Walk[1])
% 	cat(',')
% 	cat(RND_Walk[2])
% 	cat('\n')
% 	cep()
% }
% }
% cat('\n')
% cep()
% 		
% cat('END EXECUTION \n')
% cep()
