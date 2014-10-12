#!/usr/bin/Rscript
#Creator: Daniel Wooten
#Class STATS243
#PS4

# A function to make prettier output
cep <- function()
{
	cat('********************************************************** \n')
}

cep()
cat('BEGIN EXECUTION \n')

#Initialize the random walk function
Priase_Be_To_The_RNG = function(n,WholeWalk=NULL)
{
#Try to cast inpput as integer
	N <- try(as.integer(n))
#If input can be cast as integer proceed
	if(!class(N) == 'try-error')
	{
#Check to make sure N is actually an integer
		if(!is.integer(N) | n <= 0 | is.na(N) )
		{
#If N is not an integer, bork and exit with message
			cat('ERROR!!: First argument n must
				be of type positive and 
				non-zero integer')
			return()
		}
	}
	else
	{
		cat('ERROR!!: Step-number input could
			not be case as integer.')
		return()
	}
#Check if WholeWalk was given
	if(is.null(WholeWalk))
	{
#If whole walk was not give, set default vale of no walk
#output
		WholeWalk=0
	}
#If wholewalk was given, check for correct value. If not,
# fail.
	else
	{
		if(WholeWalk != 0 | WholeWalk != 1)
		{
			cat('ERROR!!: Input to select
				output of entire
				walk must be of form
				0 (no walk) or
				1 (whole walk)')
			return()
		}
	}
#These are the possible x steps we can take
	xsteps <- c(-1,1,0,0)
#These are the possible y steps we can take
	ysteps <- c(0,0,-1,1)
#This vector represents the steps (or their correlation
# to our steps vectors) that we will take
	step <- sample(1:4,n-1,replace=T)
#This if statement will handle the two cases
#whole walk, or just the final position
	if(
#These next two lines will create vectors of the new
# x and y position at each step
	posx <- c(0,CumSum(xsteps[step]))
	posy <- c(0,CumSum(ystep[step]))

cat('END EXECUTION \n')
cep()
