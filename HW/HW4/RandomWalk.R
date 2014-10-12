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
Priase_Be_To_The_RNG = function(n)
{
#These are the possible x steps we can take
	xsteps <- c(-1,1,0,0)
#These are the possible y steps we can take
	ysteps <- c(0,0,-1,1)
#This vector represents the steps (or their correlation
# to our steps vectors) that we will take
	step <- sample(1:4,n-1,replace=T)
#These next two lines will create vectors of the new
# x and y position at each step
	posx <- c(0,CumSum(xsteps[step]))
	posy <- c(0,CumSum(ystep[step]))
# This 

cat('END EXECUTION \n')
cep()
