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
cat('BEGIN EXECUTION!!!\n')
load("data.Rda") 

#Go ahead and transpose x cause we need to
X_trans <- t(X)

#Initalize p as its good practice
p <- vector('double',length(X))

#Solve for p the naieve way
p = (X_trans*beta)/(1+X_trans*beta)

#Solve for the liklihood
likelihood <- prod(dbinom(y,n,p))

#Print out for checking
cep()
cat(length(p))
cat('\n')
cep()
cat('END EXECUTION!!')
cep()
