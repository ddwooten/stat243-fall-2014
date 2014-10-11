#!/usr/bin/Rscript
#Creator: Daniel Wooten
#Class STATS243
#PS4

size <- 20
boolvec <- rep(c(F,T,F,T),500000)
ivec <- seq(100,size*100,100)
itimes1 <- vector('numeric',size)
itimes2 <- vector('numeric',size)
itimes3 <- vector('numeric',size)
itimes4 <- vector('numeric',size)
itimes5 <- vector('numeric',size)
btimes1 <- vector('numeric',size)
btimes2 <- vector('numeric',size)
btimes3 <- vector('numeric',size)
btimes4 <- vector('numeric',size)
btimes5 <- vector('numeric',size)

for(i in seq(1,size,1) )
{
	length <- as.integer(i*100)
	s1 <- as.integer(i*10)
	s2 <- as.integer(i*20)
	s3 <- as.integer(i*30)
	s4 <- as.integer(i*40)
	s5 <- as.integer(i*50)
	vec <- rnorm(length)
	bol1 <- sample(boolvec,s1,replace=T)
	bol2 <- sample(boolvec,s2,replace=T)
	bol3 <- sample(boolvec,s3,replace=T)
	bol4 <- sample(boolvec,s4,replace=T)
	bol5 <- sample(boolvec,s5,replace=T)
	int1 <- sample(1:length,s1,replace=F)
	int2 <- sample(1:length,s2,replace=F)
	int3 <- sample(1:length,s3,replace=F)
	int4 <- sample(1:length,s4,replace=F)
	int5 <- sample(1:length,s5,replace=F)
	
	itimes1[i] <- system.time(vec[int1])[3] 
	itimes2[i] <- system.time(vec[int2])[3]
	itimes3[i] <- system.time(vec[int3])[3]
	itimes4[i] <- system.time(vec[int4])[3]
	itimes5[i] <- system.time(vec[int5])[3]
	btimes1[i] <- system.time(vec[bol1])[3]
	btimes2[i] <- system.time(vec[bol1])[3]
	btimes3[i] <- system.time(vec[bol1])[3]
	btimes4[i] <- system.time(vec[bol1])[3]
	btimes5[i] <- system.time(vec[bol1])[3]

		
