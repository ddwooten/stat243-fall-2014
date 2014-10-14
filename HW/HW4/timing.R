#!/usr/bin/Rscript
#Creator: Daniel Wooten
#Class STATS243
#PS4

cat('*********************************************************** \n')
cat('BEGIN EXECUTION \n')

size <- 10000
boolvec <- rep(c(F,T,F,T),500000)
ivec <- vector('numeric',size)
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

for(j in seq(0,size,1000) )
{
	if( j == 0 ){
		i = 1
	}
	else{ i = j }	
	cat(c(j,i))
	cat('\n')
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

	ivec[i] <- length	
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
}

pdf('ps4plots.pdf')
plot(ivec,itimes1,xlab='Vector Size',ylab='Elapsed Time (s)', 
	main='Time to Subset a Vector with Indicies',col=1)
points(ivec,itimes2,col=2,pch=18)
points(ivec,itimes3,col=3,pch=18)
points(ivec,itimes4,col=4,pch=18)
points(ivec,itimes5,col=5,pch=18)
points(ivec,btimes1,col=6,pch=18)
points(ivec,btimes2,col=7,pch=18)
points(ivec,btimes3,col=8,pch=18)
points(ivec,btimes4,col=9,pch=18)
points(ivec,btimes5,col=10,pch=18)
legend("topleft",c("Index 10% of Vector","Index 20% of Vector",
	"Index 30% of Vector","Index 40% of Vector","Index 50% of Vector",
	"Bool 10% of Vector", "Bool 20% of Vector","Bool 30% of Vector",
	"Bool 40% of Vector","Bool 50% of Vector"),
	pch=18, col=c(1,2,3,4,5,6,7,8,9,10))
graphics.off()
cat(length(itimes1))
cat('\n')
cat(length(itimes2))
cat('\n')

cat('END EXECUTION \n')
cat('********************************************************* \n')
