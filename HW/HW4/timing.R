#!/usr/bin/Rscript
#Creator: Daniel Wooten
#Class STATS243
#PS4

cat('*********************************************************** \n')
cat('BEGIN EXECUTION \n')

size <- 100000
boolvec <- rep(c(F,T,F,T),500000)
ivec <- vector('numeric',size)
itimes1 <- vector('numeric',size)
itimes3 <- vector('numeric',size)
itimes5 <- vector('numeric',size)
btimes1 <- vector('numeric',size)
btimes3 <- vector('numeric',size)
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
	s3 <- as.integer(i*30)
	s5 <- as.integer(i*50)
	vec <- rnorm(length)
	bol1 <- sample(boolvec,s1,replace=T)
	bol3 <- sample(boolvec,s3,replace=T)
	bol5 <- sample(boolvec,s5,replace=T)
	int1 <- sample(1:length,s1,replace=F)
	int3 <- sample(1:length,s3,replace=F)
	int5 <- sample(1:length,s5,replace=F)

	ivec[i] <- length	
	itimes1[i] <- system.time(vec[int1])[3] 
	itimes3[i] <- system.time(vec[int3])[3]
	itimes5[i] <- system.time(vec[int5])[3]
	btimes1[i] <- system.time(vec[bol1])[3]
	btimes3[i] <- system.time(vec[bol1])[3]
	btimes5[i] <- system.time(vec[bol1])[3]
}

pdf('ps4plots.pdf')
plot(ivec,btimes5,xlab='Vector Size',ylab='Elapsed Time (s)', 
	main='Time to Subset a Vector with Indicies',col=1)
points(ivec,itimes3,col=2,pch=18)
points(ivec,itimes5,col=3,pch=18)
points(ivec,btimes1,col=4,pch=18)
points(ivec,btimes3,col=5,pch=18)
points(ivec,itimes1,col=6,pch=18)
legend("topleft",c("Bool 50% of Vector",
	"Index 30% of Vector","Index 50% of Vector",
	"Bool 10% of Vector", "Bool 30% of Vector",
	"Index 10% of Vector"),
	pch=c(16,18,18,18,18,18), col=c(1,2,3,4,5,6))
graphics.off()
cat('\n')

cat('END EXECUTION \n')
cat('********************************************************* \n')
