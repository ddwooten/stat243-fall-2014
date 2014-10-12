#!/usr/bin/Rscript
#Creator: Daniel Wooten
#Class STATS243
#PS4

# A function to make prettier output
cep <- function(){
	cat('********************************************************** \n')
}
cep()
cat('BEGIN EXECUTION \n')

cat('The Data Frame \n')

v1 <- c(1,2,3)
v2 <- c(4,5,6)
v3 <- c(7,8,9)
df1 <- data.frame(matrix(nrow=3,ncol=3))
df1[1] <- v1
df1[2] <- v2
df1[3] <- v3

cep()
print(df1)
cep()
cat('The output of .Internal(inspect(df1)) \n')
cep()
.Internal(inspect(df1))
cep()
df1[1,1] <- 10
cep()
cat('The output of .Internal(inspect(df1)) after modificaiton \n')
cep()
.Internal(inspect(df1))
cep()
cat('The output of .Internal(inspect(df1[1])) without other interference \n')
.Internal(inspect(df1[1]))
cep()
cat('END EXECUTION \n')
cep()
