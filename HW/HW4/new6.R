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
cat('BEGIN EXECUTION!!!')
load("ps4prob6.Rda") 

ll <- function(Theta, A) {
  sum.ind <- which(A==1, arr.ind=T)
  logLik <- sum(log(Theta[sum.ind])) - sum(Theta)
  return(logLik)
}

oneUpdate <- function(A, n, K, theta.old, thresh = 0.1) { 
  theta.old1 <- theta.old
  Theta.old <- theta.old %*% t(theta.old)
  L.old <- ll(Theta.old, A)
  q <- array(0, dim = c(n, n, K))
#We move this statment up here so we can condense the two z loops
  theta.new <- theta.old
#We need these two statements to elimate two of the loops and
# vectorize using outer 
	i <- c(1:n)
	j <- c(1:n) 
      for (z in 1:K) 
	{
		q[,,z] <- outer(theta.old[i,z],theta.old[j,z])/Theta.old
#Additionally we move the other z loop inside
    		theta.new[,z] <- rowSums(A*q[,,z])/sqrt(sum(A*q[,,z]))
	}
  Theta.new <- theta.new %*% t(theta.new)
  L.new <- ll(Theta.new, A)
      converge.check <- abs(L.new - L.old) < thresh
  theta.new <- theta.new/rowSums(theta.new)
  return(list(theta = theta.new, loglik = L.new,
              converged = converge.check)) 
}

# initialize the parameters at random starting values
temp <- matrix(runif(n*K), n, K)
theta.init <- temp/rowSums(temp)

# do single update
system.time(
{
out <- oneUpdate(A,n,K,theta.init)
})

cat('END EXECUTION!!')
cep()
