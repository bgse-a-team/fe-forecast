rm(list=ls())
n <- 20
p <- 5
bench <- rnorm(n)
models <- matrix(rnorm(n*p),nrow=n,ncol=p)

# define function to do circular block bootstrapping and get B samples with block size w
block_bootstrap <- function(data, B, w) {
  t <- length(data)
  k <- 1
  # number of blocks needed
  s <- ceiling(t/w)
  # get random starting points
  Bs <- ceiling(matrix(runif(s*B)*t,nrow = s,ncol = B))
  indices <- matrix(nrow=(s*w),ncol=B)
  index <- 1
  # adder gets added each loop
  adder <- matrix(rep(c(0:(w-1)),B),nrow = B,byrow = T)
  for (i in seq(1,t,w)) {
    indices[c(i:(i+w-1)),] <- t(matrix(rep(Bs[index,],w), nrow = B, ncol = w)+adder)
    index <- index+1
  }
  # fix to get circular indices
  indices <- indices[c(1:t),]
  indices[indices>t] <- indices[indices>t]-t
  # get corresponding data values
  bsdata <- matrix(data[indices],nrow = t,ncol = B)
  return(bsdata)
}

# define function to do stationary block bootstrapping and get B samples with average block size w
stat_bootstrap <- function(data, B, w) {
  t <- length(data)
  k <- 1
  # define the probability of a new block
  p <- 1/w
  # get random starting points
  indices <- matrix(nrow=t,ncol=B)
  indices[1,] <- ceiling(t*runif(B))
  # set the random numbers
  select <- matrix(runif(t*B)<p,nrow = t,ncol = B)
  indices[select]=ceiling(runif(sum(select))*t)
  for (i in (2:t)) {
    # determine whether we stay (rand>p) or move to a new starting value
    # (rand<p)
    indices[i,!select[i,]] <- indices[i-1,!select[i,]]+1
  }
  # fix to get circular indices
  indices[indices>t] <- indices[indices>t]-t
  # get corresponding data values
  bsdata <- matrix(data[indices],nrow = t,ncol = B)
  return(bsdata)
}