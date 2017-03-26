source('bootstrap_funs.R')
spa <- function(bench, models, B, w, type='STUDENTIZED', boot='STATIONARY') {
  # benchmark dimensions
  tb <- length(bench)
  kb <- 1
  # models dimensions
  t <- dim(models)[1]
  k <- dim(models)[2]
  # matrix to store bootstrapped samples
  bsdata <- matrix(nrow = t, ncol = B)
  if (boot == 'BLOCK') {
    bsdata <- block_bootstrap(c(1:t),B,w)
  }
  else {
    bsdata <- stat_bootstrap(c(1:t),B,w)
  }
  #bsdata <- as.matrix(read.csv('testbs.csv',header = F))
  colnames(bsdata) <- NULL
  # OK now we have the bootstraps, what to do with them?
  diffs <- models-matrix(rep(bench),nrow=t,ncol=k)

  # first compute the bootstrap sample averages, db*
  # second compute the variance estimate, omegak
  # first the kernel weights
  q <- 1/w
  i <- c(1:(t-1))
  kappa <- (((t-i)/t)*((1-q)^i))+((i/t)*((1-q)^(t-i)))

  # next compute the variances
  vars <- matrix(nrow=1,ncol=k)
  for (i in 1:k) {
    workdata <- diffs[,i]-mean(diffs[,i])
    vars[i] <- (t(workdata)%*%workdata)/t
    for (j in 1:(t-1)) {
      vars[i] <- vars[i] + 2*kappa[j]*(t(workdata[1:(t-j)])%*%(workdata[(j+1):t]))/t
    }
  }

  # Aold is the original method to compute the truncation point
  Aold <- (1/4)*(t^(0.25))*sqrt(vars/t)
  mean(Aold)
  # Anew uses the log(log(t)) rule
  Anew <- sqrt((vars/t)*2*log(log(t)))
  mean(Anew)

  # Only recenter if the average is reasonably small or the model is better
  # (in which case mean(diffs) is negative).  If it is unreasonably large set
  # the mean adjustment to 0
  gc <- colMeans(diffs)*(mean(diffs)<Anew)

  # The lower assumes that every loss function that is worse than BM is
  # unimportant for the asymptotic distribution, hence if its mean is
  # less than 0, g=0.  This is different from the consistent where the
  # threshold was it had to be greater than -A(i)
  gl <- sapply(colMeans(diffs), function(x) min(x,0))

  # Then the upper, which assumes all models used are reasonably close to
  # the benchmark that they could be better
  gu <- colMeans(diffs)

  # Perf will hold the boostrapped statistics for B iterations
  perfc <- matrix(nrow=B,ncol=k)
  perfl <- matrix(nrow=B,ncol=k)
  perfu <- matrix(nrow=B,ncol=k)
  if (type == "STUDENTIZED") {
    stdDev <- sqrt(vars)
  }
  else {
    stdDev <- matrix(1,nrow=1,ncol=k)
  }

  for (i in 1:k) {
    workdata <- diffs[,i]
    # the i'th column of perf holds the B bootstrapped statistics
    mworkdata <- colMeans(matrix(workdata[bsdata],nrow=t,ncol=B))
    perfc[,i] <- (t(mworkdata-gc[i]))/stdDev[i]
    perfl[,i] <- (t(mworkdata-gl[i]))/stdDev[i]
    perfu[,i] <- (t(mworkdata-gu[i]))/stdDev[i]
  }
  # compute the test statistic
  stat <- min(colMeans(diffs)/stdDev)
  # compute the min in each row
  perfc <- apply(perfc,1,min)
  perfc <- sapply(perfc,min,0)
  perfl <- apply(perfl,1,min)
  perfl <- sapply(perfl,min,0)
  perfu <- apply(perfu,1,min)
  perfu <- sapply(perfu,min,0)
  # count the number of times the min is below the statistic
  c <- mean(perfc<stat)
  l <- mean(perfl<stat)
  u <- mean(perfu<stat)
  return(list(c=c, l=l, u=u))
}