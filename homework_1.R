# homework 1
# https://github.com/rmcelreath/statrethinking_winter2019/blob/master/homework/week01.pdf

tmp_lib <- "C:/Users/aleks/R/rethinking_library"
# explicitly load the affected packages from the temporary library
library(StanHeaders, lib.loc = tmp_lib)
library(rethinking, lib.loc = tmp_lib)

# 1. Suppose the globe tossing data had turned out to be 8 water in 15 tosses.
# Construct the posterior distribution, using grid approximation. Use the
# same flat prior as before.
incremental.update.plot <- function(inp.prior, inp.samples) {
  
  ncols <- 3
  par(mfrow=c(length(inp.samples) %/% ncols,ncols))
  for (idx in 1:length(inp.samples)) {
    plot(p, inp.prior, type ="l", ylim=c(0, 3), ylab = "plausibility", xlab="proportion water", lty=2)  # add the prior
    
    obsvs <- head(inp.samples, idx)
    posterior <- dbeta(p, length(obsvs), sum(obsvs)) * prior
    lines(p, posterior, type ="l", lty=1)  # add the posterior
    inp.prior <- posterior  # update the prior
  }
  posterior
}



plot.grid.approximation <- function(W, N, npoints) {
  # define grid, necessary in and out of the function
  p_grid <-seq(from=0,to=1, length.out=npoints)
  
  # define priors
  # prior <-rep(1,20)  # constant
  # prior <-exp(-5*abs(p_grid-0.5))  # laplace double exponent
  prior <- ifelse(p_grid<0.5,0,1)  # 0 below 0.5
  
  # compute likelihood at each value in grid
  likelihood <- dbinom(W, size=N, prob=p_grid)
  
  # compute product of likelihood and prior
  unstd.posterior <-likelihood*prior
  
  # standardize the posterior, so it sums to 1
  posterior <- unstd.posterior/sum(unstd.posterior)
  plot( p_grid,posterior,type="b",
        xlab="probability ofwater",ylab="posteriorprobability")
  
  # add plot title
  mtext(paste(c(npoints, "points"), collapse = " "))
}


plot.grid.approximation(6, 9, 100)

# relative decline in the rate of Water, so median posterior 
# more variable so posterior is wider and more squat
6/9
8/15
plot.grid.approximation(8, 15, 100)

globe.qa <-quap(
  alist(
    W ~ dbinom(W+L,p),#binomiallikelihood
    p ~ dunif(0,1)#uniformprior
  ) ,
  data=list(W=8,L=7) )
# displaysummaryofquadraticapproximation
precis(globe.qa)


W <-6
L <-3
curve( dbeta(x,W+1,L+1),from=0,to=1)

# 2. Start over in 1, but now use a prior that is zero below p = 0:5 and a constant
# above p = 0:5. This corresponds to prior information that a majority
# of the Earth’s surface is water. What difference does the better prior make?
# If it helps, compare posterior distributions (using both priors) to the true
# value p = 0:7.

# "embodying assumption" in prior: that world is > 1/2 water.

plot.grid.approximation(8, 15, 100)

# "comparing": checking the intervals and spreads around the posterior max point
HDPI()

# 3. This problem is more open-ended than the others. Feel free to collaborate
# on the solution. Suppose you want to estimate the Earth’s proportion of
# water very precisely. Specifically, you want the 99% percentile interval of the
# posterior distribution of p to be only 0.05 wide. This means the distance between
# the upper and lower bound of the interval should be 0.05. How many
# times will you have to toss the globe to do this? I won’t require a precise
# answer. I’m honestly more interested in your approach.

# approach: repeated random simulation or sampling?
