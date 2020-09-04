# chapter 2

tmp_lib <- "C:/Users/aleks/R/rethinking_library"
# explicitly load the affected packages from the temporary library
library(StanHeaders, lib.loc = tmp_lib)
library(rethinking, lib.loc = tmp_lib)
library(ggplot2)



####################################################################################
# MCMC in action with Globe tossing
####################################################################################
mcmc.globe.toss <- function(W, L, n_samples=1000){
  
  p <-rep(NA, n_samples)
  p[1] <-0.5
  for (i in 2:n_samples){
    p_new <-rnorm(1,p[i-1],0.1)
    if (p_new<0) p_new <- abs(p_new)
    if (p_new>1) p_new <- 2-p_new
    q0 <-dbinom(W, W+L, p[i-1])
    q1 <-dbinom(W, W+L, p_new)
    p[i] <-ifelse(runif(1)<q1/q0,p_new,p[i-1])
  }
  
  dens(p, xlim=c(0,1))
  curve(dbeta(x,W+1,L+1),lty=2,add=TRUE)
}  
par(mfrow=c(4, 4))
mcmc.globe.toss(8, 15, 5)

####################################################################################
# Chapter 2 practice questions
####################################################################################
# 2E1
# (2) Pr(rain|Monday)
# because Pr(rain|Monday)

# 2E2
# (3) TheprobabilitythatitisMonday,giventhatitisraining.
# Pr(Monday|rain) = The probability that it is Monday, given that it is raining.

# 2E3
# the probability that it is Monday, given that it is raining?
# (1) Pr(Monday|rain)
# (4) Pr(rain|Monday) Pr(Monday)/ Pr(rain)

# 2E4
# What doesitmeantosay“the probability of water is 0.7”?
# given that we cannot deterministically model flight of globe in air, with all its twists and turns,
# we assign a fixed probability or expectation to our finger landing on water based on 
# how much water there is on the globe to land on. However this "hasn't happened", its speculation.
# could go 1 level deeper: assign a probability distribution based on 

# 2M1
plot.2m1.grid.approximation <- function(inp, npoints) {
  # define grid, necessary in and out of the function
  p_grid <-seq(from=0,to=1, length.out=npoints)
  
  # define priors
  prior <-rep(1,20)  # constant
  # prior <-exp(-5*abs(p_grid-0.5))  # laplace double exponent
  # prior <- ifelse(p_grid<0.5,0,1)  # 0 below 0.5
  
  # compute likelihood at each value in grid
  likelihood <- dbinom(inp[["W"]], size=sum(unlist(inp)), prob=p_grid)
  
  # compute product of likelihood and prior
  unstd.posterior <- likelihood*prior
  
  # standardize the posterior, so it sums to 1
  posterior <- unstd.posterior/sum(unstd.posterior)
  plot( p_grid,posterior,type="b",
        xlab="probability ofwater",ylab="posteriorprobability")
  
  # add plot title
  mtext(paste(c(npoints, "points"), collapse = " "))
}

q2m1.1 <- list(W=3, L=0)
q2m1.2 <- list(W=3, L=1)
q2m1.3 <- list(W=5, L=2)

plot.2m1.grid.approximation(q2m1.1, 100)
plot.2m1.grid.approximation(q2m1.2, 100)
plot.2m1.grid.approximation(q2m1.3, 100)

# 2M2 Nowassumeapriorfor p thatisequaltozerowhen p < 0.5 andisapositiveconstantwhen p ≥ 0.5.
plot.2m2.grid.approximation <- function(inp, npoints) {
  # define grid, necessary in and out of the function
  p_grid <-seq(from=0,to=1, length.out=npoints)
  
  # define priors
  prior <-exp(-5*abs(p_grid-0.5))  # laplace double exponent
  # prior <- ifelse(p_grid<0.5,0,1)  # 0 below 0.5
  
  # compute likelihood at each value in grid
  likelihood <- dbinom(inp[["W"]], size=sum(unlist(inp)), prob=p_grid)
  
  # compute product of likelihood and prior
  unstd.posterior <- likelihood*prior
  
  # standardize the posterior, so it sums to 1
  posterior <- unstd.posterior/sum(unstd.posterior)
  plot( p_grid,posterior,type="b",
        xlab="probability ofwater",ylab="posteriorprobability")
  
  # add plot title
  mtext(paste(c(npoints, "points"), collapse = " "))
}
par(mfrow=c(3, 1))
plot.2m2.grid.approximation(q2m1.1, 100)
plot.2m2.grid.approximation(q2m1.2, 100)
plot.2m2.grid.approximation(q2m1.3, 100)

# 2M3 - 2 globes, Earth and Mars
# P(L|E) * P(E) / P(L) = 
(0.3) * .5 / (.3*.5 + 1*.5)

# 2M4 - 
# B/B, B/W, W/W
# ways could produce black card facing up: 2, 1, 0
# ways that given black facing up, other side also black: 2, 0, 0
# so, 2/3
# or by bayes rule
(1 * 1/3) / (1/2)

# 2M5 - 
(.8 * 2/3) / (5/8)

# 2M6 - 
(.5 * 1/6) / (2/6)

# 2M7 - 


# 2H1 - 



# 2H2 - 
p.A.given.Twins <- (1/3)


# 2H3 - 
# TODO - probability seems too low... but seemingly 'correct' that multiplied previous by new
# ORDERING of births unimportant... maybe that?
p.A.given.Twins.then.Singleton <- p.A.given.Twins * ((9/10) / (17/10))


# 2H4 - 
# a) - First ignore your previous information from the births and compute the posterior probability that your panda is species A. Then redo your calculation, now using the birth data as well.
p.A.given.positive.test <- (.4 / (.4 + .325))


# b)
p.A.given.positive.test * p.A.given.Twins.then.Singleton
