# chapter_1

# installing rethinking package
tmp_lib <- "C:/Users/aleks/R/rethinking_library"
install.packages(c("coda","mvtnorm","devtools","dagitty"), lib = tmp_lib)
install.packages("StanHeaders", lib = tmp_lib)
install.packages("usethis")
install.packages("backports")


library(devtools)
devtools::install_github("rmcelreath/rethinking", lib = tmp_lib)

# explicitly load the affected packages from the temporary library
library(StanHeaders, lib.loc = tmp_lib)
library(rethinking, lib.loc = tmp_lib)


# repeated drawing from bag of marbles WITH REPLACEMENT
# updating the existing estimate: multiply because 

# using other information: 
# blue marbles are rare, but every bag contains at least one
# a "ratio" of bag frequency
# multiply existing prior array by the ratios

choose(4, 2)

# probability theory == NORMALISED COMBINATRICS OR 'COUNTING'.
# relative weighted evidence, counting the possibilities
# just nicer to work with sums between [0, 1]

ways = c(3, 8, 9)
ways/sum(ways)


########################################################################
# Lecture 2
########################################################################
# DESIGN
# throwing and catching a globe, right index finger "lands on water"


# CONDITION
# bayesian updating defines optimal learning in small world, converts prior into posterior 

# recreating the grid of plots as update the prior after repeated trials
p <- seq(0, 1, length=1000)
samples <- c(1, 0, 1, 1, 1, 0, 1, 0, 1)

prior <- rep(1/3, length(p))
# "embodying assumption" in prior: that world is > 1/2 water.
prior <- c(rep(0, length(prior) / 2), tail(prior, length(prior) / 2))

# any weird+wonderful prior you want, laplacian "double exponential distribution"
lap <- seq(-10, 10, length=1000)
prior <- exp(-abs(lap-2))/2
plot(p, prior, type ="l", ylim=c(0, 1), lty=2)

ncols <- 3
par(mfrow=c(length(samples) %/% ncols,ncols))
for (idx in 1:length(samples)) {
  plot(p, prior, type ="l", ylim=c(0, 3), ylab = "plausibility", xlab="proportion water", lty=2)  # add the prior
  
  obsvs <- head(samples, idx)
  posterior <- dbeta(p, length(obsvs), sum(obsvs)) * prior
  lines(p, posterior, type ="l", lty=1)  # add the posterior
  prior <- posterior  # update the prior
}

max(posterior)
sum(posterior)
dbinom(6, size=9, prob=0.5)


# EVALUATE
# translate "small world" findings to the "big world"
# is there auto-correlation in the findings?
# is there a colour blind person in audience? can't tell blue from green
# POSTERIOR PREDICTION CHECKS


p_grid <-seq(from=0,to=1,length.out=1000)
plot(p_grid)
prob_p <-rep(1,1000)
plot(prob_p)
prob_data <-dbinom(6,size=9,prob=p_grid)
posterior <-prob_data*prob_p
posterior <-posterior/sum(posterior)


# sampling 1, 1000 times
# samples each value in p_grid with probability posterior
samples <-sample(p_grid, prob=posterior, size=1e4, replace=TRUE)
plot(samples)


dens(samples)

# INTERVALS
# rethinking functions to return  
PI(p)
HPDI(p)



########################################################################
# R LEARNED
########################################################################
# par(mfrow=c(nrows,ncols)) 
# layout(nrows, ncols)
# https://www.statmethods.net/advgraphs/layout.html






