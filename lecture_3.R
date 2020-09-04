# lecture 3
# https://www.youtube.com/watch?v=h5aPo5wXN8E&list=PLDcUM9US4XdNM4Edgs7weiyIguLSToZRI&index=3&t=1s

tmp_lib <- "C:/Users/aleks/R/rethinking_library"
library(StanHeaders, lib.loc = tmp_lib)
library(rethinking, lib.loc = tmp_lib)

# 
data(Howell1)
d <- Howell1

precis(d)
precis(d[d$male==1, ])
hist(d[d$male==1 & d$age >= 18, 1])
hist(d[, 1])

# kalahari 
# PRIOR predictive distribution: before model has seen data, what does it believe?
# 'building good priors' before we show it the data

# plotting priors
curve( dnorm(x,178,20),from=100,to=250)
curve( dunif(x,0,50),from=-10,to=60)

# prior prediction, BEFORE show model the data. not much effect with simple single variable models
# density of the priors
# NOTE: this is a t-distribution, NOT normal, has fat tails
# bad: some really tall individuals, some absolute midgets
# good: at least no negatives, if mu (s.d.) were 100 then would get some negative heights
sample_mu <-rnorm(1e4,178,200)
sample_sigma <- runif(1e4,0,50)
prior_h <-rnorm(1e4,sample_mu,sample_sigma)
dens( prior_h)


# grid approxamation: only estimating 2 params, sigma and mu
# estimating P(observed height|sigma and mu)
d2 <- d[d$age >= 18, ]

mu.list <-seq(from=150,to=160,length.out=100)
sigma.list <-seq(from=7,to=9,length.out=100)
post <-expand.grid(mu=mu.list,sigma=sigma.list)
post$LL <-sapply(1:nrow(post),function(i)sum(
  dnorm(d2$height,post$mu[i],post$sigma[i],log=TRUE)))
post$prod <-post$LL+dnorm(post$mu,178,20,TRUE)+
  dunif( post$sigma,0,50,TRUE)
post$prob <-exp(post$prod-max(post$prod))

# contour plots: 'number of ways that could see the observed data, given that sigma and mu'
contour_xyz(post$mu, post$sigma, post$prob)
image_xyz(post$mu, post$sigma, post$prob)

# sampling from the posterior: since there are two parameters, and
# we want to sample combinations of them, we first randomly sample row numbers in post
# in proportion to the values in post$prob.


# note: skew in sigma, right tail is longer
# know that sigma is +ve, negative s.d. is impossible! so almost always more uncertainty 
# in the larger values.
# skew not present in mu because mean _COULD_ be negative


# quadratic approximation: algorithmic way of solving for posterior
# basically MLE (with flat priors)
# need the peak of the posterior: MAXIMUM A POSTERIOR (MAP) 
# 2 parameters: so need the covariance matrix between the parameters
# rethinking::quap()  # quadratic approximation

sample_sigma <- rcauchy(1e4,0,50)
curve( dunif(x,0,50),from=-10,to=60)
curve( dcauchy(x,600,100),from=0,to=120)

flist <-alist(
  height ~ dnorm(mu,sigma),
  mu ~ dnorm(178,20),
  # sigma ~ dunif(0,50)  # original
  sigma ~ dcauchy(100,100)  # trying new distributions for S.D.
)


m4.1 <- quap(flist, data=d2)

summary(m4.1)  # inbuilt summary, criticises for "too many p-values"
precis(m4.1)  # McElreth's minimalistic summary() inbuilt


# adding a predictor: weight, how does weight predict height (in adults)?
# now mu depends on upon the person!
# priors: coefficients in the prediction



# building in prior knowledge: positive heights
# use log-normal distribution. "where numbers previously didn't look normal, but are when you log. strictly positive"

b <- rlnorm(1e4, 0, 1)
dens( b,xlim=c(0,5),adj=0.1)



     
     
