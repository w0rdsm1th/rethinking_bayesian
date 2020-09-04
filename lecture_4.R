# lecture 4
# https://www.youtube.com/watch?v=ENxTrFf9a7c&list=PLDcUM9US4XdNM4Edgs7weiyIguLSToZRI&index=4&t=10s

tmp_lib <- "C:/Users/aleks/R/rethinking_library"
library(StanHeaders, lib.loc = tmp_lib)
library(rethinking, lib.loc = tmp_lib)

# Uncertainty
# getting a distribution of regression lines
# sample from posterior ('an infinite collection of regression lines, ranked by )
# works for all models: 
# 1) sample posterior
# 2) push samples 'through models' 
# 3) use samples to generate predictions that effectively integrate over the uncertainty


# generating a confidence interval: 2 ways, 'spaghetti plot of alpha and beta samples from posterior' OR sampling mu from posterior
# predicting mu: given individual weighs 50kg, how tall does the prior distribution predict they are?
# get a range of mu values for fixed 
?rethinking::link  
# link samples from posterior
# we input a series of predictor values
# for each predictor value, for each sample from posterior, calcs mu: a + b*(weight - xbar)

# another concept: sampling for sigma (s.d.). uncertainty around the range of predictions


# CURVES FROM LINES
# - "linear" models are additive, e.g. a+bx
# - Polynomial regression: common BUT badly behaved
# - Splines: very flexible, highly geocentric 
# (not mechanistic, no 'science' in the generation of the shape, SO predictions not reliable if go outside the range of the data)

# Polynomial regression
# 1st order: mu = alpha + Beta1*x
# 2nd order (parabola): mu = alpha + Beta1*x + Beta2*x**2
# 3rd... add a cubic


# example Polynomial: include the kids in the !Kung height/weight
# n.b. centered the weight (weight for indiv i - mean weight), then alpha is the mean
# Beta1 and Beta2 parameters need to be considered together, and dont really "have meaning", but both control the shape of the curve


# standardise the variables before fit: 
# SHOULD BE DEFAULT BEHAVIOUR UNLESS REASON OTHERWISE
# sometimes helps interpretation: get Z-scores, so a value of 1 is 1 s.d. away from the mean
# R fitting works better because doesnt have to guess scale of variable


# TODO - try model without standardising the weight first

# posterior sampling after poloynomial 

# polynomial downsides
# 1) the model FLAILS about at the edges of the data, even goes 'up' at lower weights
# 2) every parameter (e.g. B1 in isolation) acts globally on the shape of the parabola
# 3) cannot do monotonic relationships: parabolas have to curve down eventually, so RHS edge of data are bound 
# cubics have to turn twice: will see an s-shape change, TBD where happens at boundaries of data


# SPLINES: anchoring bars with 'knots'. alternative: bezier curve.
# B-Splines or Basis-Splines. Bayesian B-Spline often called P-Spline (penalty-Splines, Penalty are the priors)
# anchors are local parameters
# basis function: a local function, a component of the model. tunable individually so dont get wild polynomial swings.

# mu = alpha + w1+Bi,1 + w2+Bi,2 + w3+Bi,3 
# predictor variable (e.g. individual's weight) will not appear in your model
# w are like slopes
# basis functions B are synthetic variables, like squared or cubed terms,
# B values just 'turn on parameters' over different ranges of the x-axis variables

# temperature data in the date of the bloom of the cherry blossoms
data("cherry_blossoms")
d <- cherry_blossoms
precis(d)


par(mfrow=c(1, 1))
plot(d$year, d$temp)

# GOAL: detrend the temperature data at a really long time scale, to compare the short term deviations 

# B-spline recipe:
# choose the location of knots (global wiggly). choice method: even intervals, density of obsv? a big literature on optimal choice
# choose degree of basis functions (local wiggly)
# find posterior dist of weights




library(splines)
d2 <- d[complete.cases(d$doy),] #complete cases on doy
num_knots <-15
knot_list <-quantile(d2$year,probs=seq(0,1,length.out=num_knots))
B <- bs(d2$year,
       knots=knot_list[-c(1,num_knots)] ,
       degree=3, 
       intercept=TRUE)

# Typical of Bayesian models: 
# more uncertainty at the parameter level than at the prediction level
# params COMBINE to make a prediction
# tell me the exact value of w3? will be in a range. and to make that weight estimate "more accurate" would come at cost of another weight
# model handles these trade-offs between weights internally to make single prediction

# Knots and basis degrees are choices
# must worry about overfitting >> could have a knot at every year, but would be overfitted
# other types of splines dont require knots?
# 

