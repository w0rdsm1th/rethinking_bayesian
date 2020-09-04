# lecture 5
# https://www.youtube.com/watch?v=e0tO64mtYMU&list=PLDcUM9US4XdNM4Edgs7weiyIguLSToZRI&index=6

tmp_lib <- "C:/Users/aleks/R/rethinking_library"
library(StanHeaders, lib.loc = tmp_lib)
library(rethinking, lib.loc = tmp_lib)

# spurious correlations
# example of the south, waffle houses, and any old bad thing about the south!
# e.g. divorce, hurricanes,...

# dedicated website to spurious correlations
# https://www.tylervigen.com/spurious-correlations
# other example: Divorce rates in maine correlates with Per Cpaita consumption of margarine
# r=99%!!
# correlation is common place in nature, not necessarily causally related

############################
# spurious association
############################
# why are divorce rates higher in the south? well what are the causes of divorce?
# divorce rates +vely correlated with marriage rate
# divorce rates -vely correlated with median marriage age

# mutliple causes of divorce, how to compare their relative value
# see the model uplift from including 
#  what is value of knowing marriage rate, once we already know the median maraige age
#  and vice versa

# where marriage age affects the marraige age directly AS WELL as the Divorce rate
# age is a confounding variable
# 'pipe' | is 'conditional on'
# need the conditional association M <-> D | A

# building a Divorce rate causal model
# another reason for standardising the variables: priors are easier to set, 
# e.g. then let alpha ~ Normal(0, 0.2)
# where alpha has meaning of 'expected value of outcome when all the predictors are at their expected values'
# then let Bm ~ Normal(0, 0.5), validate thru prior predictive simulation: 'what does the model think before it sees the data, how dumb is it?'


# reason why causal inference is important: what trying to do, 
# if just trying to predict: dont necessarily need to understand the causal direction
# if trying to intervene, change the divorce rate, then need to understand else won't have an impact

# predictor residual plots: understanding how regression works.
# "control for the other predictors"
# residual = 'unexplained bit' of observation after applying the model regression
# massive sin of this approach: doesn't consider the relationship between variables,
# e.g. interaction between marriage Age and Rate



# counterfactual plots: keeping one variable fixed, vary all the other variables in the model

# posterior predicted compared to observed: per-state compairson of predicted versus observed (and here allow for 89% CIs from points to the )

# masked association: both real predictors, act in opposite direction to each other and correlated with each other.
# e.g. milk energy concentration (kcal/g) and neocortex (wrinkly bit) size compared to brain size
data(milk)
d <- milk
pairs(~kcal.per.g + log(mass) + neocortex.perc, data=d)

d$K <- standardize(d$kcal.per.g)
d$N <- standardize(d$neocortex.perc)
d$M <- standardize(log(d$mass))


dcc <-d[complete.cases(d$K,d$N,d$M),]
m5.5_draft <-quap(
  alist(
    K ~dnorm(mu,sigma),
    mu <-a+bN*N,
    a ~dnorm(0,1),
    bN ~dnorm(0,1),
    sigma ~dexp(1)
  ) ,data=dcc)

prior <-extract.prior(m5.5_draft)
xseq <-c(-2,2)
mu <-link(m5.5_draft,post=prior,data=list(N=xseq))

# 
plot( NULL,xlim=xseq,ylim=xseq)
for (i in 1:50)lines(xseq,mu[i,],col=col.alpha("black",0.3))

# findings: need both neocortex mass AND body mass. some UNOBSERVED which causing both.

# 


# problems with dummy variables
# for k categories, need k-1 dummy variables. erosion of model power as need to fit different slopes to each categories
# need to develop priors for each category
# assumes an a priorii uncertainty of one category over another: e.g. less certain about mens heights vs females
# 

# THE SOLUTION: index variables, e.g. 
# pros:
# dont need multiple different priors for same category, can apply same prior to single Index variable 
# whereas for dummys would naturally need a wider (higher S.D.) prior because it uses both parameters.
# scales well, can go to k-1 arbitrary different indicator variables. 

# BEWARE: R’s lm package automatically creats K-1 variables

# use same model spec (no new coefficients, no new priors)
# can still back out the 'difference between categories'

# CONS
# difficult to supply different priors if actually expect the categories to be different

# in practice: removing the 0s, encode with values 1 onwards
d$sex <- ifelse(d$male==1,2,1)  # constructing Index
# to use with QUAP: just make it a vector, use []
m5.8 <- quap(
  alist(
    height ~dnorm(mu,sigma),
    mu <-a[sex],
    a[sex] ~dnorm(178,20),
    sigma ~dunif(0,50)
  ) ,data=d)
precis( m5.8,depth=2)  # Note the depth=2 that I added to precis. This tells it to show any vector parameters,like our new a vector.

# practice: playing with 'clade' the type of monkey in the milk dataset
data(milk)
d <-milk
levels(d$clade)
d$clade_id <- as.integer(d$clade)  # coerces the factor variable into an index
d$K <-standardize(d$kcal.per.g)

par(mfrow=c(3,1))
# playing with narrowing and widening shared prior Standard Deviation
for( prior.s.d. in c(0.1, 0.5, .7)) {
  m5.9 <-quap(
    alist(
      K ~dnorm(mu,sigma),
      mu <-a[clade_id],
      a[clade_id] ~dnorm(0,prior.s.d.),
      sigma ~dexp(1)
    ) ,data=d)
  labels <-paste("a[",1:4,"]:",levels(d$clade),sep="")
  
  xlab <- paste("expected kcal(std) for prior ", prior.s.d.)
  plot( precis(m5.9,depth=2,pars="a"),labels=labels,
        xlab=xlab)
  
}

# adding a playful new category to the model
# distributions remain the same for a posteriors, pretty random for h 
par(mfrow=c(3,1))
for (rand.draw in sample.int(100, 3)) {
  set.seed(rand.draw)
  d$house <-sample(rep(1:4,each=8),size=nrow(d))
  print(d$house)
  
  m5.10 <-quap(
    alist(
      K ~dnorm(mu,sigma),
      mu <-a[clade_id]+h[house],
      a[clade_id] ~dnorm(0,0.5),
      h[house] ~dnorm(0,0.5),
      sigma ~dexp(1)
    ) ,data=d)
  
  xlab <- paste("expected kcal(std) for rand.draw ", rand.draw)
  plot( precis(m5.10,depth=2,pars="h"),
        xlab=xlab)
}

  


############################
# multiple regression
############################
# GOOD:
# reveal spurious correlation
# uncover masked association


# BAD:
# cause spurious correlation
# hide real associations

############################
# Directed Acyclic Graphs
############################
# DAGs
# Directed: arrows, edges have direction. can be bi-directional
# Acyclic: arrows dont make loops. (Nature does have loops over time, but these are )
# Graphs: nodes (variables) and edges (causal relationships)
# DIFFERENT from statistical model, has causal implications

# DAG terminology: forks, pipes, colliders

# backdoor criterion: if adding a variable is warranted by the causal inference that trying to prove








