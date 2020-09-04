# chapter 3

tmp_lib <- "C:/Users/aleks/R/rethinking_library"
# explicitly load the affected packages from the temporary library
library(StanHeaders, lib.loc = tmp_lib)
library(rethinking, lib.loc = tmp_lib)
library(ggplot2)


######################################################################
# notes from chapter
######################################################################
# when condition of interest is rare, tests that find all true cases still guarantee that +ve result carries much information at all
# P(vampire | positive result) = 
(.95 * .001) / (.01 * .999 + 0.95*.001)

# frequency format or natural frequencies.
# alternative way of calculating:
# (1) Inapopulationof100,000people,100ofthemarevampires.
# (2) Ofthe100whoarevampires,95ofthemwilltestpositiveforvampirism.
# (3) Ofthe99,900mortals,999ofthemwilltestpositiveforvampirism.
# Pr(vampire|positive) = 95 / 1094 ≈ 0.087


# reasons for working with samples from posterior and not integrals of the posterior itself
# 1) many scientists feel uncomfortable with integrals
# 2) MCMC produces samples


# aside on "bad science", trying to detect signal 
# POWER of the test: Pr(sig|true result)
# false positive rate: Pr(signal | false result)

# The most important thing to do is to improve the base rate, Pr(true), and that requires thinking, not testing.


# 3.2 - sampling to summarize
# (1) intervals of defined boundaries,
# (2) questions about intervals of defined probability mass,
# (3) questions about point estimates.


# playing around with parameter ranges to rbinom and how changes the distribution
par(mfrow=c(4, 6))
for (size in c(5, 10, 50, 1e3)) {
  for (prob in c(0.001, 0.1, 0.3, .5, .8, .99)) {
    iter_obsvs <- rbinom( n_simulations, size=size, prob=prob)
    simplehist( iter_obsvs )
  }
}

# combining sampling of simulated observations as above with sampling parameters from the posterior distribution.
# we do better when we use the entire posterior distribution, not just some point estimate derived from it, because use all the uncertainty info in the entire distribution
# propagating the parameter uncertainty into predictions
posterior.predictive.dist <- rbinom(n_simulations, size=15, prob=q3m_samples)
table(posterior.predictive.dist)/n_simulations


# alternative views of "sequences" of Water versus Land, measures of correlation between tosses in the data. are the data "expected"?
# maximum run of water or land
p <- 0.7
n <- 9
# https://math.stackexchange.com/questions/1409372/what-is-the-expected-length-of-the-largest-run-of-heads-if-we-make-1-000-flips
probability_long_switch <- log(1/p)*n*(1-p)
simplehist(rbinom(1e4, size=n, prob=q3m_samples))

# number of switches between water and land
probability_switch <- p * (1-p)
simplehist(rbinom(1e4, size=n, prob=probability_switch))


###################################
# exercises
###################################

n_samples <- 1e6
length.out <- 1e3
# definegrid
p_grid <-seq(from=0,to=1,length.out=length.out)
# defineprior
prior <-rep(1,length.out)

# computelikelihoodateachvalueingrid
likelihood <-dbinom(6,size=9,prob=p_grid)
# computeproductoflikelihoodandprior
unstd.posterior <-likelihood*prior
# standardizetheposterior,soitsumsto1
posterior <-unstd.posterior/sum(unstd.posterior)
# sample from prior distribution
set.seed(100)
samples <-sample(p_grid,prob=posterior,size=n_samples,replace=TRUE)


### alternative using quap
globe.qa <-quap(
  alist(
    W ~dbinom(W+L,p),#binomiallikelihood
    p ~dunif(0,1)#uniformprior
  ) ,
  data=list(W=6,L=3) )
# displaysummaryofquadraticapproximation
precis( globe.qa)
W <-6
L <-3
curve( dbeta(x,W+1,L+1),from=0,to=1)


simplehist( samples, xlab="sampled posterior")

# plotting the Kernel Density Plot
# basic R way
posterior.dens <- density(posterior) # returns the density data
plot(posterior.dens) # plots the results
# rethinking convenience wrapper
dens(samples)

# 3E1. How much posterior probability lies below p = 0.2?
q3e1 <- sum(posterior[p_grid < 0.2])  # where have a defined posterior
sum(samples < 0.2) / n_samples  # where unable to define posterior because of estimation issues, so sampled

#   3E2. Howmuchposteriorprobabilityliesabove p = 0.8?
q3e2 <- sum(posterior[p_grid > 0.8])
1 - sum(posterior[p_grid < 0.8])

#   3E3. Howmuchposteriorprobabilityliesbetween p = 0.2 and p = 0.8?
(1 - q3e2) - q3e1


# trying to plot regions on the 
# dd <- dens(posterior)
# qplot(x,y,data=dd,geom="line") +
#   geom_ribbon(data=subset(dd, x>q75 & x<q95),aes(ymax=y),ymin=0,
#               fill="red",colour=NA,alpha=0.5)


#   3E4. 20% oftheposteriorprobabilityliesbelowwhichvalueof p?
# You know this interval starts at p = 0. To find out where it stops,
# think of the samples as data and ask where the 20th percentile lies:
q3e4 <- quantile(samples, 0.2)

#   3E5. 20% oftheposteriorprobabilityliesabovewhichvalueof p?
1 - q3e4

#   3E6. Which values of p contain the narrowest interval equal to 66% of the posterior probability?
HPDI(samples, prob=.66)

#   3E7. Whichvaluesof p contain66%oftheposteriorprobability,assumingequalposteriorprobabil-
#   itybothbelowandabovetheinterval?
PI(samples, prob=.66)


# 3M1. Supposetheglobetossingdatahadturnedouttobe8waterin15tosses.Constructtheposte-
#   riordistribution,usinggridapproximation.Usethesameflatpriorasbefore.
likelihood <-dbinom(8, size=15, prob=p_grid)
q3m1.posterior <-likelihood*prior
q3m1.posterior <- q3m1.posterior/sum(q3m1.posterior)

# 3M2. Draw10,000samplesfromthegridapproximationfromabove.Thenusethesamplestocal-
#   culatethe90%HPDIfor p.
q3m_samples <- sample(p_grid, prob=q3m1.posterior, size=1e4, replace=TRUE)
HPDI(q3m_samples, .90)  # revised posterior sample after 8/15 draws
HPDI(samples, .9)  # versus the HPDI of 6/9

# 3M3. Constructaposteriorpredictivecheckforthismodelanddata.Thismeanssimulatethedistri-
#   butionofsamples,averagingovertheposterioruncertaintyin p. Whatistheprobabilityofobserving
# 8 waterin15tosses?
n_simulations <- 1e5
dummy_observations <- rbinom(n_simulations, size=15, prob=q3m_samples)
table(dummy_observations)/n_simulations
simplehist( dummy_observations, xlab="dummy water count")

#   3M4. Usingtheposteriordistributionconstructedfromthenew(8/15)data,nowcalculatetheprob-
#   abilityofobserving6waterin9tosses.
~0.13897

# 3M5. Startoverat 3M1, butnowuseapriorthatiszerobelow p = 0.5 andaconstantabove p = 0.5.
# ThiscorrespondstopriorinformationthatamajorityoftheEarth’ssurfaceiswater.Repeateach
# problemaboveandcomparetheinferences.Whatdifferencedoesthebetterpriormake?Ifithelps,
# compareinferences(usingbothpriors)tothetruevalue p = 0.7.
q3m5.prior <- ifelse(p_grid < 0.5, 0, 1)

water.world <- function(prior, ){
  p_grid <-seq(from=0,to=1,length.out=length.out)
  # computelikelihoodateachvalueingrid
  likelihood <-dbinom(6,size=9,prob=p_grid)
  # computeproductoflikelihoodandprior
  unstd.posterior <-likelihood*prior
  # standardizetheposterior,soitsumsto1
  posterior <-unstd.posterior/sum(unstd.posterior)
  # sample from prior distribution
  set.seed(100)
  samples <-sample(p_grid, prob=posterior, size=n_samples, replace=TRUE)
  posterior
}

water.world(q3m5.prior)
# 3M6. SupposeyouwanttoestimatetheEarth’sproportionofwaterveryprecisely.Specifically,you
# wantthe99%percentileintervaloftheposteriordistributionof p tobeonly0.05wide.Thismeans
# thedistancebetweentheupperandlowerboundoftheintervalshouldbe0.05.Howmanytimeswill
# youhavetotosstheglobetodothis?


#   Hard. TheHardproblemshereallusethedatabelow.
# Thesedataindicatethegender(male=1,female=0)ofofficiallyreportedfirstandsecondbornchildrenin100two-childfamilies.

data(homeworkch3)

# total number of boys
sum(birth1) +sum(birth2)


# 3H1. Usinggridapproximation,computetheposteriordistributionfortheprobabilityofabirth
# beingaboy.Assumeauniformpriorprobability.Whichparametervaluemaximizestheposterior
# probability?



#   3H2. Usingthe sample function,draw10,000randomparametervaluesfromtheposteriordistri-
#   butionyoucalculatedabove.Usethesesamplestoestimatethe50%,89%,and97%highestposterior
# densityintervals.
# 3H3. Use rbinom tosimulate10,000replicatesof200births.Youshouldendupwith10,000num-
#   bers,eachoneacountofboysoutof200births.Comparethedistributionofpredictednumbers
# ofboystotheactualcountinthedata(111boysoutof200births).Therearemanygoodwaysto
# visualizethesimulations,butthe dens command(partofthe rethinking package)isprobablythe
# easiestwayinthiscase.Doesitlooklikethemodelfitsthedatawell?Thatis,doesthedistribution
# ofpredictionsincludetheactualobservationasacentral,likelyoutcome?
#   3H4. Nowcompare10,000countsofboysfrom100simulatedfirstbornsonlytothenumberofboys
# in thefirstbirths, birth1. Howdoesthemodellookinthislight?
#   3H5. Themodelassumesthatsexoffirstandsecondbirthsareindependent.Tocheckthisassump-
#   tion,focusnowonsecondbirthsthatfollowedfemalefirstborns.Compare10,000simulatedcounts
# ofboystoonlythosesecondbirthsthatfollowedgirls.Todothiscorrectly,youneedtocountthe
# numberoffirstbornswhoweregirlsandsimulatethatmanybirths,10,000times.Comparethe
# countsofboysinyoursimulationstotheactualobservedcountofboysfollowinggirls.Howdoesthe
# modellookinthislight?Anyguesseswhatisgoingoninthesedata?
