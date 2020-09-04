# lecture 6
# https://www.youtube.com/watch?v=l_7yIUqWBmE&list=PLDcUM9US4XdNM4Edgs7weiyIguLSToZRI

tmp_lib <- "C:/Users/aleks/R/rethinking_library"
library(StanHeaders, lib.loc = tmp_lib)
library(rethinking, lib.loc = tmp_lib)


# selection distortion effect or Berkson's Paradox: not causal
# e.g 'big if true' Science
# female hurricanes are more deadly than male
# Mono Lake: high levels of arsenic, which mimics phosphate, replaces phosphate in cells and breaks thinks
# newsworthiness versus trustworthiness 

# ANALAGOUS TO MULTI-VARIABLE MODEL, which creates an arbitrary line or sub-population in the data
# need to be cautious adding variables because it will 
# regression is clumsy and automatically focuses on most informative cases


# RISK to adding everything
# - CREATES confounding variables
# - residual confounding
# - overfitting


# within DAG view, it is provable that there are 4x causal confounding 
# (ignoring things like measurement error)
# there is a matching remedy to each

# Fork: Z is a common cause of X and Y
# X <- Z -> Y
# e.g. median age of marriage influences both Marriage rate and Divorce rate
# Deconfounding: condition on Z removes dependency between X and Y. Including Z in the regression


# Pipe: there's an intermediary variable that clumsily including and hiding the real causal influence.
# X -> Z -> Y
# X causes Z, which causes Y. NOTE: in data alone, can't tell difference between Pipe and Fork, need something else.
# Deconfounding: conditioning on Z removes dependency between X and Y. 
# including Z in regression "Cuts out" the real causal variable, X.
# e.g. Post-treatment bias, controlling for consequence of treatment statistically knocks out treatment. End up inferring that treatment don't work.
# greenhouse experiment on plants: Initial height of plant, Anti-fungal treatment on plants to help them grow Fungus, Outcome.
# Initial height AND Anti-funagl treatment (direct affect on Fungus, not on height) combine to cause the final height
# where Fungus measurements are an intermediary variable that might affect H1

# e.g. gender or race wage gaps: nobody wants to put out a DAG
# "if condition on career choice, there's no wage gap"
# gender influences field choice, and some fields are less well paid
# to fix it, have to fix it "upstream"


# *most dangerous, and common*
# X -> Z <- Y
# Collider: Z is a common result of X and Y. X and Y are really independent, but both cause Z.
# conditioning on Z creates a spurious association between X and Z.
# e.g. a study's trustworthiness and newsworthiness, no statistical association. but Z of "being published" puts a lens that both 
# another example: a lightswitch (X) and electricity (Y) both cause lightbulb (Z). can deduce other causal factor if know one (X or Y) and outcome Z
# e.g. taller people better at basketball?
# being tall is causally an advantage in basketball
# conditional on being a professional player, no correlation between Avg points per game and height 
# shorter players compensate, they're awesome in other ways.
# Deconfounding: 


# Descendant
# X -> Z -> Y
#     |
#     A
# Deconfounding: 



