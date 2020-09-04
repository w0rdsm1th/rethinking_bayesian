# homework 2
# https://github.com/rmcelreath/statrethinking_winter2019/blob/master/homework/week02.pdf

tmp_lib <- "C:/Users/aleks/R/rethinking_library"
# explicitly load the affected packages from the temporary library
library(StanHeaders, lib.loc = tmp_lib)
library(rethinking, lib.loc = tmp_lib)

# 1) weights of unobserved individuals from !Kung


indivs <- c(45, 40, 65, 31, 53)

preds <- ...
conf_interval <- 