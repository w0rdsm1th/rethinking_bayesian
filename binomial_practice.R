# binomial practice
# https://learn.lboro.ac.uk/archive/olmp/olmp_resources/pages/workbooks_1_50_jan2008/Workbook37/37_2_bnml_dist.pdf

(85/120) * (84/119) 

own.binom <- function(k, n, p){
  # (n!/ (n-k)! k! ) p**k*(1-p)**k
  ways <- factorial(n) / (factorial(n-k) * factorial(k))
  proba <- p**k * (1-p)**(n-k)
  ways*proba
}

# on the average, how many time must a die be thrown until one gets a 6?
dice.binom <- function(n) {
  # probability of single number e.g. 6 appearing after n trials
   (n/6)*(5/6)**(n-1)
}

own.binom(1, 1:6, 1/6)
dice.binom(1:6)

neg.binom.failures <- function(n.trials) {
  (1/6)*(5/6)**n.trials
}

cumsum(neg.binom.failures(1:9))
sample(neg.binom.failures(1:6), 1e5, replace = TRUE)

# poker - possible number of 5 card hands, (52 choose 5), unordered with replacement
factorial(52) / (factorial(5)*factorial(47))


# 1) An electronic product has a total of 30 integrated circuits built into it. The product is capable
# of operating successfully only if at least 27 of the circuits operate properly. What is the
# probability that the product operates successfully if the probability of any integrated circuit
# failing to operate is 0.01?
dbinom(27, 30, 0.01)
own.binom(27, 30, 0.01)


# 2. Digital communication is achieved by transmitting information in “bits”. Errors do occur in
# data transmissions. Suppose that the number of bits in error is represented by the random
# variable X and that the probability of a communication error in a bit is 0.001. If at most 2
# errors are present in a 1000 bit transmission, the transmission can be successfully decoded. If
# a 1000 bit message is transmitted, find the probability that it can be successfully decoded.
sum(dbinom(0:2, 1e4, 0.001))


# Example 7
# In a box of floppy discs it is known that 95% will work. A sample of three of the
# discs is selected at random.
# Find the probability that (a) none (b) 1, (c) 2, (d) all 3 of the sample will work
dbinom(0:3, 3, .95)
own.binom(0:3, 3, .95)
