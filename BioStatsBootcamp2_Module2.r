#############################################
#Mathematical Biostats Bootcamp II - Module 2
#David Schonberger
#############################################

##################################
#Lec 4 - Two Sample Binomial Tests
##################################

#Drug A: 11 of 20 patients have side effects

#using z dist
p0 <- 0.1
phat <- 11/20
n <- 20
teststat <- (phat - p0)/sqrt(p0*(1 - p0)/n)
pnorm(teststat, lower.tail = F) #9.9e-12

#Exact test: prob of 11 or more side effects in 20 trials, assuming H_0: p = 0.1
pbinom(10,20,.1, lower.tail = F) #7e-7

#or...

binom.test(11,20,.1, alternative = "greater") #7e-7

#MC sim for Bayesian analysis on two binomial proportions
x <- 11
n1 <- 20
alpha1 <- 1
beta1 <- 1
y <- 5
n2 <- 20
alpha2 <- 1
beta2 <- 1

p1 <- rbeta(1000, x + alpha1, n1 - x + alpha1)
p2 <- rbeta(1000, y + alpha2, n2 - y + alpha2)
rd <- p2 - p1
plot(density(rd))
quantile(rd, c(.025, .975))
mean(rd)
median(rd)


#######################################
#Lec 5 - Relative Risks and Odds Ratios
#######################################




#####################
#Lec 6 - Delta Method
#####################


###########
#Homework 2
###########





####
#3
####
x1 <- 70
x2 <- 15
n1 <- 100
n2 <- 100
p1hat <- x1/n1
p2hat <- x2/n2
phat <- (x1+x2)/(n1+n2)
testStat <- (p1hat - p2hat)/sqrt(phat * (1 - phat) * (1 / n1 + 1 / n2))

####
#4
####
n11 <- 45
n12 <- 21
n21 <- 15
n22 <- 52
seEst <- sqrt(1/n11 + 1/n12 + 1/n21 + 1/n22)


########
#Quiz 2
########

####
#3
####

x1 <- 70
x2 <- 15
n1 <- 100
n2 <- 100
p1hat <- x1/n1
p2hat <- x2/n2
phat <- (x1+x2)/(n1+n2)
testStat <- (p1hat - p2hat)/sqrt(phat * (1 - phat) * (1 / n1 + 1 / n2))
2 * pnorm(testStat, lower.tail = F)

###
#4 est se of log RR
###
n11 <- 45
n12 <- 21
n1 <- n11+n12

n21 <- 15
n22 <- 52
n2<- n21 + n22

p1 <- n11/(n1)
p2 <- n21/(n2)

seEst <- sqrt((1-p1)/(n1*p1) + (1-p2)/(n2*p2))

