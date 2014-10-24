############################################
#Mathematical Biostats Bootcamp 2 - Module 4
############################################

############################
#Lec 9AB - Simpson's Paradox
############################

#Marginal and conditional associations may be opposing
#Example: *Marginally*, white defendants get the death penalty more frequently than black defendants
#but *conditionally*, across both black and white victims, black defendants get the death penalty more often than white defendants

?UCBAdmissions
data(UCBAdmissions)
apply(UCBAdmissions, 1, sum) #sums over cols and dept, giving admit/reject total, 1 x 2 tbl
apply(UCBAdmissions, 2, sum) #sums over rows and dept, giving male/female total, 1 x 2 tbl
m <- apply(UCBAdmissions, c(1,2), sum) #sum over depts, produces a 2 x 2 tbl

          # Gender
# Admit    Male   Female
# Admitted 1198    557
# Rejected 1493   1278

#Marginal Acceptance rates:
male_acceptrate <- m[1,1] / colSums(m)[1] #.445
female_acceptrate <- m[1,2] / colSums(m)[2] #.304
#Males acceptance rate almost 1.5 times as big as female but...

#Conditional Acceptance rate by department
m2 <- apply(UCBAdmissions, 3, function(x) c(x[1]/ sum(x[1:2]), x[3]/ sum(x[3:4])))

           # Dept
         # A         B         C         D         E          F
# Male   0.6206061 0.6303571 0.3692308 0.3309353 0.2774869 0.05898123
# Female 0.8240741 0.6800000 0.3406408 0.3493333 0.2391858 0.07038123

#In 4 of 6 depts female accpet rate is higher than male
#and in the two dept (C, E) where male rate is higher,
#it is not much higher

#Why????

#Consider application rates by dept
m3 <- apply(UCBAdmissions, c(2,3), sum)

#Total apps by genrder and dept; (sum of admit and reject numbers):
        # Dept
# Gender A   B   C   D   E   F
# Male   825 560 325 417 191 373
# Female 108  25 593 375 393 341

#Simpson's paradox is only a perceived paradox:
#apparent relationship between two variables can change in presence or absence of third var


#Confounding: external var correlated with explanatory and response vars can distort
#estimated effect.

#Ex: Victim's race is correated with defendants race and use of death penalty

#Can adjust for confounding by stratify by confounder and then combine strata-specific estimates
#Have to correctly weight the strata-specific estimates

#Note: unnecessary stratification reduces precision



###################
#Lec 9C - Weighting
###################
##################
#Lec 9D - CMH Test
##################


dat <- array(c(11, 10, 25, 27, 16 ,22, 4, 10, 14, 7, 5, 12, 2, 1, 14, 16, 6, 0, 11, 12, 1, 0, 10, 10, 1, 1, 4, 8, 4, 6, 2, 1), 
c(2,2,8))
mantelhaen.test(dat, correct = F)



################################
#Lec 10A - Case Control Sampling
################################
#########################################
#Lec 10B - Exact Inference for Odds Ratio
#########################################


################################
#Lec 11A - Matched 2x2 Tables
################################
##############################################
#Lec 11B - Dependence and Marginal Homogeneity
##############################################

#McNemar's Test for marginal homogenity in Matched Pairs binary data

#Ex: survey 1 v. survey 2 approval data
mcnemar.test(matrix(c(794, 86, 150, 570),2), correct = F)
# McNemar's Chi-squared test

# data:  matrix(c(794, 86, 150, 570), 2)
# McNemar's chi-squared = 17.3559, df = 1, p-value = 3.099e-05


###############################################################
#Lec 11C - Estimation of the Marginal Difference in Proportions
###############################################################
###############################################
#Lec 11D - Odds and Ends for Matched 2x2 Tables
###############################################


########################
#Lec 12A - The sign test
########################
#############################
#Lec 12B - The sign rank test
#############################
############################
#Lec 12C - The rank sum test
############################






#####################
#Poisson Distribution
#####################







###################
#Poisson likelihood
###################

#Example: 5 failure events is 94 day monitoring period of a pump
lambda <- seq(0,0.2, length = 1000)
likelihood <- dpois(5, 94* lambda)/ dpois(5,5)
plot(lambda, likelihood, frame = F, lwd = 3, type = "l", xlab = expression(lambda))
lines(rep(5/94,2), 0:1, col = "red", lwd = 3)
lines(range(lambda[likelihood > 1/16]), rep(1/16,2), lwd = 2)
lines(range(lambda[likelihood > 1/8]), rep(1/8,2), lwd = 2)


############################
#Poisson P-value calculation
############################


###########
###########
#Homework 4
###########
###########

###########
#question 1
###########

dat <- array(c(8, 52, 5, 164, 25, 29, 21, 128), c(2,2,2))
mantelhaen.test(dat)


###########
#question 3
###########

dat <- matrix(c(5,1,4,0), 2)
mcnemar.test(dat) #pvalue = 0.38 : fail to reject

n11 <- 5
n12 <- 4
n21 <- 1
n22 <- 0

ts <- (n21 - n12)^2/(n12 + n21) #1.8


###########
#question 4
###########

#marginal odds ratio for exposure of case to exposure of controls

dat <- matrix(c(243, 189, 54, 153),2)
mor <-  (colSums(dat)[1] * rowSums(dat)[2])/ (rowSums(dat)[1] * colSums(dat)[2])  #2.4

###########
#question 5
###########

#Note: can also use large sample mcnemar test

#same data as #4; large sample pvalue for H_O: d = p1+ - p+1 == 0
dat <- matrix(c(243, 189, 54, 153),2)
n <- sum(dat)
propdiff <- (rowSums(dat)[1] - colSums(dat)[1])/n
pi_1p <- rowSums(dat)[1]/n
pi_p1 <- colSums(dat)[1]/n
pi_11 <- dat[1,1]/n
pi_12 <- dat[1,2]/n
pi_21 <- dat[2,1]/n
pi_22 <- dat[2,2]/n

vardiff <- (pi_1p * (1 - pi_1p) + pi_p1 * (1 - pi_p1) - 2 * (pi_11 - pi_22 - pi_12 * pi_21))/n
sediff <- sqrt(vardiff)
teststat <- propdiff/sediff


###########
#question 6
###########

#p-value > .05
binom.test(3,5, alternative = "g")
binom.test(4,5, alternative = "g")

#p-value > .05
binom.test(5,5, alternative = "g")

###########
#question 7
###########

#power of above test when p = 0.9 instead of 0.5
p <- 0.9^5 #.5905


#######
#######
#Quiz 4
#######
#######

###########
#question 1
###########

dat <- array(c(8, 52, 5, 164, 25, 29, 21, 128), c(2,2,2))
mantelhaen.test(dat) #p < .05, common odds ratio likely != 1


###########
#question 2
###########

#3 ranks, 8 possibilities of sign assignment: (1,2,3), (1,2,-3), (1,-2,3), (-1,2,3), ..., (-1,-2,-3)
#E(W+) = 24/8 = 3
#Smallest p-value is when W+ = 6 or W+ = 0, each with prob = 0.125, thus two sided p-value = .25 at the very least

###########
#question 3
###########

n11 <- 55
n12 <- 41
n21 <- 12
n22 <- 20
ts <- (n21 - n12)^2/(n12 + n21) #15.9
pchisq(ts,1,lower.tail = F) #p-value 0.00007

#or...

dat <- matrix(c(55,12,41,20), 2)
mcnemar.test(dat, correct= F) #p-value 0.00007


###########
#question 4
###########

#same data as 3
#marginal odds ratio for positive diag, B/ positive diag A

dat <- matrix(c(55,12,41,20), 2)
mor <- (rowSums(dat)[1] * colSums(dat)[2]) / (colSums(dat)[1] * rowSums(dat)[2])


###########
#question 5
###########

#conditional odds ratio: odd of exposure for cases / odds of exposure for controls

dat <- matrix(c(243, 189, 54, 153),2)

###########
#question 6
###########


binom.test(0,5, alternative = "g")
binom.test(1,5, alternative = "g")
binom.test(2,5, alternative = "g")
binom.test(3,5, alternative = "g")
binom.test(4,5, alternative = "g")
binom.test(5,5, alternative = "g")

binom.test(0,5)
binom.test(1,5)
binom.test(2,5)
binom.test(3,5)
binom.test(4,5)
binom.test(5,5)

###########
#question 7
###########

#power of above test when p = 0.8 instead of 0.5
p <- 0.8^5 #0.32768

