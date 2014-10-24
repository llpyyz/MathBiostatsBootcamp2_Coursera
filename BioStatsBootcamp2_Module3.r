############################################
#Mathematical Biostats Bootcamp 2 - Module 3
############################################

##############################
#Lec 7A - Fisher's Exact Test
##############################

#Not using asymptotics here.
#Want exact test that guarantees the alpha rate regardless of n

#Testing H0: p1 = p2 = p for two proportions, 
#say treated mice receiving a toxicant 
#versus control mice receiving no toxicant.
#We observe that some treated mice get tumors, some don't
#and likewise for control group.

#Can't use Z or chi-sq since sample size is too small
#Don't have specific hypothesized value for p

#Under null hypothesis every permutation of treatment and control subjects is
#equally likely to produce the observed sequence tumor/no tumor

#E.g. suppose observed data is:
#     Treatment: T T T T T C C C C C
#     Tumor    : T T T T N T T N N N

#Then any other perumtation of the T/C is just as likely under H0



##############################
#Lec 7B - Hyper-geometric Dist
##############################

#X = number of tumoer for treated, binom(n1,p)
#Y = number of tumors for control, binom(n2,p)
#X + Y is binom(n1+n2, p)
#Then P(X = x | X + Y = z) has hypergeometric dist



######################################################
#Lec 7C - Fisher's Exact Test In Practice / MC Methods
######################################################

#exact test, built in to R
dat <- matrix(c(4,1,2,3),2)
fisher.test(dat, alternative = "greater")

#MC method - test p1 > p2
treatment_vec <- c("T","T","T","T","T","C","C","C","C","C")
tumor_vec <- c("T","T","T","T","N","T","T","N","N","N")
sims <- 10000
observed_tumors <- 4
count <- 0
for(i in 1 : sims){
    s <- sample(treatment_vec)
	if(sum(s[c(1,2,3,4,6,7)] == "T") >= observed_tumors){
	    count <- count + 1
	}
}
count/sims

############################
#Lec 8A - Chi Square Testing
############################

############################################
#Example 1 - testing equality of proportions
############################################

#p1 and p2 are rates of side effects for treatments X and Y, resp
#H0: p1 = p2
n11 <- 44 #treatment X, side effects
n12 <- 56 #treatment X, none
n21 <- 77 #treatment Y, side effects
n22 <- 43 #treatment Y, none

n1p <- n11 + n12
n2p <- n21 + n22
np1 <- n11 + n21
np2 <- n12 + n22
n <- n1p + n2p

#Method 1:
e11 <- n1p*np1/n
e12 <- n1p*np2/n
e21 <- n2p*np1/n
e22 <- n2p*np2/n
chisq_stat1 <- (n11 - e11)^2/e11 + (n12 - e12)^2/e12 + (n21 - e21)^2/e21 + (n22 - e22)^2/e22
degfr <- 1
pchisq(chisq_stat1, degfr, lower.tail = F) #0.002755 => reject for typical alpha

#Method 3:
dat <- matrix(c(44,77, 56,43), 2)
chisq.test(dat)
chisq.test(dat, correct = F)

#Method 2:
chisq_stat2 <- n*(n11*n22 - n12*n21)^2/(n1p*n2p*np1*np2)

#################################
#Example 2 - testing independence
#################################

#Maternal age versus birthweight
#H0: MA and BW are indep.
#HA: MA and BW are dep.
n11 <- 20 #MA < 20, BW < 2500 g
n12 <- 80 #MA < 20, BW >= 2500 g
n21 <- 30 #MA >= 20, BW < 2500 g
n22 <- 270 #MA >= 20, BW >= 2500 g

n1p <- n11 + n12
n2p <- n21 + n22
np1 <- n11 + n21
np2 <- n12 + n22
n <- n1p + n2p

e11 <- n1p*np1/n
e12 <- n1p*np2/n
e21 <- n2p*np1/n
e22 <- n2p*np2/n
chisq_stat1 <- (n11 - e11)^2/e11 + (n12 - e12)^2/e12 + (n21 - e21)^2/e21 + (n22 - e22)^2/e22 #6.857
degfr <- 1
alpha <- 0.05
qchisq(1 - alpha, degfr) #3.84
#Reject at alpha = 0.05 since chisq_stat > crit chi sq value

#or do pvalue:
pchisq(chisq_stat1, degfr, lower.tail = F) #0.00882 < 0.05 so reject...

####################################################
#Example 2 - testing equality of several proportions
####################################################

#High/low alcohol use across four professions: clergy, educators, executives, retailers
#Test whether proportion of high alcohol use is same across four professions
#H0: p1 = p2 = p3 = p4 = p
#HA: not all equal
n11 <- 32 #high, clergy
n12 <- 268 #low, clergy
n21 <- 51 #high, educators
n22 <- 199 #low, educators
n31 <- 67 #high, execs
n32 <- 233 #low, execs
n41 <- 83 #high, retailers
n42 <- 267 #low, retailers

n1p <- n11 + n12
n2p <- n21 + n22
n3p <- n31 + n32
n4p <- n41 + n42

np1 <- n11 + n21 + n31 + n41
np2 <- n12 + n22 + n32 + n42
n <- n1p + n2p + n3p + n4p

e11 <- n1p*np1/n
e12 <- n1p*np2/n
e21 <- n2p*np1/n
e22 <- n2p*np2/n
e31 <- n3p*np1/n
e32 <- n3p*np2/n
e41 <- n4p*np1/n
e42 <- n4p*np2/n

chisq_stat1 <- (n11 - e11)^2/e11 + (n12 - e12)^2/e12 + (n21 - e21)^2/e21 + (n22 - e22)^2/e22 + (n31 - e31)^2/e31 + (n32 - e32)^2/e32 + 
(n41 - e41)^2/e41 + (n42 - e42)^2/e42 #20.59
degfr <- 3
alpha <- 0.05
qchisq(1 - alpha, degfr) #7.815
#Reject at alpha = 0.05 since chisq_stat > crit chi sq value

#or do pvalue:
pchisq(chisq_stat1, degfr, lower.tail = F) #0.00013 < 0.05 so reject...





###############################
#Lec 8B - Testing Independence
###############################




#############################
#Lec 8C - Generalization / MC
#############################

#chisq.test(x, simulate.p.value = T) #uses MC sim to do Fisher's exact test to yield exact (though conservative) p-value





##################################
#Lec 8D - Goodness of Fit Testing
##################################


###########
#Homework 3
###########

###############
#Question 2 & 3
###############

tp1 <- .53
tp2 <- .35
tp3 <- .12
count1 <- 140
count2 <- 100
count3 <- 50
n <- count1 + count2 + count3
expcount1 <- tp1 * n
expcount2 <- tp2 * n
expcount3 <- tp3 * n

chisq_stat <- (expcount1 - count1)^2/expcount1 + (expcount2 - count2)^2/expcount2 + (expcount3 - count3)^2/expcount3
degfr <- 2
pchisq(chisq_stat, degfr, lower.tail = F) #0.019

###########
#Question 4
###########

dat <- matrix(c(80,60,15, 30,5,10),2)
dat
chisq.test(dat)

###########
#Question 5
###########

tp1 <- log10(2) - log10(1)
tp2 <- log10(3) - log10(2)
tp3 <- log10(4) - log10(3)
tp4 <- log10(5) - log10(4)
tp5 <- log10(6) - log10(5)
tp6 <- log10(7) - log10(6)
tp7 <- log10(8) - log10(7)
tp8 <- log10(9) - log10(8)
tp9 <- log10(10) - log10(9)

count1 <- 275
count2 <- 183
count3 <- 133
count4 <- 111
count5 <- 76
count6 <- 66
count7 <- 66
count8 <- 44
count9 <- 46

n <- count1 + count2 + count3 + count4 + count5 + count6 +count7 + count8 + count9

expcount1 <- tp1 * n
expcount2 <- tp2 * n
expcount3 <- tp3 * n
expcount4 <- tp4 * n
expcount5 <- tp5 * n
expcount6 <- tp6 * n
expcount7 <- tp7 * n
expcount8 <- tp8 * n
expcount9 <- tp9 * n

chisq_stat <- (expcount1 - count1)^2/expcount1 + (expcount2 - count2)^2/expcount2 + (expcount3 - count3)^2/expcount3 +
(expcount4 - count4)^2/expcount4 + (expcount5 - count5)^2/expcount5 + (expcount6 - count6)^2/expcount6 +
(expcount7 - count7)^2/expcount7 + (expcount8 - count8)^2/expcount8 + (expcount9 - count9)^2/expcount9

degfr <- 8
pchisq(chisq_stat, degfr, lower.tail = F) #0.500

###########
#Question 6
###########

n11 <- 65
n12 <- 35
n21 <- 70
n22 <- 30
n31 <- 15
n32 <- 85
n <- n11 + n12 + n21 + n22 + n31 + n32

n1p <- n11 + n12
n2p <- n21 + n22
n3p <- n31 + n32
np1 <- n11 + n21 + n31
np2 <- n12 + n22 + n32

#all expected counts are 50 since each row sums to 100
#each col sums to 150 and thus ec = 100 * 150/ 300 = 50
e11 <- n1p * np1/n
e12 <- n1p * np2/n
e21 <- n2p * np1/n
e22 <- n2p * np2/n
e31 <- n3p * np1/n
e32 <- n3p * np2/n

#can also do:
dat <- matrix(c(65, 70, 15, 35, 30, 85), 3)
colSums(dat)/sum(dat)


#######
#Quiz 3
#######

###########
#Question 1
###########

dat <- matrix(c(4,1,2,6),2)
fisher.test(dat, alternative = "two.sided")

###########
#Question 2
###########

dat <- matrix(c(3,2,1,4),2)
fisher.test(dat, alternative = "greater")

###########
#Question 3
###########

tp1 <- .53
tp2 <- .35
tp3 <- .12
count1 <- 140
count2 <- 100
count3 <- 50
n <- count1 + count2 + count3
expcount1 <- tp1 * n
expcount2 <- tp2 * n
expcount3 <- tp3 * n

chisq_stat <- (expcount1 - count1)^2/expcount1 + (expcount2 - count2)^2/expcount2 + (expcount3 - count3)^2/expcount3
degfr <- 2
pchisq(chisq_stat, degfr, lower.tail = F) #0.019

###########
#Question 4
###########

dat <- matrix(c(80, 60, 15, 30, 5, 10),2)
rs <- rowSums(dat)
cs <- colSums(dat)
e11 <- rs[1] * cs[1]/sum(dat)
e12 <- rs[1] * cs[2]/sum(dat)
e13 <- rs[1] * cs[3]/sum(dat)
e21 <- rs[2] * cs[1]/sum(dat)
e22 <- rs[2] * cs[2]/sum(dat)
e23 <- rs[2] * cs[3]/sum(dat)

###########
#Question 5
###########

dat <- matrix(c(43,8,4,45),2)
chisq.test(dat)
chisq.test(dat, correct = F)

###########
#Question 7
###########

n1 <- 46
n2 <- 54
n3 <- 49
n4 <- 51
n <- n1+n2+n3+n4
e1 <- 50
e2 <- 50
e3 <- 50
e4 <- 50
chisq_stat <- (n1-e1)^2/e1 + (n2-e2)^2/e2 + (n3-e3)^2/e3 + (n4-e4)^2/e4
degfr <- 3
pchisq(chisq_stat, degfr, lower.tail = F) #0.88


###########
#Question 8
###########

n11 <- 65
n12 <- 35
n21 <- 70
n22 <- 30
n31 <- 15
n32 <- 85
n <- n11 + n12 + n21 + n22 + n31 + n32

n1p <- n11 + n12
n2p <- n21 + n22
n3p <- n31 + n32
np1 <- n11 + n21 + n31
np2 <- n12 + n22 + n32

#all expected counts are 50 since each row sums to 100
#each col sums to 150 and thus ec = 100 * 150/ 300 = 50
e11 <- n1p * np1/n
e12 <- n1p * np2/n
e21 <- n2p * np1/n
e22 <- n2p * np2/n
e31 <- n3p * np1/n
e32 <- n3p * np2/n

#can also do:
dat <- matrix(c(65, 70, 15, 35, 30, 85), 3)
chisq.test(dat, correct = F)
chisq.test(dat, correct = T)
