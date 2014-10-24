################################################################################
#Math Biostats Bootcamp 2 Module 1 - Hypothesis Testing, Power, Two Sample Tests
################################################################################

#####
#Lec 1
#####

#upper-tailed test shading example for alpha = 0.5
xval <- seq(-3.2, 3.2, length = 1000)
yval <- dnorm(xval)
plot(xval, yval, type = "l", axes = T, frame = F, lwd = 3, xlab = "", ylab = "")
x <- seq(qnorm(.95), 3.2, length = 100)
polygon(c(x,rev(x)), c(dnorm(x), rep(0, length(x))), col = "salmon" )
text(mean(x), mean(dnorm(x)) + .02, "5%", cex = 2)
text(qnorm(.95), .01, "1.645", cex = 2)

#two-tailed test for alpha = 0.05
plot(xval, yval, type = "l", axes = T, frame = F, lwd = 3, xlab = "", ylab = "")
x <- seq(qnorm(.975), 3.2, length = 100)
polygon(c(x,rev(x)), c(dnorm(x), rep(0, length(x))), col = "salmon" )
text(mean(x), mean(dnorm(x)) + .02, "2.5%", cex = 2)
text(qnorm(.975), .01, "1.96", cex = 2)
x <- seq(-3.2, qnorm(.025), length = 100)
polygon(c(x,rev(x)), c(dnorm(x), rep(0, length(x))), col = "salmon" )
text(mean(x), mean(dnorm(x)) + .02, "2.5%", cex = 2)
text(qnorm(.025), .01, "-1.96", cex = 2)
text(0, dnorm(0)/5, "95%", cex = 2)

#z-tests above use CLT assumptions and big enough n

#if n is small, use t test; n = 16 here
xval <- seq(-4, 4, length = 1000)
yval <- dt(xval, 15)
plot(xval, yval, type = "l", axes = T, frame = F, lwd = 3, xlab = "", ylab = "")
x <- seq(qt(.975, 15), 4, length = 100)
polygon(c(x,rev(x)), c(dt(x, 15), rep(0, length(x))), col = "salmon" )
text(mean(x), mean(dt(x,15)) + .02, "2.5%", cex = 2)
text(qt(.975,15), .01, "2.13", cex = 2)
x <- seq(-4, qt(.025,15), length = 100)
polygon(c(x,rev(x)), c(dt(x, 15), rep(0, length(x))), col = "salmon" )
text(mean(x), mean(dt(x, 15)) + .02, "2.5%", cex = 2)
text(qt(.025, 15), .01, "-2.13", cex = 2)
text(0, dt(0, 15)/5, "95%", cex = 2)

#p-val shading, n = 16, test stat = 0.8 -> p = .22
pt(0.8, 15, lower.tail = F) #about 0.22
xval <- seq(-4, 4, length = 1000)
yval <- dt(xval, 15)
plot(xval, yval, type = "l", axes = T, frame = F, lwd = 3, xlab = "", ylab = "")
x <- seq(.8, 4, length = 100)
polygon(c(x,rev(x)), c(dt(x, 15), rep(0, length(x))), col = "salmon" )
text(mean(x), mean(dt(x, 15)) + .02, "22%", cex = 2)
text(.8, .01, ".8", cex = 2)

#####
#Lec 2
#####

#power calculation n = 16, mu_a - mu_0 = 2; sigma = 4; assumed alpha 0.05
power.t.test(n = 16, delta = 2 / 4, type = "one.sample", alt = "one.sided") #about 60.4%

#monte carlo sim of pwer calc
nosim <- 100000
n <- 16
sigma <- 4
mu0 <- 30
mua <- 32
z <- rnorm(nosim)
xsq <- rchisq(nosim, df = n - 1)
t <- qt(.95, n - 1)

#calc mean of logicals (T = 1, F = 0) -> simulated % of time probability
#giving power is satisfied (see slide 10, Lec 2)
mean(z + sqrt(n) * (mua - mu0)/sigma > t / sqrt(n - 1) * sqrt(xsq)) 



#####
#Lec 3 Two Sample Tests
#####



#####
#####
#HW 1
#####
#####

###
#Q1 - single sample
###
n <- 100
mu0 <- 10
xbar <- 12
sdev <- 4
tcrit <- qt(.95, n - 1) #1.61
tstat <- (xbar - mu0)/(sdev/sqrt(n)) #5 -> reject H_0

###
#Q2 - paired data
###

d <- c(-8, -3, 1, -2, -5)
xbar <- mean(d)
n <- 5
sdev <- sd(d)
tstat <- xbar/(sdev/sqrt(n))
qt(.025, n - 1) #crit t val
pt(tstat, n - 1) * 2 #pval -> fail to reject H_0


###
#Q3
###

tcrit <- qt(.975,8)
xbar <- 1100
sdev <- 30
n <- 9
se <- sdev/sqrt(n)

#fail to reject if mu) is in this range:
xbar - tcrit * se #1077
xbar + tcrit * se #1123

###
#Q4 - two sample indep groups
###

#wait time for new triage system
n1 <- 100
xbar1 <- 4 
sdev1 <- .5

#wait time for old triage system
n2 <- 100
xbar2 <- 6
sdev2 <- 2

#2-tailed z test

#assume equal variances
zcrit <- qnorm(.975) #1.96
zstat <- (xbar1 - xbar2)/(sqrt(.5*(sdev1^2 + sdev2^2)) * sqrt(1/n1+1/n2)) #-9.75 -> pval is 10^-21 reject H_0

#assume unequal variances
zstat <- (xbar1 - xbar2)/(sqrt(sdev1^2/n1 + sdev2^2/n2)) #-9.70 -> reject H_0

###
#Q5 - two sided t test
###

#treatment group
n1 <- 9
xbar1 <- -3
sdev1 <- 1.5

#placebo group
n2 <- 9
xbar2 <- 1
sdev2 <- 1.8

#assuming common variance and normal populations
tcrit <- qt(.975, n1+n2 - 2) #2.11
sp <- sqrt(.5*(sdev1^2 + sdev2^2))
tstat <- (xbar1 - xbar2)/(sp*sqrt(1/n1 + 1/n2)) #-5.12
pval <- pt(tstat,n1+n2-2) #.00005 -> reject H_0

###
#Q6 - 95% CI for Q5 data
###
(xbar1 - xbar2) - tcrit * (sp * sqrt(1/n1 + 1/n2)) #-5.66
(xbar1 - xbar2) + tcrit * (sp * sqrt(1/n1 + 1/n2)) #-2.34

###
#Q7 - pval for 2-sided hypothesis test
###

#systolic bp for oral contraceptive users
n1 <- 16
xbar1 <- 11
sdev1 <- 20

#systolic bp for control
n2 <- 16
xbar2 <- 4
sdev2 <- 28

sp <- sqrt(.5*(sdev1^2 + sdev2^2))
zstat <- (xbar1 - xbar2)/(sp * sqrt(1/n1+1/n2))
pval <- pnorm(zstat, lower.tail = F) * 2 #.415

tstat <- (xbar1 - xbar2)/(sp * sqrt(1/n1+1/n2))
pval2 <- pt(tstat, n1 + n2 - 2, lower.tail = F) * 2 #.422

###
#Q8 - power of a 1-sided 5% z test
###
n <- 100
sdev <- .04
d <- .01
std_mua <- sqrt(n) * d / sdev
zcrit <- qnorm(.95)
pow <- pnorm(zcrit - std_mua, lower.tail = F)


###
#Q9 - 2 sided z test for indep groups
###

#sample from Metropolis
n1 <- 288
xbar1 <- 44
sdev1 <- 12

#sample from Gotham City
n2 <- 288
xbar2 <- 42.04
sdev2 <- 12

sp <- sqrt(.5 * (sdev1 ^ 2 + sdev2 ^ 2))
se <- sp * sqrt(1 / n1 + 1 / n2)

zstat <- (xbar1 - xbar2)/(se)
pval <- 2 * pnorm(zstat, lower.tail = F) #.05

#########
#########
#Quiz #1
#########
#########

###
#Q1
###

n <- 100
xbar <- 12
sdev <- 4
se <- sdev / sqrt(n)
tcrit <- qt(.95, n - 1) #1.66
zcrit <- qnorm(.95) #1.645
mu0a <- xbar - tcrit * se #about 11.3 or lower to reject at .05 level
mu0b <- xbar - zcrit * se

###
#Q2 - compare hypothesis testing for paired and unpaired (on clearly paired data)
###

x0 <- c(140, 138, 150, 148, 135)
x1 <- c(138, 136, 148, 146, 133)
d <- x1 - x0 #diffs are all 2 -> var = 0 -> reject H_0 with a pval = 0

#indep group test, .05 level, 2-sided
xbar0 <- mean(x0)
sdev0 <- sd(x0)
n0<- length(x0)

xbar1 <- mean(x1)
sdev1 <- sd(x1)
n1 <- length(x1)

#assume equal variances
sp <- sqrt(.5 * (sdev0^2 + sdev1^2))
se <- sp * sqrt(1/n0 + 1/n1)

tcrit <- qt(.975, n0 + n1 - 2) #2.3
tstat <- (xbar0 - xbar1) / se #.49 -> fail to reject

#or...
t.test(x0,x1)

###
#Q4
###

xnew <- c(.929, -1.745, 1.677, .701, .128)
xbar_n <- mean(xnew)
sdev_n <- sd(xnew)
n1 <- length(xnew)

xold <- c(2.233, -2.513, 1.204, 1.938, 2.533)
xbar_o <- mean(xold)
sdev_o <- sd(xold)
n2 <- length(xold)

d <- xbar_n - xbar_o

#assume equal variances
sp <- sqrt(.5*(sdev_n ^ 2 + sdev_o ^2))
se <- sp * sqrt(1/n1 + 1/n2)
tstat <- d / se
pval1 <- pt(tstat, n1 + n2 - 2) #.26

#unequal variances
se2 <- sqrt(sdev_n^2/n1 + sdev_o^2/n2)
tstat2 <- d / se
a <- (sdev_n^2/n1 + sdev_o^2/n2)^2
b <- (sdev_n^2/n1)^2/(n1-1) + (sdev_o^2/n2)^2/(n2-1)
df <- a / b
pval2 <- pt(tstat2, df) #.26

#using t.test
t.test(xnew, xold, "less") #pval = .2597

#using paired data...which actually makes sense (and negates the manual work above)
t.test(xnew, xold, "less", paired = T) #pval = .1405

#if doing paired test, manually...
xnew <- c(.929, -1.745, 1.677, .701, .128)
xbar_n <- mean(xnew)
sdev_n <- sd(xnew)
n1 <- length(xnew)

xold <- c(2.233, -2.513, 1.204, 1.938, 2.533)
xbar_o <- mean(xold)
sdev_o <- sd(xold)
n2 <- length(xold)

d <- xnew - xold
xb <- mean(d)
sdev <- sd(d)
n <- length(d)
se <- sdev/sqrt(n)
tstat <- xb / se
pv <- pt(tstat, n - 1) #.1405

###
#Q5 - 95% CI (2-sided) for data from Q4
###

tcrit <- qt(.975, n - 1)
xb - tcrit * se
xb + tcrit * se

#oops, forgot to exponentiate the boundaries...
exp(xb - tcrit * se) #.09
exp(xb + tcrit * se) #2.49




###
#Q6 
###

#treatment group
n1 <- 9
xbar1 <- -3
sdev1 <- 1.5

#placebo group
n2 <- 9
xbar2 <- 1
sdev2 <- 1.8

#assuming common variance and normal populations
sp <- sqrt(.5*(sdev1^2 + sdev2^2))
se <- sp * sqrt(1/n1 + 1/n2)
tstat <- (xbar1 - xbar2)/ se #-5.12
pval <- pt(tstat,n1+n2-2) * 2 #.0001 <--- ooops1, forgot to double pval for 2-sided test


###
#Q8 - sample size cal for given power
###
pow <- .9
d <- .01
sdev <- .04
alpha <- .05
zcrit <- qnorm(1 - alpha)
z10 <- qnorm(.1)
n <- ((zcrit - z10) * sdev / d)^2 #about 137


###
#Q9 - power
###

#control sample (Gotham)
n1 <- 288
sdev1 <- 12

#Metropolis sample
n2 <- 288
sdev2 <- 12

d <- 2
sp <- sqrt(.5*(sdev1^2 + sdev2^2))
se <- sp *sqrt(1/n1 + 1/n2)
alpha <- .05
zcrit <- qnorm(1 - alpha) #1.645
z_std_mua <- d/ se
pow <- pnorm(zcrit - z_std_mua, lower.tail = F) #.63


