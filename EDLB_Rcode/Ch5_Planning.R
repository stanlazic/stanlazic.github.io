## ----setup, include=FALSE, cache=FALSE-----------------------------------
require(knitr)

opts_chunk$set(
    fig.path = "knitr_figs/",
    fig.height = 3,
    fig.width = 4,
    out.width = "0.98\\textwidth", 
    fig.keep = "high",
    fig.show = "hold",
    fig.align = "center",
    fig.pos = "htb",
    warning = FALSE,
    error = FALSE,
    prompt = TRUE,
    comment = NA,
    highlight = FALSE,
    background = "#F7F7F7",
    size = "small",
    strip.white=FALSE,
    cache = TRUE,
    cache.path = "cache_optimal/")

opts_knit$set(progress = TRUE,
              verbose = TRUE,
              unnamed.chunk.label = "CHUNK",
              width = 70)
options(show.signif.stars=FALSE)

## ----echo=FALSE----------------------------------------------------------
rm(list=ls())

## ------------------------------------------------------------------------
library(labstats)
head(CV)

## ----reliability_2vars,  out.width = "0.98\\textwidth", fig.height = 3.25, fig.width = 9, fig.cap='Two outcome variables, A and B, are measured on five samples and are highly correlated. Three measurements are taken on each sample (subsamples) to estimate the measurement error and are plotted by sample number. Which variable would make a better primary outcome?'----
# load required packages
library(lattice)

p1 <- xyplot(B ~ A, data=CV, col="black",
             xlab="Outcome A", ylab="Outcome B")
p2 <- dotplot(A ~ Sample, data=CV, col="black",
              xlab="Sample", ylab="Outcome A")
p3 <- dotplot(B ~ Sample, data=CV, col="black",
              xlab="Sample", ylab="Outcome B")

trellis.par.set(axis.components=list(right=list(tck=0),
                                     top=list(tck=0)))
print(p1, split=c(1,1,3,1), more=TRUE)
print(p2, split=c(2,1,3,1), more=TRUE)
print(p3, split=c(3,1,3,1))

## ----reliability_2vars_red,  out.width = "0.6\\textwidth", fig.height = 4.5, fig.width = 4.5, fig.cap='Coefficient of variation for three measurements on five samples for two outcome variables. Outcome B is preferred because the measurements are more reliable.'----
library(plyr)
# calculate the CV for both variables
cv.red <- ddply(CV, .(Sample), summarize,
                CV.A = sd(A)/mean(A),
                CV.B = sd(B)/mean(B))

cv.red

par(las=1)
matplot(t(cv.red[,2:3]), xlim=c(0.5,2.5), pch=16,
        col="black", ylab="CV", xaxt="n",
        xlab="Outcome variable")

axis(1, at=1:2, labels=c("A","B"))
# add lines to connect samples
segments(rep(1,5), cv.red[,2], rep(2,5), cv.red[,3])

## ------------------------------------------------------------------------
head(goldstandard)

## ----reliability_goldstandard, echo=FALSE,  out.width = "0.98\\textwidth", fig.height = 6.5, fig.width = 5.75, fig.cap='Comparing a new variable with a gold standard. r = Pearson correlation; CCC = Lin\'s concordance correlation coefficient'----
par(mfrow=c(2,2),
    las=1,
    mar=c(4,4,4,1))
plot(perfect.cor ~ goldstandard, data=goldstandard,
     xlim=c(0,28), ylim=c(0,28), xlab="Gold standard",
     ylab="New", main="Perfect reliability")
abline(0,1)
mtext("A", side=3, line=2, font=2, adj=0)
text(x=28, y=1, labels=c("r = 1.0\n\n","CCC = 1.0"), adj=1)

plot(noise ~ goldstandard, data=goldstandard,
     xlim=c(0,28), ylim=c(0,28), xlab="Gold  Standard",
     ylab="New", main="Noise")
abline(0,1)
mtext("B", side=3, line=2, font=2, adj=0)
text(x=28, y=1, labels=c("r = 0.988\n\n","CCC = 0.988"), adj=1)

plot(location ~ goldstandard, data=goldstandard,
     xlim=c(0,28), ylim=c(0,28), xlab="Gold standard",
     ylab="New", main="  Noise + location shift")
abline(0,1)
mtext("C", side=3, line=2, font=2, adj=0)
text(x=28, y=1, labels=c("r = 0.988\n\n","CCC = 0.950"), adj=1)

plot(scale ~ goldstandard, data=goldstandard,
     xlim=c(0,28), ylim=c(0,28), xlab="Gold standard",
     ylab="New", main="Noise + scale shift")
abline(0,1)
mtext("D", side=3, line=2, font=2, adj=0)
text(x=28, y=1, labels=c("r = 0.993\n\n","CCC = 0.876"), adj=1)

## ------------------------------------------------------------------------
# function to calculate CCC
lin.cor <- function(x,y){
    cov.xy <- cov(x,y)
    var.x <- var(x)
    var.y <- var(y)
    mean.x <- mean(x)
    mean.y <- mean(y)

    2*cov.xy / (var.x + var.y + (mean.x - mean.y)^2)
}


# compare Pearson correlations with CCC
with(goldstandard, cor(goldstandard, noise))
with(goldstandard, cor(goldstandard, location))
with(goldstandard, cor(goldstandard, scale))

with(goldstandard, lin.cor(goldstandard, noise))
with(goldstandard, lin.cor(goldstandard, location))
with(goldstandard, lin.cor(goldstandard, scale))

## ----echo=FALSE----------------------------------------------------------
trellis.par.set(axis.components=list(right=list(tck=0),
                                     top=list(tck=0)))

## ----reliability_goldstandard_tmd, out.width = "0.98\\textwidth", fig.height = 3.5, fig.width = 6, fig.cap='Tukey mean-difference plot for comparing measurements with a gold-standard.'----
p1 <- tmd(xyplot(location ~ goldstandard, data=goldstandard,
                 main="Noise + location shift"),
          ylim=c(-1,5), col="black")
p2 <- tmd(xyplot(scale ~ goldstandard, data=goldstandard,
                 main="Noise + scale shift"),
          ylim=c(-1,5), col="black")

print(p1, split=c(1,1,2,1), more=TRUE)
print(p2, split=c(2,1,2,1))

## ------------------------------------------------------------------------
head(assay.window)

## ----assay_window, out.width = "0.98\\textwidth", fig.height = 4.5, fig.width = 7, fig.cap='Assay window. Outcome A has a smaller difference between means, but less variability within the two groups, compared with Outcome B. Which outcome has a better assay window?'----
library(beeswarm)
par(mfrow=c(1,2),
    mar=c(4,4,2,1),
    las=1)

beeswarm(A ~ condition, data=assay.window, ylab="Value",
         method="center", pch=16, ylim=c(0,40), main="Outcome A")
beeswarm(B ~ condition, data=assay.window, ylab="Value",
         method="center", pch=16, ylim=c(0,40), main="Outcome B")

## ------------------------------------------------------------------------
# calculate mean and variance for Outcome A
with(assay.window, tapply(A, condition, mean))
with(assay.window, tapply(A, condition, var))

# standardised effect size
abs(10.390 - 14.985) / sqrt((3.31253 + 6.86134)/2)


# calculate mean and variance for Outcome B
with(assay.window, tapply(B, condition, mean))
with(assay.window, tapply(B, condition, var))

# standardised effect size
abs(10.695 - 25.815) / sqrt((16.3310 + 70.4434)/2)

## ------------------------------------------------------------------------
# Outcome A in assay.window data set
power.t.test(n = NULL, delta = 4.595, sd = 2.25542,
             sig.level = 0.05, power = 0.8)

## ------------------------------------------------------------------------
# Outcome B in assay.window data set
power.t.test(n = NULL, delta = 15.12, sd = 6.5869,
             sig.level = 0.05, power = 0.8)

## ------------------------------------------------------------------------
# Outcome A with smallest relevant difference
power.t.test(n = NULL, delta = 3, sd = 2.25542,
             sig.level = 0.05, power = 0.8)

## ------------------------------------------------------------------------
# Outcome A for power
power.t.test(n = 6, delta = 3, sd = 2.25542,
             sig.level = 0.05, power = NULL)

## ------------------------------------------------------------------------
# Outcome A: calculate minimum detectable effect
power.t.test(n = 8, delta = NULL, sd = 2.25542,
             sig.level = 0.05, power = 0.8)


## ------------------------------------------------------------------------
# various sample sizes, SDs, and mean differences
pow <- expand.grid(N=seq(5, 40, 2),
                   SD=seq(1.85, 2.65, 0.2),
                   diff=c(1.5, 2),
                   power=NA)

head(pow)

dim(pow)

## ------------------------------------------------------------------------
# Calculate power for each combination
for (i in 1:nrow(pow)){
    pow$power[i] <- power.t.test(delta = pow$diff[i], 
                                 n=pow$N[i], sd = pow$SD[i],
                                 sig.level = 0.05)$power
}

## ----echo=FALSE----------------------------------------------------------
trellis.par.set(list(superpose.line=list(lty=1:5, col="black"),
                     axis.components=list(right=list(tck=0),
                         top=list(tck=0))))

## ----power_complex, out.width = "0.98\\textwidth", fig.height = 4, fig.width = 7, fig.cap='Power as a function of the sample size, effect size, and within-group variability (SD).'----
xyplot(power ~ N|factor(diff), data=pow, groups=SD, type="l",
       ylab="Power", xlab="Sample size per group",
       ylim=c(0,1), between=list(x=1),
       strip = strip.custom(var.name=c("Effect size"), strip.names=TRUE),
       scales=list(alternating=FALSE), 
       auto.key=list(columns=1, space="right",
           points=FALSE, lines=TRUE, title="SD"))

## ------------------------------------------------------------------------
set.seed(1)
# generate values for mean difference and within group SD
delta <- rnorm(5000, 4.6, 0.05)
sigma <- rnorm(5000,  2.3, 0.3)

# combine into a data frame
vals <- data.frame(delta=delta, sd=sigma)

head(vals)

## ------------------------------------------------------------------------
# calculate samp size for each combination of effect size
# and within group SD
res <- apply(vals, 1, function (x){
                 power.t.test(delta = x[1], sd = x[2],
                              power=0.8)$n })

## ----bayes_power, out.width = "0.98\\textwidth", fig.height = 3.25, fig.width = 7, fig.cap='Simulation-based sample size calculations. The uncertainty in the effect size and variability is represented with a normal distribution. The number of samples required also has a distribution from which we can calculate useful summaries. Black triangles are the values from the standard sample size calculation.'----
par(mfrow=c(1,3),
    mar=c(4.5,1.5,3,0.5))
hist(delta, breaks=20, col="grey", border="white",
     main="Uncertainty in ES", xlab="Effect size (ES)",
     ylab="", yaxt="n")
points(x=4.595, y=-20, pch=17, cex=2)

hist(sigma, breaks=20, col="grey", border="white",
     main="Uncertainty in SD", xlab="Within group SD",
     ylab="", yaxt="n")
points(x=2.255, y=-20, pch=17, cex=2)

hist(res, breaks=20, col="grey", border="white",
     main="Uncertainty in n", xlab="Sample size per group",
     ylab="", yaxt="n", xlim=c(3,9))
points(x=4.95, y=-20, pch=17, cex=2)

## ------------------------------------------------------------------------
# how many samples to ensure 90% prob of 80% power?
quantile(res, 0.9)

## ------------------------------------------------------------------------
                                                      # Panel in graph
x1 <- rep(c(0,8,16,24), each=5)                       # A
x2 <- rep(c(0,8,16,24), times=c(8,4,4,4))             # B
x3 <- rep(c(0,24), each=10)                           # C
x4 <- rep(c(0,8,16,24), times=c(7,3,3,7))             # D
x5 <- rep(c(0,8,16,24), times=c(3,7,7,3))             # E
x6 <- rep(c(0,12,24), times=c(7,6,7))                 # F
x7 <- rep(round(seq(0,24, length.out=10),0), each=2)  # G
x8 <- round(seq(0,24, length.out=20),1)               # H
z1 <- rep(c(1,8,1,8), each=5) # time variable         # H

## ----eval=FALSE----------------------------------------------------------
## library(BHH2)
## dotPlot(x1, pch=16, xlab="Dose", base=FALSE, axes = FALSE)
## axis(1, at=c(0,8,16,24))

## ----design_points_samp_alloc, echo=FALSE, out.width = "0.99\\textwidth", fig.height = 6, fig.width = 6, fig.cap='Nine designs with twenty samples (black dots). All designs have the same design space for Dose (0--24 mg/kg) but different design points and number of samples per design point. Design (I) has Time as an additional variable.'----
library(BHH2)
par(mfrow=c(3,3),
    mar=c(4.5,2,0.1,1),
    las=1)

dotPlot(x1, pch=16, xlab="Dose", base=FALSE, axes = FALSE)
axis(1, at=c(0,8,16,24))
text(x=1, y=0.75, labels="A", font=2, cex=1.5)

dotPlot(x2, pch=16, xlab="Dose", base=FALSE, axes = FALSE)
axis(1, at=c(0,8,16,24))
text(x=1, y=0.75, labels="B", font=2, cex=1.5)

dotPlot(x3, pch=16, xlab="Dose", base=FALSE, axes = FALSE)
axis(1, at=c(0,24))
text(x=1, y=0.75, labels="C", font=2, cex=1.5)

dotPlot(x4, pch=16, xlab="Dose", base=FALSE, axes = FALSE)
axis(1, at=c(0,8,16,24))
text(x=1, y=0.75, labels="D", font=2, cex=1.5)

dotPlot(x5, pch=16, xlab="Dose", base=FALSE, axes = FALSE)
axis(1, at=c(0,8,16,24))
text(x=1, y=0.75, labels="E", font=2, cex=1.5)

dotPlot(x6, pch=16, xlab="Dose", base=FALSE, axes = FALSE)
axis(1, at=c(0,12,24))
text(x=1, y=0.75, labels="F", font=2, cex=1.5)

dotPlot(x7, pch=16, xlab="Dose", base=FALSE, axes = FALSE)
axis(1, at=c(0,8,16,24))
text(x=1, y=0.75, labels="G", font=2, cex=1.5)

dotPlot(x8, pch=16, xlab="Dose", base=FALSE, axes = FALSE)
axis(1, at=c(0,8,16,24))
text(x=1, y=0.75, labels="H", font=2, cex=1.5)

set.seed(1)
plot(jitter(z1, factor=0.3) ~ jitter(x3, factor=0.3), xaxt="n", yaxt="n",
     xlim=c(-8,32), ylim=c(0,10), pch=16, bty="n", xlab="Dose")
axis(1, at=c(0,24))
axis(4, at=c(1,8), line=-1)
text(x=-7.5, y=8, labels="I", font=2, cex=1.5)
par(las=0)
mtext("Time    ",side=4, line=-0.5, cex=0.8)

## ------------------------------------------------------------------------
# sample sizes for two groups
n1 <- 10:18
n2 <- 10:2

# confirm that total is 20
n1 + n2

## ----var_of_estimator, out.width = "0.99\\textwidth", fig.height = 4, fig.width = 6, fig.cap='Efficiency of estimating a mean difference. The $y$-axis is the variance of the estimate and lower values are better. An 18:2 sample allocation requires 1.7 times more samples compared with a 10:10 allocation.'----
# variance of the estimator
v <- sqrt((1/n1 + 1/n2))

par(las=1,
    mar=c(5.1,7,1,1))
plot(v ~ n1, type="b", xaxt="n", xlab="",
     ylab="")
axis(1, at=n1)
axis(1, at=n1, labels=n2, line=1.5, tick=FALSE)
axis(1, at=9, labels=~n[1], xpd=TRUE,
     tick=FALSE, font=2)
axis(1, at=9, labels=~n[2], xpd=TRUE,
     tick=FALSE, font=2, line=1.5)

mtext(expression(sqrt(frac(1,n[1]) + frac(1,n[2]))), side=2, line=4)
mtext("Balance of samples", side=1, line=4)

## ------------------------------------------------------------------------
# Relative efficiency
v[1]/v[9]

# number of additional samples
v[9]/v[1]

## ------------------------------------------------------------------------
t <- 3 # number of treated groups
k <- sqrt(t) # scaling factor
n.t <- 2:10  # various sample sizes for treated groups
n.c <- round(k*n.t) # number in control group
N <- n.c + t*n.t # total sample size

# combine into data frame
(data.frame(n.t, n.c, N))

## ------------------------------------------------------------------------
# calculate variance of predictor
var(x1) # A
var(x3) # C
var(x6) # F

# RE
var(x1)/var(x3) # four vs. two group

var(x6)/var(x3) # three vs. two group

## ------------------------------------------------------------------------
var(x1) # A
var(x4) # D
var(x5) # E

# RE
var(x1)/var(x4)

var(x5)/var(x4)

## ----range_of_X, echo=FALSE, out.width = "0.98\\textwidth", fig.height = 4, fig.width = 7, fig.cap='How the range of a predictor variable (age) affects the efficiency. Both graphs have the same number of samples and the same underlying relationship between the outcome and age (slope = 1). With a narrow age range, three times as many samples are required to have the same power as with a wide age range.'----
set.seed(1)
x1 <- runif(100, 20, 80)
y1 <- rnorm(100, 10 + x1, 10)

x2 <- runif(100, 40, 60)
y2 <- rnorm(100, 10 + x2, 10)

par(mfrow=c(1,2),
    las=1,
    mar=c(4.1,4,3,1))
plot(y1 ~ x1, xlab="Age (y)", ylab="Outcome",
     xlim=c(15, 85), ylim=c(15, 110),
     main="N = 100; Wide age range")

plot(y2 ~ x2, xlab="Age (y)", ylab="Outcome",
     xlim=c(15, 85), ylim=c(15, 110),
     main="N = 100; Narrow age range")

## ----dichot, echo=FALSE, out.width = "0.98\\textwidth", fig.height = 5.5, fig.width = 7, fig.cap='Binning a continuous variable. Two studies (panels A--C and D--F) perform a median split (vertical dashed bar) of the biomarker to define two groups. The groups are tested if they differ on disease severity. The groups are not comparable between studies because the cutoff for the median is different. The relationship between the biomarker and disease severity is the same in both studies, they only differ in the distribution of the biomarker variable.'----
library(sciplot)
set.seed(1)
x <- rgamma(50, shape=0.5, rate=0.25)
y <- 10 + 1.1*x + rnorm(50, 0, 5)


x2 <- rgamma(50, shape=2, rate=0.25)
y2 <- 10 + 1.1*x2 + rnorm(50, 0, 5)

# median split
grp <- factor(ifelse(x < median(x), "Low", "High"), levels=c("Low","High"))
grp2 <- factor(ifelse(x2 < median(x2), "Low", "High"), levels=c("Low","High"))

par(mfrow=c(2,3),
    mar=c(6,4,3,1),
    cex.lab=1.1, cex.axis=1.2)

dotPlot(x, pch=16, xlab="Level of biomaker",
        xlim=c(0,25), cex=1)
abline(v = median(x), lty=2)
mtext("Study 1", side=2, line=1.5, font=2)
mtext("A", side=3, line=1, font=2, adj=0, cex=1.1)

par(las=1)

bargraph.CI(grp, y, ylab="Disease severity",
            ylim=c(0, 25), xlab="Level of biomaker")
mtext("B", side=3, line=1, font=2, adj=0, cex=1.1)

plot(y ~ x, ylab="Disease severity",
     xlab="Level of biomaker")
abline(v = median(x), lty=2)
mtext("C", side=3, line=1, font=2, adj=0, cex=1.1)

par(las=0)

dotPlot(x2, pch=16, xlab="Level of biomaker",
        xlim=c(0,25), cex=1)
abline(v = median(x2), lty=2)
mtext("Study 2", side=2, line=1, font=2)

par(las=1)
mtext("D", side=3, line=1, font=2, adj=0, cex=1.1)

bargraph.CI(grp2, y2, ylab="Disease severity",
            ylim=c(0, 25), xlab="Level of biomaker")
mtext("E", side=3, line=1, font=2, adj=0, cex=1.1)

plot(y2 ~ x2, ylab="Disease severity",
     xlab="Level of biomaker")
abline(v = median(x2), lty=2)
mtext("F", side=3, line=1, font=2, adj=0, cex=1.1)

## ------------------------------------------------------------------------
# create another variable 
z2 <- rep(c(1,8,1,8), times=c(3,7,7,3))

xtabs(~ x3 + z2)

## ------------------------------------------------------------------------
# correlation between predictors with equal n
cor.test(x3 , z1)

# correlation between predictors with unequal n
cor.test(x3 , z2)

## ------------------------------------------------------------------------
# change default contrasts
options(contrasts=c("contr.sum","contr.poly"))
 
# design matrix for main and interaction effects
des1 <- model.matrix(~ factor(x3) * factor(z1)) # balanced
des2 <- model.matrix(~ factor(x3) * factor(z2)) # unbalanced

## ------------------------------------------------------------------------
# covariance matrix
cov.des1 <- solve(t(des1) %*% des1)
cov.des2 <- solve(t(des2) %*% des2)

# remove row and column names to save space
dimnames(cov.des1) <- NULL
dimnames(cov.des2) <- NULL

## ------------------------------------------------------------------------
# examine covariances
round(cov.des1,2)

round(cov.des2,2)

## ------------------------------------------------------------------------
# Calculate A-optimality for both designs
sum(diag(cov.des1)) # balanced

sum(diag(cov.des2)) # unbalanced

# calculate RE
0.2 / 0.238095

## ----autocor, echo=FALSE, out.width = "0.65\\textwidth", fig.height = 5, fig.width = 5, fig.cap='Correlation between time points of different lags. As the distance between time points increases the correlation gets weaker.'----
kh.wide <- unstack(KH2004[KH2004$cond=="Placebo", ], values~time)

cor.wide <- cor(kh.wide[, -1])

par(las=1)
plot.new()
plot.window(xlim=c(1,14), ylim=c(0.5,1))
axis(1, at=seq(1,13,2)); axis(2); box()
mtext("Lag", side=1, line=3)
par(las=0)
mtext("Correlation", side=2, line=3)

for (i in 1:13){
    for (j in 1:13){
        if (i + j <= 14) {
        points(cor.wide[i, (i+j)] ~ j, cex=0.8)
    }
    }
}

abline(h=0.9, lty=2)

## ----pk_example, out.width = "0.6\\textwidth", fig.height = 5, fig.width = 5, fig.cap='Concentration of a compound in the blood over time.'----
# make sequence of observations
t <- seq(0, 24, 0.1)

# define values for the rates
k1 <- 0.2 # /hour
k2 <- 3   # /hour

# two compartment model with first-order kinetics
y <- exp(-k1 * t) - exp(-k2 * t)

par(las=1)
plot(y ~ t, type="l", ylab="Concentration", xlab="Time (h)",
     lwd=1.5)

## ------------------------------------------------------------------------
# make time values in 1 hour increments
t.vals <- 0:24

## ------------------------------------------------------------------------
# Define the PK model
pk.mod <- expression(exp(-k1 * t) - exp(-k2 * t))

# calculate partial derivatives with respect to each parameter
D(pk.mod, "k1")

D(pk.mod, "k2")

## ------------------------------------------------------------------------
# matrix to store results
res <- matrix(NA, nrow=25, ncol=2)

# evaluate partial derivs. at each time point
for(i in 1:25){
    res[i,1] <- -(exp(-k1 * t.vals[i]) * t.vals[i])
    res[i,2] <- exp(-k2 * t.vals[i]) * t.vals[i]
}

# put everything into one matrix
res <- cbind(t.vals, res)
colnames(res) <- c("t.vals","k1","k2")
head(res)

## ------------------------------------------------------------------------
library(AlgDesign)
set.seed(1)
best.des <- optFederov(~ 0 + k1 + k2, data=data.frame(res),
                       nTrials=4, criterion="I", nRepeats=100,
                       row=c(1,25), augment=TRUE)

## ------------------------------------------------------------------------
best.des$design

## ----pk_curves, out.width = "0.6\\textwidth", fig.height = 5, fig.width = 5, fig.cap='The PK curve shows the concentration of the drug in the blood over 24 hours. With an initial and final time point selected at 0 and 24 hours, two more time points are required. The optimal values (circles) occur at 1 and 5 hours. Spacing the observations out equally puts them at 8 and 16 hours, but the early changes are missed.'----
# make equally spaced points and the optimal points
eq.space <- seq(0,24, length.out=4)
best <- best.des$design$t.vals

par(las=1)
plot(y ~ t, type="l",  ylab="Concentration", xlab="Time (h)",
     lwd=1.5)

points(exp(-k1 * eq.space) - exp(-k2 * eq.space) ~ eq.space,
       pch=4, cex=2) 
points(exp(-k1 * best) - exp(-k2 * best) ~ best,
       pch=1, cex=2)

legend("topright", pch=c(1,4), cex=1.2,
       legend=c("Optimal","Equal spacing"))

## ------------------------------------------------------------------------
block.covars

## ----block_covars, echo=FALSE, out.width = "0.98\\textwidth", fig.height = 6, fig.width = 8, fig.cap='Blocking versus ANCOVA. The top row of graphs are from a randomised block design where rats are paired according to baseline body weight and one member of each pair is randomised to each treatment group. The bottom row is a completely randomised design and the effect of baseline body weight on the outcome is removed statistically. The range of the $y$-axes is 60 units in all graphs, so the range bars can be directly compared.'----
adj.y <- resid(lm(block.covars$y.CRD~block.covars$weight))

par(las=1,
    mfrow=c(2,3),
    mar=c(4,4,3,1))
# RBD
plot(y.RBD ~ as.numeric(RBD), data=block.covars,
     ylab="Outcome", xlab="", pch=16,
     xlim=c(0.5, 2.5), ylim=c(240, 300), xaxt="n")
axis(1, at=1:2, labels=levels(block.covars$RBD))

segments(1, 260.121, 2,  275.605, lwd=2)
arrows(0.85, 247.186, 0.85, 280.503, length=0.05,
       angle=90, code=3)
arrows(2.15, 263.562, 2.15, 289.976, length=0.05,
       angle=90, code=3)
mtext("A", side=3, font=2, cex=1.2, adj=0, line=1.5)

# RBD
matplot(t(cbind(block.covars$y.RBD[block.covars$RBD=="Control"],
                block.covars$y.RBD[block.covars$RBD=="New Diet"])),
        type="b", col="black", lty=1, 
        ylab="Outcome", xlab="", pch=16,
        xlim=c(0.5, 2.5), ylim=c(240, 300), xaxt="n")
axis(1, at=1:2, labels=levels(block.covars$RBD))
mtext("B", side=3, font=2, cex=1.2, adj=0, line=1.5)

# RBD
plot(I(block.covars$y.RBD[block.covars$RBD=="New Diet"] -
           block.covars$y.RBD[block.covars$RBD=="Control"]) ~ rep(1,4),
     ylim=c(-10, 50), xaxt="n", pch=16,
     ylab="Difference (New Diet - Control)", xlab="")
abline(h=0, lty=2)

arrows(1.05, 9.47299, 1.05, 25.4878, length=0.05,
       angle=90, code=3)

mtext("C", side=3, font=2, cex=1.2, adj=0, line=1.5)

# CRD
plot(y.CRD ~ as.numeric(CRD), data=block.covars,
     ylab="Outcome", xlab="", pch=16,
     xlim=c(0.5, 2.5), ylim=c(240, 300), xaxt="n")
axis(1, at=1:2, labels=levels(block.covars$RBD))

segments(1, 259.746, 2, 271.186 , lwd=2)
arrows(0.85, 251.097, 0.85, 268.720, length=0.05,
       angle=90, code=3)
arrows(2.15, 257.238, 2.15, 295.585, length=0.05,
       angle=90, code=3)
mtext("D", side=3, font=2, cex=1.2, adj=0, line=1.5)

# CRD
plot(y.CRD ~ weight, data=block.covars,
     pch=ifelse(block.covars$CRD=="Control", 16, 2),
     ylim=c(240, 300), xlim=c(210, 280), ylab="Outcome",
     xlab="Weight")
mtext("E", side=3, font=2, cex=1.2, adj=0, line=1.5)
abline(a=60.441 , b=0.847)
legend("topleft", pch=c(16,2), legend=levels(block.covars$CRD))

# CRD
plot(adj.y ~ as.numeric(CRD),
     data=block.covars,
     ylab="Adjusted outcome", xlab="", pch=16,
     xlim=c(0.5, 2.5), ylim=c(-30, 30), xaxt="n")
axis(1, at=1:2, labels=levels(block.covars$RBD))

segments(1, -6.33569, 2, 6.33569, lwd=2)
arrows(0.85, -11.78901, 0.85, -2.07762, length=0.05,
       angle=90, code=3)
arrows(2.15, 1.40489, 2.15, 11.51370, length=0.05,
       angle=90, code=3)
mtext("F", side=3, font=2, cex=1.2, adj=0, line=1.5)

## ------------------------------------------------------------------------
summary(aov(y.RBD ~ RBD, data=block.covars))

## ------------------------------------------------------------------------
summary(aov(y.RBD ~ factor(block) + RBD, data=block.covars))

## ------------------------------------------------------------------------
summary(aov(y.CRD ~ CRD, data=block.covars))

## ------------------------------------------------------------------------
summary(aov(y.CRD ~ weight + CRD, data=block.covars))

## ------------------------------------------------------------------------
block.covars$adj.y <- resid(aov(y.CRD ~ weight, data=block.covars))
block.covars

## ------------------------------------------------------------------------
# generate layout
d <- expand.grid(Drug=c("-","+"), Duration=c("-","+"),Sex=c("-","+"),
                 Litter=LETTERS[1:2])

# add animal ID, cage, and reorder columns
d <- data.frame("Animal ID"=1:nrow(d),
                Cage=rep(1:2, each=8), d[,4:1] )

d

## ------------------------------------------------------------------------
# make the layout
d2 <- expand.grid(Drug=c("-","+"), Duration=c("-","+"),Sex=c("-","+"),
                 Litter=LETTERS[1:3])

# add animal ID, cage, and reorder columns
d2 <- data.frame("Animal ID"=1:nrow(d2),
                 Cage=rep(1:3, each=8), d2[,4:1] )

d2

## ------------------------------------------------------------------------
xtabs(~Litter+Drug, data=d)

xtabs(~Sex+Duration, data=d)

xtabs(~Litter + Duration, d)

