
## ----echo=FALSE----------------------------------------------------------
rm(list=ls())

## ------------------------------------------------------------------------
y <- c(6, 2, 3, 1, 4, 8, 7, 9)
x <- factor(rep(c("A", "B"), each=4))
data.frame(y, x)

## ----echo=FALSE----------------------------------------------------------

mean.y <- mean(y)
mean.A <- mean(y[1:4])
mean.B <- mean(y[5:8])

# calculating SS
tss <- sum((y - mean.y)^2)

# Residual SS
rss <- sum((y[1:4] - mean.A)^2) + sum((y[5:8] - mean.B)^2)

# SS due to Group
ssx <- tss - rss


library(beeswarm)
par(las=1,
    mar=c(4,4,3,1),
    mfrow=c(2,2))

beeswarm(y ~ x, pch=16, ylim=c(0,11),
         method="center", xlab="")
mtext("A", side=3, line=1, adj=0, font=2, cex=1.5)

plot(y, pch=16, ylim=c(0,11), xlab="",
     main=~Overall~mean, xaxt="n")
abline(h=mean.y)
axis(1, at=1:8, labels=x)
segments(1:8, y, 1:8, mean(y))

mtext("B", side=3, line=1, adj=0, font=2, cex=1.5)


plot(y, pch=16, ylim=c(0,11), xlab="",
     main=~Group~means, xaxt="n")
axis(1, at=1:8, labels=x)
segments(1, mean.A, 4, mean.A)
segments(5, mean.B, 8, mean.B)

segments(1:4, y[1:4], 1:4, mean.A)
segments(5:8, y[5:8], 5:8, mean.B)

mtext("C", side=3, line=1, adj=0, font=2, cex=1.5)


barplot(c(tss, ssx, rss), ylim=c(0,65),
        names=c("Total","Group","Residual"), ylab="SS",
        main=~Partitioning~the~SS)

mtext("D", side=3, line=1, adj=0, font=2, cex=1.5)

## ------------------------------------------------------------------------
mean.y <- mean(y)
tss <- sum((y - mean.y)^2)
tss

## ------------------------------------------------------------------------
# mean differences are positive and negative
y - mean.y

# sum without squaring equals zero
sum(y - mean.y)

# squaring the deviations makes them positive
(y - mean.y)^2

## ------------------------------------------------------------------------
mean.A <- mean(y[1:4])
mean.A

mean.B <- mean(y[5:8])
mean.B

# Residual SS
rss.A <- sum((y[1:4] - mean.A)^2)
rss.B <- sum((y[5:8] - mean.B)^2)
rss <- rss.A + rss.B
rss

## ------------------------------------------------------------------------
# SS due to Group
ssx <- tss - rss
ssx

## ----eval=FALSE----------------------------------------------------------
## outcome ~ A + B + C + A:B + A:C + B:C + A:B:C

## ----eval=FALSE----------------------------------------------------------
## model <- aov(outcome ~ A*B) # example model
## summary(model) # Type I
## anova(model) # Type I
## car::Anova(model) # Type II
## car::Anova(model, type="III") # Type III

## ------------------------------------------------------------------------
simple.mod <- aov(y ~ x)
summary(simple.mod)

## ------------------------------------------------------------------------
pf(6.86, df1=1, df=6,  lower.tail = FALSE)

## ------------------------------------------------------------------------
set.seed(123)
y.rand <- rnorm(7*5, 2) # 7 groups with 5 samples each
g <- gl(7, 5, labels=LETTERS[1:7]) # grouping variable

library(sciplot)
par(las=1)
bargraph.CI(g, y.rand, xlab="Group", ylab="Outcome",
            ylim=c(0,3))
box()

## ------------------------------------------------------------------------
# overall anova
summary(aov(y.rand ~ g))

# t-test between two groups
t.test(y.rand[g=="E"], y.rand[g=="G"], var.equal=TRUE)

## ----echo=FALSE----------------------------------------------------------
par(las=1)
plot(I(1-0.95^c(1:80)) ~ c(1:80), type="l", xlab="Number of tests",
     ylab="P(At least one false positive)")

## ------------------------------------------------------------------------
# Fisher's LSD
pairwise.t.test(y.rand, g, p.adjust.method = "none",
                     pool.sd=TRUE)

## ------------------------------------------------------------------------
pairwise.t.test(y.rand, g, p.adjust.method = "bonferroni",
                     pool.sd=TRUE)

## ------------------------------------------------------------------------
summary(simple.mod)

## ------------------------------------------------------------------------
t.test(y ~ x, var.equal=TRUE)

## ------------------------------------------------------------------------
summary.lm(simple.mod)

## ------------------------------------------------------------------------
mean.B - mean.A

## ----echo=FALSE----------------------------------------------------------
par(mfrow=c(1,2),
    mar=c(5,4.5,4,2),
    las=1)

num.group <- as.numeric(x) - 1

bargraph.CI(x, y, ylab="Outcome", ylim=c(0, 11),
            main=expression(Mean %+-% SEM))

plot(y ~ num.group, ylab="Outcome", xlab="",
     ylim=c(0,11), xlim=c(-1,2), main=Raw~data)

segments(-0.25, mean.A, 0.25, mean.A, lwd=2)
segments(0.75, mean.B, 1.25, mean.B, lwd=2)

axis(1, at=0:1, labels=c("A","B"), line=1.5, tick=FALSE)
abline(lm(y ~ num.group), lty=2)

## ------------------------------------------------------------------------
library(labstats)
head(fluoxetine)

summary(fluoxetine)

## ----echo=FALSE----------------------------------------------------------
par(las=1,
    mar=c(4,4,1,1))
plot(time.immob ~ dose, data=fluoxetine, ylim=c(0, 250),
     xlab="Dose", ylab="Total immobility (s)",
     xaxt="n", xlim=c(-10, 250))
axis(1, at=c(0, 80, 160, 240))
points(tapply(fluoxetine$time.immob, fluoxetine$dose, mean) ~ c(0, 80, 160, 240),
       pch="-", cex=4)
abline(lm(time.immob ~ dose, data=fluoxetine))

## ------------------------------------------------------------------------
crd.mod1 <- aov(time.immob ~ factor(dose), data=fluoxetine)
summary(crd.mod1)

## ------------------------------------------------------------------------
crd.mod2 <- aov(time.immob ~ dose, data=fluoxetine)
summary(crd.mod2)

## ------------------------------------------------------------------------
anova(crd.mod2, crd.mod1)

## ------------------------------------------------------------------------
coef(summary.lm(crd.mod2))

confint(crd.mod2)

## ----eval=FALSE----------------------------------------------------------
## data(poisons, package="boot")

## ----include=FALSE-------------------------------------------------------
suppressMessages(data(poisons, package="boot"))

## ------------------------------------------------------------------------
summary(poisons)

## ------------------------------------------------------------------------
# reciprocal transformation
poisons$rate <- 1/poisons$time

par(mfrow=c(1,2),
    las=1)
bargraph.CI(poison, time, treat, data=poisons,
            legend=TRUE, xlab="Poison", ylab="Survival time",
            err.width=0.05, ylim=c(0,1))
bargraph.CI(treat, rate, poison, data=poisons,
            legend=TRUE, xlab="Treatment", ylab="Rate of death (1/time)",
            err.width=0.05, ylim=c(0,6))

## ------------------------------------------------------------------------
mod.2way <- aov(rate~treat*poison, data=poisons)
summary(mod.2way)

## ------------------------------------------------------------------------
par(las=1)
with(poisons, interaction.plot(poison, treat, rate, lwd=2))

## ------------------------------------------------------------------------
TukeyHSD(mod.2way, which="treat")

TukeyHSD(mod.2way, which="poison")

par(mfrow=c(1,2),
    las=1)
plot(TukeyHSD(mod.2way, which="treat"))
plot(TukeyHSD(mod.2way, which="poison"))

## ------------------------------------------------------------------------
# simple effect of treatment for poison 1
summary(aov(rate~treat, data=poisons, subset=poison=="1"))

## ------------------------------------------------------------------------
# p-value for simple effect (no pooling)
pf(7.23, 3, 12, lower.tail=FALSE)

# F-statistic with pooled MS residual
1.191/0.24

# p-value for simple effect (with pooling)
pf(4.9625, # F-statistic calculate above
   3,
   36,  # residual df from the full ANOVA
   lower.tail=FALSE)

## ------------------------------------------------------------------------
phia::testInteractions(mod.2way, across="treat", fixed="poison", 
                 adjustment="none")

## ------------------------------------------------------------------------
phia::testInteractions(mod.2way, across="poison", fixed="treat", 
                 adjustment="none")

## ------------------------------------------------------------------------
head(glycogen)

sapply(glycogen, class) # check whether variables are factors

# make numeric variables into factors
glycogen$Treatment <- factor(glycogen$Treatment)
glycogen$Rat <- factor(glycogen$Rat)
glycogen$Liver <- factor(glycogen$Liver)

summary(glycogen)

## ------------------------------------------------------------------------
xtabs(~ Treatment + Rat, data=glycogen)

## ------------------------------------------------------------------------
xtabs(~ Liver + Rat, data=glycogen)

## ------------------------------------------------------------------------
glycogen <- within(glycogen, {
            uniq.rat <- factor(paste(Treatment, Rat, sep="."))
            uniq.liver <- factor(paste(Treatment, Rat, Liver, sep="."))
                              })

# examine new variables
head(glycogen)

## ------------------------------------------------------------------------
xtabs(~ Treatment + uniq.rat, data=glycogen)

## ------------------------------------------------------------------------
xtabs(~ uniq.liver + uniq.rat, data=glycogen)

## ----echo=FALSE, include=FALSE, eval=TRUE--------------------------------
# used to suppress message in the chunk below
library(lattice)

## ----glycogen------------------------------------------------------------
library(lattice)
trellis.par.set(superpose.symbol=list(col="black", pch=1:3))

xyplot(Glycogen ~ Rat|Treatment, data=glycogen, groups=Liver,
       type=c("g","p"), jitter.x=TRUE, layout=c(3,1),
       scales=list(alternating=FALSE), between=list(x=1), 
       auto.key=list(columns=3, title="Liver piece"),
       strip = strip.custom(var.name="Treatment",
           strip.names = TRUE))

## ------------------------------------------------------------------------
# incorrect analysis
summary(aov(Glycogen ~ Treatment, data=glycogen))

## ------------------------------------------------------------------------
library(plyr)
glyc.red <- ddply(glycogen, .(Treatment, uniq.rat), summarize,
                  Glycogen = mean(Glycogen))

glyc.red

## ------------------------------------------------------------------------
xyplot(Glycogen ~ Treatment, data=glyc.red,  type=c("g","p"),
       col="black")

## ------------------------------------------------------------------------
# summary measure
summary(aov(Glycogen ~ Treatment, data=glyc.red))

## ------------------------------------------------------------------------
# indicate that the EU is the rat
summary(aov(Glycogen ~ Treatment + Error(uniq.rat), data=glycogen))

## ------------------------------------------------------------------------
# incorrect, Rat ID is not unique
summary(aov(Glycogen ~ Treatment + Error(Rat), data=glycogen))

## ------------------------------------------------------------------------
library(nlme)
lme.mod1 <- lme(Glycogen ~ Treatment, random=~1|uniq.rat, data=glycogen)
anova(lme.mod1)

## ------------------------------------------------------------------------
ranef(lme.mod1)

## ------------------------------------------------------------------------
data(cats, package="MASS")

# ratio of heart weight to body weight
cats$ratio <- cats$Hwt/cats$Bwt

summary(cats)

## ------------------------------------------------------------------------
par(mfrow=c(2,2),
    mar=c(4,4,2,1),
    las=1)
beeswarm(Hwt ~ Sex, data=cats, method="center", pch=16,
         cex=0.8, ylab="Heart weight")
beeswarm(Bwt ~ Sex, data=cats, method="center", pch=16,
         cex=0.8, ylab="Body weight")
beeswarm(ratio ~ Sex, data=cats, method="center", pch=16,
         cex=0.8, ylab="Heart/Body weight ratio")

plot(Hwt ~ Bwt, data=cats, subset=Sex=="M",
     ylim=c(5,21), xlim=c(1.75,4),
     cex=0.8, ylab="Heart weight", xlab="Body weight")
points(Hwt ~ Bwt, data=cats, subset=Sex=="F", pch=17, cex=0.8)

# add regression lines
abline(lm(Hwt ~ Bwt, data=cats, subset=Sex=="M"))
abline(lm(Hwt ~ Bwt, data=cats, subset=Sex=="F"), lty=2)

legend("topleft", legend=c("M","F"), pch=c(1,17), lty=1:2)

## ------------------------------------------------------------------------
summary(aov(Hwt ~ Sex, data=cats))

summary(aov(ratio ~ Sex, data=cats))

## ------------------------------------------------------------------------
summary(aov(Hwt ~ Bwt * Sex, data=cats))

## ------------------------------------------------------------------------
summary(aov(Hwt ~ Bwt + Sex, data=cats))

## ------------------------------------------------------------------------
summary(aov(Hwt ~  Sex + Bwt, data=cats))

## ------------------------------------------------------------------------
car::Anova(aov(Hwt ~  Sex + Bwt, data=cats))

## ------------------------------------------------------------------------
# select data from the first batch
f2 <- subset(festing, batch==1)
f2

## ------------------------------------------------------------------------
trellis.par.set(superpose.symbol=list(col="black", pch=1:4),
                superpose.line=list(col="black"),
                axis.components=list(right=list(tck=0),
                                     top=list(tck=0)))

xyplot(value~treatment, data=f2, groups=strain,
             auto.key=list(columns=4, title="Strain"),
             type=c("g","p","a"), scales=list(alternating=FALSE),
             xlab="", ylab="Gst")

## ------------------------------------------------------------------------
summary(aov(value ~ strain * treatment, data=f2))

## ------------------------------------------------------------------------
summary(aov(value ~ strain + treatment, data=f2))

## ------------------------------------------------------------------------
trellis.par.set(superpose.symbol=list(col="black", pch=1:4),
                superpose.line=list(col="black"),
                axis.components=list(right=list(tck=0),
                                     top=list(tck=0)))

p1 <- xyplot(value~treatment, data=festing, groups=strain,
             auto.key=list(columns=4, title="Strain"),
             type=c("g","p","a"), scales=list(alternating=FALSE),
             xlab="", ylab="Gst")

p2 <- dotplot(strain~value|treatment, data=festing, groups=batch,
	layout=c(1,2), strip.left=TRUE, strip=FALSE,
        auto.key=list(columns=2, title="Batch"), xlab="Gst")

print(p1, split=c(1,1,2,1), more=TRUE)
print(p2, split=c(2,1,2,1))

## ------------------------------------------------------------------------
summary(aov(value ~ batch + strain * treatment, data=festing))

## ------------------------------------------------------------------------
summary(VPA)

# set SAL as the baseline group (for graphs)
VPA$drug <- relevel(VPA$drug, "SAL")

xtabs(~litter+group, VPA)

## ----echo=FALSE----------------------------------------------------------
par(las=1, xpd=TRUE)
bargraph.CI(group, activity/10000, drug, data=VPA,
            ylim=c(0,10), legend=TRUE, xlab="Group",
            ylab="Locomotor activity (x10000)",
            y.leg=11, x.leg=5.5,
            main="Error bars are meaningless")

## ----echo=FALSE----------------------------------------------------------
trellis.par.set(superpose.symbol=list(col="black", pch=1:4),
                superpose.line=list(col="black", lty=1:2),
                axis.components=list(right=list(tck=0),
                                     top=list(tck=0)))

xyplot(activity/10000 ~ group, data=VPA, groups=drug,
       type=c("g","p","a"), col="black",
       xlab="Treatment", ylab="Locomotor activity (x10000)", jitter.x=TRUE,
       auto.key=list(columns=1, lines=TRUE))

## ----echo=FALSE----------------------------------------------------------
trellis.par.set(superpose.symbol=list(col="black", pch=1:4),
                superpose.line=list(col="black", lty=1:2),
                axis.components=list(right=list(tck=0),
                                     top=list(tck=0)))

## ------------------------------------------------------------------------
xyplot(activity/10000 ~ drug|group:reorder(litter,activity,mean),
       data=VPA, type=c("g","p","a"), col="black", layout=c(6,1),
       xlab="Treatment", ylab="Locomotor activity (x10000)",
       scales=list(alternating=FALSE), between=list(x=c(0,0,1,0,0)))

## ------------------------------------------------------------------------
summary(aov(activity ~ group*drug + Error(litter), data=VPA))

## ------------------------------------------------------------------------
VPA.red <- ddply(VPA, .(litter), summarise,
                 group=unique(group),
                 activity=mean(activity))

VPA.red

summary(aov(activity ~ group, data=VPA.red))

## ------------------------------------------------------------------------
VPA$adj.act <- resid(aov(activity ~ litter, data=VPA))

summary(aov(adj.act ~ group*drug , data=VPA))

## ------------------------------------------------------------------------
anova(lme(activity ~ group*drug, random=~1|litter, data=VPA))

## ------------------------------------------------------------------------
summary(KH2004)

KH2004$rat <- factor(KH2004$rat)

## ----echo=FALSE----------------------------------------------------------
trellis.par.set(superpose.symbol=list(col="black", pch=1:2),
                superpose.line=list(col="black", lty=1:2),
                axis.components=list(right=list(tck=0),
                                     top=list(tck=0)))

xyplot(values~time|rat, data=KH2004, groups=cond,
       type=c("g","p","a"),  xlab="Time", ylab="Relative force (%)",
       strip=strip.custom(var.name="Rat", strip.names=TRUE),
       scales=list(alternating=FALSE),
       auto.key=list(lines=TRUE))

## ----echo=FALSE----------------------------------------------------------
wide <- unstack(KH2004, values~rat)

pinacidil <- t(wide[1:15, ])
placebo <- t(wide[16:30, ])

# variances
var.pl <- diag(cov(placebo))
var.pina <- diag(cov(pinacidil))

par(las=1)
plot(var.pl ~ c(0:14), type="b", lwd=1.25,
     xlab="Time", ylab="Within group variance")
points(var.pina ~ c(0:14) , type="b",
       lwd=1.25, pch=2, lty=2)

legend("topright", legend=c("Placebo","Pinacidil"), #horiz = TRUE,
       lty=1:2, pch=1:2, bty="n")

## ----echo=FALSE----------------------------------------------------------
cors <- cor(placebo[,-1])
colnames(cors) <- 1:14
rownames(cors) <- 1:14
corrplot::corrplot(cors, method="ellipse", type="upper")

## ------------------------------------------------------------------------
summary(hypertension)

# convert Subject and Time into factors
hypertension$Subject <- factor(hypertension$Subject)
hypertension$fac.time <- factor(hypertension$Time)

xyplot(Y~fac.time|Diet, data=hypertension, groups=Subject,
       type=c("g","b"), scales=list(alternating=FALSE),
       ylab="Blood pressure", xlab="Time", col="black",
       lty=1, pch=1) 

## ------------------------------------------------------------------------
summary(aov(Y ~ fac.time*Diet + Error(Subject), data=hypertension))

## ------------------------------------------------------------------------
fits <- coef(lmList(Y ~ Time|Subject, data=hypertension))
head(fits)

slopes <- fits[,"Time"]

## ------------------------------------------------------------------------
# make new treatment factor
treatment <- gl(2, 5, labels=c("HighCa","LowCA"))

par(las=1)
beeswarm(slopes ~ treatment, pch=16)

## ------------------------------------------------------------------------
summary(aov(slopes ~ treatment))

## ------------------------------------------------------------------------
lme.mod1 <- lme(Y ~ fac.time * Diet, random=~1|Subject, data=hypertension)
anova(lme.mod1)

plot(lme.mod1, col="black")

## ------------------------------------------------------------------------
lme.mod2 <- lme(Y ~ Time*Diet, random=~1|Subject, data=hypertension)
anova(lme.mod2)

