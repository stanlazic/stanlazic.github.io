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
    cache.path = "cache_EDA/")

opts_knit$set(progress = TRUE,
              verbose = TRUE,
              unnamed.chunk.label = "CHUNK",
              width = 70)
options(show.signif.stars=FALSE)

## ----echo=FALSE----------------------------------------------------------
rm(list=ls())

## ------------------------------------------------------------------------
cor.test(~y1+x1, data=anscombe)

## ----anscombe, echo=FALSE, out.width = "0.85\\textwidth", fig.height = 6, fig.width = 6, fig.cap='Anscombe data. The data in all four panels have the same correlation, p-value, slope, and intercept, but a correlation or linear regression is only appropriate for the top left graph.'----
par(mfrow=c(2,2),
    mar=c(4,4,2,1),
    las=1)
plot(y1 ~ x1, data=anscombe, xlim=c(0,20), ylim=c(0,14))
mtext("A", side=3, line=0.5, adj=0, font=2, cex=1.25, )
abline(lm(y1 ~ x1, data=anscombe))

plot(y2 ~ x2, data=anscombe, xlim=c(0,20), ylim=c(0,14))
mtext("B", side=3, line=0.5, adj=0, font=2, cex=1.25, )
abline(lm(y2 ~ x2, data=anscombe))

plot(y3 ~ x3, data=anscombe, xlim=c(0,20), ylim=c(0,14))
mtext("C", side=3, line=0.5, adj=0, font=2, cex=1.25, )
abline(lm(y3 ~ x3, data=anscombe))

plot(y4 ~ x4, data=anscombe, xlim=c(0,20), ylim=c(0,14))
mtext("D", side=3, line=0.5, adj=0, font=2, cex=1.25, )
abline(lm(y4 ~ x4, data=anscombe))

## ------------------------------------------------------------------------
library(labstats)

# check dimensions
dim(hypertension)

head(hypertension)

## ------------------------------------------------------------------------
# convert to wide layout
wide.hyper <- reshape(hypertension,
                      timevar="Time",
                      idvar=c("Subject","Diet"),
                      direction="wide")

# check dimensions
dim(wide.hyper)

wide.hyper

## ------------------------------------------------------------------------
long.hyper <- reshape(wide.hyper,
                      varying=c("Y.1","Y.2","Y.3"),
                      v.names="BP",
                      timevar="Time",
                      direction="long")

# check dimensions
dim(long.hyper)

# reorder by subject, then time
long.hyper <- long.hyper[order(long.hyper$Subject, long.hyper$Time), ]

head(long.hyper)

## ------------------------------------------------------------------------
data(cats, package="MASS")
summary(cats)

## ------------------------------------------------------------------------
head(cellcount)

sapply(cellcount, class)

## ------------------------------------------------------------------------
xtabs(~Subject + Diet, hypertension)

## ------------------------------------------------------------------------
# make data
date.data <- data.frame(subject=rep(1:2, each=3),
             visit.number=rep(1:3, 2),
             obs.date=c("01-03-2015","08-03-2015","15-03-2015",
                        "01-03-2015","03-08-2015","15-03-2015"))
 
# convert the date from a string into a "Date" format
date.data$obs.date <- as.Date(date.data$obs.date, format="%d-%m-%Y")

# check order
date.data

## ------------------------------------------------------------------------
# check difference between observation dates
with(date.data, tapply(obs.date, subject, diff))

## ------------------------------------------------------------------------
person <- c("Anna","Brynn","Carey","Brynn","Brynn")

# number of entries
length(person)

# unique entries
unique(person)

# number of unique entries
length(unique(person))

## ------------------------------------------------------------------------
# is a value duplicated?
duplicated(person)

# how many duplications?
sum(duplicated(person))

# not duplicated?
!duplicated(person)

# filter duplications
person[!duplicated(person)]

## ------------------------------------------------------------------------
head(hypertension, 3)

# combine rows into one long string
combos <- apply(hypertension, 1, paste, collapse="_")

head(combos)

# check for duplicated row entries
sum(duplicated(combos))

## ------------------------------------------------------------------------
cond <- factor(c("Treat","treat","Treat ","Cntrl","Cntrl","Cntrl"))
summary(cond)

# convert to lowercase (factor also converted to character)
cond <- tolower(cond)

# "find and replace" white space with nothing
cond <- factor(gsub(" ", "", cond))
cond

## ------------------------------------------------------------------------
table(cats$Bwt)

## ------------------------------------------------------------------------
# the 1st, 3rd, and 6th entry have only one value after the decimal
prec <- c(2.1, 2.254, 2.3, 2.376, 2.450, 2.2)

# the output is padded with zeros
prec

## ------------------------------------------------------------------------
# converting to a character removes trailing zeros
as.character(prec)

# format character output to keep zeros
char.prec <- sprintf("%.3f", prec)
char.prec

## ------------------------------------------------------------------------
# find location of pattern matches
index <- grep("00$", char.prec)
index

## ------------------------------------------------------------------------
# make a factor for pattern type
group.prec <- ifelse(1:length(prec) %in% index, "X00", "XXX")
group.prec <- factor(group.prec)

prec.dat <- data.frame(prec, group.prec)
prec.dat

## ----prec, out.width = "0.55\\textwidth", fig.height = 4.5, fig.width = 4, fig.cap='A categorical variable is made based on the precision of the measurements for the outcome. Then, the values of the outcome are plotted by group so see if there are systematic differences.'----
library(beeswarm)
par(las=1)
beeswarm(prec ~ group.prec, data=prec.dat, pch=16,
         ylim=c(2, 2.5), xlab="Group", ylab="Outcome")

## ------------------------------------------------------------------------
caret::nearZeroVar(cats, saveMetrics = TRUE)

## ------------------------------------------------------------------------
# percent unique
length(unique(cats$Bwt))/nrow(cats) * 100

## ------------------------------------------------------------------------
# frequency ratio
sort(table(cats$Bwt), decreasing=TRUE)

14/13

## ------------------------------------------------------------------------
mean(c(NA , 10, 20), na.rm=TRUE)   # NA indicates missing value
mean(c(0  , 10, 20))               # 0 indicates missing value
mean(c(-1 , 10, 20))               # -1 indicates missing value
mean(c(999, 10, 20))               # 999 indicates missing value

## ----missing_LOD, echo=FALSE, out.width = "0.6\\textwidth", fig.height = 4.5, fig.width = 4.25, fig.cap='Missing values and the limit of detection (LOD). Values below the LOD were substituted with a value of LOD/2 = 0.5. The substitutions appear reasonable for the control group but not the disease group. Could there be another reason that values are missing?'----
set.seed(2)
gene.exp <- c(rnorm(20,4), rnorm(20,8))
disease <- gl(2,20, labels=c("Control","Disease"))

par(las=1)
beeswarm(gene.exp ~ disease, method="center", pch=16, cex=0.8,
         ylim=c(0,12), xlab="", ylab="Gene expression")
abline(h=1, lty=2)
points(c(0.5,0.5,0.5) ~ c(0.95,1.05,2), pch=4, cex=1.5)
text(x=2.4, y=1, "LOD", pos=3)

legend("topleft", pch=c(16, 4), legend=c("Observed","NA"))

## ----echo=FALSE, include=FALSE, eval=TRUE--------------------------------
# used to suppress message in the chunk below
library(VIM)

## ------------------------------------------------------------------------
library(VIM)
summary(sleep)

# number of missing values by row (species)
apply(sleep, 1, function(x) sum(is.na(x)))

## ------------------------------------------------------------------------
# cross-tabulation of missing values
xtabs(~is.na(NonD) + is.na(Dream), data=sleep)

## ----missing_dat, out.width = "0.98\\textwidth", fig.height = 4, fig.width = 7, fig.cap='Missing data. The proportion of missing values for each variable (left) and the pattern of missing values (right). Grey squares indicate missing values and white squares indicate observed values.'----
miss <- aggr(sleep, plot=FALSE)
plot(miss, col=c("white", "darkgrey"), numbers = TRUE)

## ------------------------------------------------------------------------
Y <- rnorm(10)
L <- rnorm(10)
W <- rnorm(10)
P <- 2*L + 2*W

combo.mod <- aov(Y ~ L + W + P)
summary(combo.mod)

## ------------------------------------------------------------------------
caret::findLinearCombos(cbind(Y,L,W,P))

## ----mean_infor, echo=FALSE, out.width = "0.6\\textwidth", fig.height = 5, fig.width = 5, fig.cap='Averaging can remove information. Both distributions have the same mean and variance but one is bimodal.'----
x.vals <- seq(0,15, length.out=300)

par(mar=c(2,2,2,2))

plot(dnorm(x.vals, 7,1) ~ x.vals, type="l", xlim=c(4,10),
          ylab="", xlab="", yaxt="n", xaxt="n", ylim=c(0,0.5))

lines(dnorm(x.vals, 6.1, 0.5)/2 + dnorm(x.vals, 7.9, 0.5)/2 ~ x.vals,
      type="l", lty=2, lwd=1.5)

## ------------------------------------------------------------------------
# generate 3 unrelated variables
set.seed(1)
x1 <- rnorm(100, 10)
x2 <- rnorm(100, 10)
x3 <- rnorm(100, 10)

# x1 and x2 are uncorrelated
cor.test(x1, x2)

## ----sup_cor, out.width = "0.98\\textwidth", fig.height = 4, fig.width = 7, fig.cap='Spurious correlation. Two uncorrelated variables can become correlated when \"corrected\" for a common third variable.'----
# when divided by x3, x1 and x2 are correlated
cor.test(x1/x3, x2/x3)

par(las=1,
    mfrow=c(1,2))
plot(x2 ~ x1, main="Raw values")
plot(I(x2/x3) ~ I(x1/x3), ylab="x2/x3", xlab="x1/x3",
     main="Common denominator")

## ----preprocBlocks, echo=FALSE, out.width = "0.98\\textwidth", fig.height = 3.5, fig.width = 9.5, fig.cap='Correcting for block effects. The variances of the treatment groups become unequal when using only the control samples to remove differences between days (p = 0.007, middle graph). When all samples are used, the variances are similar (p = 0.531, right graph).'----

library(lattice)

# make blocking factor and grouping factor
block <- factor(rep(LETTERS[1:3], each=9))
fac.A <- factor(rep(c("Control","Group 1","Group 2"), each=3, times=3))

# design matrix for fixed effects
X.mat <- model.matrix(~ fac.A)

# set parameters
parms <- c(mu = 100,
           tau.A = -4,
           tau.B = 5)
sigma.w <- 2 # within group variation
sigma.b <- 15 # variation between blocks

# value of y without random error
y.det <- X.mat %*% parms

# create design matrix for random effects
R.mat <- model.matrix(~ 0 + block + block:fac.A) # cell means model

# random effects for blocks (variation between blocks)
set.seed(12081976)
block.effect <- c(rnorm(3, 0, sigma.b), rnorm(6, 0, 3))

rand.eff <- R.mat %*% block.effect

# generate data
y.dat <- rnorm(27, mean=y.det + rand.eff, sigma.w)

# tweak
y.dat[19:27] <- y.dat[19:27] + 8

# combine into dataframe
d.dat <- data.frame(y=y.dat, block, fac.A)


# subtract control group means
sumstat <- with(d.dat, tapply(y, list(block,fac.A), mean))
d.dat$cor.fac <- rep(sumstat[,1], each=9)
d.dat$normed <- d.dat$y - d.dat$cor.fac


# subtract block means
sumstat2 <- with(d.dat, tapply(y, block, mean))
d.dat$cor.fac2 <- rep(sumstat2, each=9)
d.dat$normed2 <- d.dat$y - d.dat$cor.fac2


# make plots
p1 <- xyplot(y ~ fac.A, ylim=c(80,140), data=d.dat,
       groups=block, type=c("p","a"),
       xlab="Treatment group", ylab="Outcome",
             main="      Raw values")


p2 <- xyplot(normed ~ fac.A, data=d.dat, ylim=c(-15,15),
       groups=block, type=c("p","a"),
       xlab="Treatment group", ylab="Corrected outcome",
             main="      Subtract control mean")


p3 <- xyplot(normed2 ~ fac.A, data=d.dat, ylim=c(-15,15),
       groups=block, type=c("p","a"),
       xlab="Treatment group", ylab="Corrected outcome",
             main="      Subtract day mean")

trellis.par.set(superpose.symbol=list(col="black", fill="grey", pch=c(21,22,23)),
                superpose.line=list(col="black", lty=1:3, lwd=c(1.5,1.5,2)),
                axis.components=list(right=list(tck=0),
                                     top=list(tck=0)),
                layout.widths=list(right.padding=0.1, left.padding=0.1))

print(p1, split=c(1,1,3,1), more=TRUE)
print(p2, split=c(2,1,3,1), more=TRUE)
print(p3, split=c(3,1,3,1))


grid::grid.text("Day 1", x=0.28, y=0.77, gp=gpar(font=1, cex=0.8))
grid::grid.text("Day 2", x=0.28, y=0.57, gp=gpar(font=1, cex=0.8))
grid::grid.text("Day 3", x=0.28, y=0.27, gp=gpar(font=1, cex=0.8))

## ----nonnorm, out.width = "0.6\\textwidth", fig.height = 5, fig.width = 5, fig.cap='Non-normal data generated from a normal distribution.'----
# mean for each person
x <- 1:100

# within group standard deviation
sigma <- 0.25

# data generated from a normal distribution
y.sim <- rnorm(n=100, mean=x, sd=sigma)

# mean of simulated data
mean(y.sim)

par(las=1)
hist(y.sim, col="grey", border="white", breaks=10,
    main="", xlab="y.sim")

## ----nonnormresid, out.width = "0.6\\textwidth", fig.height = 5, fig.width = 5, fig.cap='Residuals are normally distributed even though the data are not.'----
# fit a model and save the residuals
resid <- resid(aov(y.sim ~ x))

# residuals are normally distributed
par(las=1)
hist(resid, col="grey", border="white", breaks=10,
     main="", xlab="Residuals of y.sim")

## ------------------------------------------------------------------------
set.seed(123)
n.dist <- rnorm(100, 100, 10)
u.dist <- runif(100)
t.dist <- 10 * rt(100, df=3) + 100
l.dist <- rlnorm(100, 4.5, 0.65)

## ----1Dhist, out.width = "0.98\\textwidth", fig.height = 6, fig.width = 8, fig.cap='Graphs to assess the shape of a single variable.'----
library(BHH2)
par(mfrow=c(2,3),
    mar=c(5,4,3,1),
    las=1)

plot(table(round(n.dist,0)), ylab="Frequency",
     main="Histogram no binning", xlim=c(70, 125))

hist(n.dist, col="grey", border="white", breaks=10,
     main="Histogram", xlab="", xlim=c(70, 125))

dotPlot(n.dist, pch=19, main="Dot plot", xlab="",
        xlim=c(70, 125), cex=1.1)

plot(density(n.dist), main="Density plot", xlab="", xlim=c(70, 125))

qqnorm(n.dist, main="Q-Q plot"); qqline(n.dist)

## ----qqplots, out.width = "0.7\\textwidth", fig.height = 8, fig.width = 5, fig.cap='Dot plots and Q-Q plots for three non-normal distributions.'----
par(mfrow=c(3,2),
    mar=c(5,4,3,1),
    las=1)
dotPlot(u.dist, pch=19, main=~Uniform, xlab="")
qqnorm(u.dist, main=~Uniform); qqline(u.dist)

dotPlot(t.dist, pch=19, main=~t[(3)], xlab="")
qqnorm(t.dist, main=~t[(3)]); qqline(t.dist)

dotPlot(l.dist, pch=19,main=~log~normal, xlab="")
qqnorm(l.dist, main=~log~normal); qqline(l.dist)

## ------------------------------------------------------------------------
ks.test(n.dist, "pnorm", mean=mean(n.dist), sd=sd(n.dist))

## ------------------------------------------------------------------------
data(chickwts)

# reorder groups by mean weight
chickwts$feed <- reorder(chickwts$feed, chickwts$weight, mean)

## ----2vars, out.width = "0.99\\textwidth", fig.height = 9, fig.width = 11, fig.cap='Graphs of one continuous versus one categorical variable from the \\texttt{sciplot}, \\texttt{base}, \\texttt{beeswarm}, and \\texttt{beanplot} packages.'----
library(sciplot); library(beanplot)
par(mfrow=c(2,2),
    mar=c(4,3.8,3,0.5),
    las=1)

bargraph.CI(feed, weight, data=chickwts, main=~Mean~and~SEM~(sciplot),
            ylab="weight")

plot(weight ~ feed, data=chickwts, col="grey", main=~Boxplot~(graphics),
     xlab="")

beeswarm(weight ~ feed, data=chickwts, method="center",
         pch=16, main=~Dot~plot~(beeswarm), xlab="")
bxplot(weight ~ feed, data=chickwts, probs=0.5, add=TRUE)

beanplot(weight ~ feed, data=chickwts, main=~Bean~plot~(beanplot),
         what=c(1,1,1,1), col=c("darkgrey","white","black","black"),
         ylab="Weight", border="black")

## ----designplot, out.width = "0.98\\textwidth", fig.height = 5, fig.width = 11, fig.cap='A design plot is useful for ANOVA models.'----
par(mfrow=c(1,2),
    mar=c(1,4,3,0.5),
    las=1)

plot.design(activity/10000 ~ litter + group + drug + sex,
            data=VPA, main="VPA data")

plot.design(value ~ strain + treatment + factor(batch), data=festing,
            main="festing data")

## ------------------------------------------------------------------------
data(Soils, package="car")

# all pairwise correlations 
all.cors <- cor(Soils[,c(6,13,14)])

# reorder rows to make the same as the graph
all.cors[3:1,]

## ----splom, out.width = "0.9\\textwidth", fig.height = 8, fig.width = 8, fig.cap='Scatterplot matrix.'----
library(lattice)
splom(~Soils[,c(6,13,14)], cex=0.8, col="black",
      type=c("p","smooth"))

## ----eval=FALSE----------------------------------------------------------
## trellis.focus()
## panel.brush.splom()

## ----splom2, out.width = "0.9\\textwidth", fig.height = 8, fig.width = 8, fig.cap='Scatterplot matrix with information on sample depth encoded by symbol type (1 = shallow; 4 = deep).'----
trellis.par.set(superpose.symbol=list(col="black", pch=c("1","2","3","4")))

splom(~Soils[,c(6,13,14)], data=Soils, groups=Depth,
      auto.key=list(columns=4, title="Depth"), cex=1.2)

## ----threeD, out.width = "0.68\\textwidth", fig.height = 6, fig.width = 6, fig.cap='3D scatterplot with information on sample depth encoded by symbol type (1 = shallow; 4 = deep).'----
trellis.par.set(superpose.symbol=list(col="black", pch=c("1","2","3","4")))

cloud(Conduc ~ Na + pH, data = Soils, col="black", group=Depth, cex=1.2,
      R.mat = matrix(c(-0.448, 0.308, -0.841, 
          0, -0.894, -0.173, 0.413, 0, -0.017, 0.939, 
          0.353, 0, 0, 0, 0, 1), nc = 4), screen = list())

## ----eval=FALSE----------------------------------------------------------
## library(playwith)
## playwith(
##     cloud(...)
##     )

## ----xyplot1, out.width = "0.98\\textwidth", fig.height = 4, fig.width = 8, fig.cap='Scatterplot with three predictor variables. Sodium and conductance are conditioned on contour.'----
trellis.par.set(superpose.symbol=list(col="black", pch=1:4))

xyplot(Conduc~Na|Contour, data=Soils, group=Depth, type=c("g","p"),
       auto.key=list(columns=4, title="Depth"),
       scales=list(alternating=FALSE), layout=c(3,1))

## ----echo=FALSE, include=FALSE, eval=TRUE--------------------------------
# used to suppress message in the chunk below
library(latticeExtra)

## ----xyplot2, out.width = "0.98\\textwidth", fig.height = 7, fig.width = 7, fig.cap='Scatterplot with four predictor variables.'----
library(latticeExtra)

trellis.par.set(superpose.symbol=list(col="black", pch=1:4))

useOuterStrips(
xyplot(Conduc~Na|reorder(Contour,Conduc,mean)+Depth, data=Soils,
       groups=Block, type=c("g","p"), scales=list(alternating=FALSE),
       auto.key=list(columns=4, title="Block"))
    )

## ----xyplot3, out.width = "0.98\\textwidth", fig.height = 5, fig.width = 7, fig.cap='Another scatterplot with three predictor variables but varying the grouping and conditioning variables. This graph allows block effects to be easily seen.'----
trellis.par.set(superpose.symbol=list(col="black", pch=1:4),
                superpose.line=list(col="black"))

xyplot(Conduc~Block|reorder(Contour,Conduc,mean), data=Soils,
       type=c("g","b"), scales=list(alternating=FALSE), layout=c(3,1),
       groups=Depth, auto.key=list(columns=4, title="Depth"))

## ------------------------------------------------------------------------
summary(cellcount)

## ----cell_count_hist, out.width = "0.85\\textwidth", fig.height = 4, fig.width = 6, fig.cap='Cell count across all plates.'----
cell.num <- table(cellcount$cell.count) # seven wells with zero cells
head(cell.num)

par(las=1)
plot(cell.num, xlab="Cell count", ylab="Frequency")

## ----plate_dists, out.width = "0.98\\textwidth", fig.height = 4, fig.width = 8, fig.cap='Plate effects. Distributions are similar across all plates indicating the absence of large plate effects.'----
par.set <- list(box.umbrella=list(col="black", lty=1), 
                box.dot=list(col="black"), 
                box.rectangle = list(col="black"),
                plot.symbol=list(col="black"))

p1 <- bwplot(cell.count ~ plate, data=cellcount,
             ylab="Cell count", scales=list(x=list(rot=45)),
             col="black", pch="|", fill="lightgrey",
             main="Boxplot", par.settings=par.set)

p2 <- densityplot(~cell.count, data=cellcount, groups=plate,
            plot.points=FALSE, col="black", type=c("g","l"),
                  xlab="Cell count", main="Density plot")

print(p1, split=c(1,1,2,1), more=TRUE)
print(p2, split=c(2,1,2,1))

## ------------------------------------------------------------------------
# calculate row and column means
row.means <- with(cellcount, tapply(cell.count, list(plate,row), mean))
col.means <- with(cellcount, tapply(cell.count, list(plate,column), mean))

dim(row.means)

row.means[,1:5]

dim(col.means)

## ----plates_profiles, out.width = "0.98\\textwidth", fig.height = 4, fig.width = 8, fig.cap='Spatial effects. Each line is one plate and row numbers 1--10 have lower cell counts. There also appears to be some zig-zagging across rows. The horizontal line is the overall mean cell count (across all plates).'----
par(mfrow=c(1,2), las=1)
matplot(t(row.means), type="l", col="black", ylim=c(200, 450),
        xlab="Row", ylab="Mean cell count", main="Row means")
abline(h=mean(cellcount$cell.count))

matplot(t(col.means), type="l", col="black", ylim=c(200, 450),
        xlab="Column", ylab="Mean cell count", main="Column means")
abline(h=mean(cellcount$cell.count))

## ----plate_layout, out.width = "0.98\\textwidth", fig.height = 8, fig.width = 6, fig.cap='Spatial effects. A level plot shows that the top left part of some plates tend to have lower cell counts (darker shades).'----
# nice colour scheme (not used for book)
# levelcols <- colorRampPalette(c("darkblue", "blue", "lightgreen",
#                                 "white", "orange", "red", "darkred"))

greycols <- colorRampPalette(c("black","white"))

levelplot(cell.count ~ column*row|plate, data=cellcount, aspect=0.67,
          ylim=c(32,1), between=list(x=1,y=1), layout=c(2,3),
          as.table=TRUE, scales=list(alternating=FALSE),
          col.regions=greycols(100), colorkey=FALSE)

## ------------------------------------------------------------------------
# select only plate five
plate5 <- subset(cellcount, plate=="Plate_5")

# subtract the median
plate5$cent.count <- plate5$cell.count - median(plate5$cell.count)

# +1 if well is above plates median, -1 otherwise
plate5$above.below <- ifelse(plate5$cent.count > 0, 1, -1)

head(plate5)

## ----plates_layout_check, out.width = "0.6\\textwidth", fig.height = 4*0.9, fig.width = 6*0.9, fig.cap='Spatial effects for wells above (white) or below (black) the median. The first few rows tend to be black, indicating a spatial effect.'----
levelplot(above.below ~ column*row, data=plate5,
          main="", aspect=0.67, ylim=c(32,1), colorkey=FALSE,
          #panel = panel.2dsmoother, args = list(span=0.1), n=200
          col.regions=greycols(100))

## ----eval=FALSE----------------------------------------------------------
## wireframe(cell.count ~ column*row, data=plate5, colorkey=FALSE,
##           scales=list(arrows=TRUE,distance=1.4), zlim=c(0,600),
##           drape=TRUE, col.regions=greycols(100), zoom=0.65,
##           zlab="cell\ncount", main="Raw values",  aspect=c(0.67,0.6))

## ------------------------------------------------------------------------
sm1 <- loess(cell.count ~ column*row, data=plate5,
             span=0.25, normalize = FALSE,
             family="symmetric", na.action="na.exclude")

## ------------------------------------------------------------------------
# extract and save the smoothed values
plate5$smoothed <- fitted(sm1)

## ------------------------------------------------------------------------
# extract residuals
plate5$resid <- resid(sm1)

# add plate mean to shift the residuals to be positive
plate5$corrected.count <- plate5$resid + mean(plate5$cell.count)

summary(plate5[,c(4,5,8,9)])

## ----plates_wireframe, echo=FALSE, out.width = "0.98\\textwidth", fig.height = 4, fig.width = 10, fig.cap='Surface plots for spatial effects. The graphs show the raw cell count values for Plate 5, the smoothed values, and the corrected values.'----
p1 <- wireframe(cell.count ~ column*row, data=plate5, colorkey=FALSE,
          scales=list(arrows=TRUE,distance=1.4), zlim=c(0,600),
          drape=TRUE, col.regions=greycols(100), zoom=0.65,
          zlab="cell\ncount", main="Raw values",  aspect=c(0.67,0.6))

p2 <- wireframe(smoothed ~ column*row, data=plate5, colorkey=FALSE,
          scales=list(arrows=TRUE,distance=1.4), zlim=c(0,600),
          drape=TRUE, col.regions=greycols(100), zoom=0.65,
          zlab="cell\ncount", main="Smoothed",  aspect=c(0.67,0.6))


p3 <- wireframe(corrected.count ~ column*row, data=plate5, colorkey=FALSE,
          scales=list(arrows=TRUE,distance=1.4), zlim=c(0,600),
          drape=TRUE, col.regions=greycols(100), zoom=0.65,
          zlab="cell\ncount", main="Corrected",  aspect=c(0.67,0.6))

print(p1, split=c(1,1,3,1), more=TRUE)
print(p2, split=c(2,1,3,1), more=TRUE)
print(p3, split=c(3,1,3,1))

## ----correct_cell_counts, echo=FALSE, out.width = "0.58\\textwidth", fig.height = 4.5, fig.width = 4.5, fig.cap='Raw versus corrected cell counts. The black triangle indicates a point with a raw cell count of zero, but a corrected count of 110.'----
par(las=1)
plot(corrected.count ~ cell.count, data=plate5,
     xlim=c(-60,620), ylim=c(-60,620), cex=0.8, col="grey40",
     xlab="Raw cell counts", ylab="Corrected cell counts")
abline(0, 1)
points(plate5$corrected.count[195] ~ plate5$cell.count[195],
       pch=17, cex=1.5)

## ------------------------------------------------------------------------
summary(locomotor)

## ----LMA_mean_sem, echo=FALSE, out.width = "0.8\\textwidth", fig.height = 5, fig.width = 6, fig.cap='Means and SEM. The data represent the total distance travelled by animals every 15 minutes. The two groups are similar at 15 min, and then the drug increases the distance travelled between 15 and 30 min, where it reaches its maximum effect and then remains stable until the last observation.'----
par(las=1)
lineplot.CI(time, dist, drug, data=locomotor, xlab="Time (min)",
             ylab="Locomotor activity", ylim=c(0,3.2),
             legend=FALSE, xlim=c(0.5,7.7),
             err.width=0.05)
text("Drug", x=7, y=2, font=2)
text("Control", x=7.1, y=0.6, font=2)

## ----LMA_repeated_raw, out.width = "0.98\\textwidth", fig.height = 4, fig.width = 8, fig.cap='Profile plot of each animal. No rat in the drug group follows the pattern of change in Figure \\ref{fig:LMA_mean_sem}.'----
xyplot(dist ~ factor(time)|drug, data=locomotor, group=animal, 
       type=c("g","l"), between=list(x=1), col="black",
       xlab="Time (min)", ylab="Locomotor activity",
       strip=strip.custom(bg="lightgrey"),
       scales=list(alternating=FALSE) )

## ------------------------------------------------------------------------
# select only drug group
drug.only <- locomotor[locomotor$drug=="Drug",]

# convert data into wide format
drug.wide <- unstack(drug.only, dist~time)

# examine layout
head(drug.wide)

## ------------------------------------------------------------------------
# perform clustering
set.seed(1)
km <- kmeans(drug.wide, centers=3, nstart=100)

# show results
km

## ----LMA_repeated_clust, out.width = "0.98\\textwidth", fig.height = 4, fig.width = 8, fig.cap='Profile plot for each animal in the drug group. Some animals do not respond at all or have a small increase over time (cluster 3), some have a large early response but then drop back down to control levels (cluster 2), and some have a gradual increase that levels off (cluster 1).'----
# make the assigned cluster into a factor
drug.only$clust <- as.factor(paste("Drug Cluster",
                             rep(km$cluster, 6), sep=" "))

# make plot and conditon on cluster
xyplot(dist ~ factor(time)|clust, group=animal, data=drug.only,
       type=c("g","l"), between=list(x=1), col="black",
       xlab="Time (min)", ylab="Locomotor activity",
       layout=c(3,1), scales=list(alternating=FALSE))

