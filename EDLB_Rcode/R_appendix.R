## ----echo=FALSE----------------------------------------------------------
rm(list=ls())

## ----eval=FALSE----------------------------------------------------------
## install.packages("package_name")

## ----eval=FALSE----------------------------------------------------------
## library(package_name)

## ----eval=FALSE----------------------------------------------------------
## update.packages()

## ----eval=FALSE----------------------------------------------------------
## install.packages(c("labstats", "sciplot", "plyr", "beeswarm",
##                    "AlgDesign", "VIM", "beanplot", "playwith",
##                    "latticeExtra"))

## ------------------------------------------------------------------------
2 + 2

## ------------------------------------------------------------------------
y <- 2 + 2

## ------------------------------------------------------------------------
y

## ------------------------------------------------------------------------
y + 3  # add 3 to y

x <- 1 # make a new variable x

x + y  # add the variables together

## ------------------------------------------------------------------------
sqrt(y)

## ------------------------------------------------------------------------
ls()

## ----eval=FALSE----------------------------------------------------------
## # do not run this code as x and y will be used later
## rm(x, y)

## ----eval=FALSE----------------------------------------------------------
## save.image(file="all_objects.RData")

## ----eval=FALSE----------------------------------------------------------
## load("all_objects.RData")

## ----eval=FALSE----------------------------------------------------------
## help("plot")
## ?plot        # is the same

## ----eval=FALSE----------------------------------------------------------
## help.search("plot")
## 
## # graphics::barplot              Bar Plots

## ------------------------------------------------------------------------
head(apropos("plot"), 10)

## ----echo=FALSE----------------------------------------------------------
# change my default to the "factory default"
options(show.signif.stars = TRUE)

## ------------------------------------------------------------------------
options()$show.signif.stars

## ------------------------------------------------------------------------
options(show.signif.stars = FALSE) # set to FALSE
options()$show.signif.stars  # check that it worked

## ----eval=FALSE----------------------------------------------------------
## data() # lists data sets in loaded packages
## 
## data(package = .packages(all.available = TRUE)) # lists all data sets
## 
## data(package="car") # lists all data sets for a given package

## ------------------------------------------------------------------------
library(car)         # load package

data("ChickWeight")  # load data

summary(ChickWeight) # summarise the data

## ----eval=FALSE----------------------------------------------------------
## d1 <- read.delim("filename")  # read a tab-delimited file
## d2 <- read.csv("filename")    # read a comma separated values file

## ----eval=FALSE----------------------------------------------------------
## write.table(object, filename="saved_data.txt", sep="\t", quote=FALSE,
##             row.names=FALSE)
## write.csv(object, filename="saved_data.csv")

## ------------------------------------------------------------------------
class(y)

library(labstats)
class(festing)

class(festing$value)     # class = integer

class(festing$treatment) # class = factor

## ------------------------------------------------------------------------
summary(festing$value) 

summary(festing$treatment)

## ------------------------------------------------------------------------
sapply(festing, class)

## ------------------------------------------------------------------------
festing$fac.batch <- as.factor(festing$batch) # convert to factor

class(festing$fac.batch) # check class

## ------------------------------------------------------------------------
is.factor(festing$fac.batch) # is it a factor?

is.integer(festing$fac.batch) # is it an integer?

## ------------------------------------------------------------------------
str(festing) # look at structure

## ------------------------------------------------------------------------
dim(festing) # Dimensions (number of rows and columns)

names(festing)  # Column names (use colnames() for a matrix)

rownames(festing) # Row names

head(festing) # Show the first few lines (rows)

tail(festing) # Show the last few lines (rows)

## ------------------------------------------------------------------------
z <- c(3, 5, NA, 8)
z

mean(z)

mean(z, na.rm=TRUE)

## ------------------------------------------------------------------------
0/0

## ------------------------------------------------------------------------
# division by zero
10/0

Inf + 1 # still infinity

## ------------------------------------------------------------------------
# Is 5 greater than 3?
5 > 3

# Are the words "hat" and "cat" equal (the same)?
"hat" == "cat"

# Are the words "hat" and "cat" unequal (different)?
"hat" != "cat"

# Is "hat" less than "cat" (is "hat" alphabetically before "cat")?
"hat" <  "cat"

# Does the word "cat" equal 5?
"cat" == 5

# Does the variable x equal the variable y?
# Recall that we previously defined x=1 and y=4
x == y

## ------------------------------------------------------------------------
if (5 > 3) print("Bigger!")

if (3 > 5) print("Bigger!")

## ------------------------------------------------------------------------
if (x > y) {
    
    print("Bigger!")
    
} else if (x < y) {
    
    print("Smaller!")
    
} else {
    
    print("The same!")
    
    }

## ----eval=FALSE----------------------------------------------------------
## # if (3 > 5) { print("Bigger!") }
## # else       { print("Smaller!") }

## ------------------------------------------------------------------------
# ifelse(condition, if TRUE, if FALSE)
w <- ifelse(x == y, "Same", "Different") # Recall that x = 1, y =4
w

## ----eval=FALSE----------------------------------------------------------
## function.name <- function(arguments){ code }

## ----error=TRUE----------------------------------------------------------
cuberoot <- function(x){
    ## calculates the cube root of a number
    
    # check input
    if (!is.numeric(x)) { stop("x must be a number!") }
    
    # do calculation
    output <- x^(1/3)
    
    return(output)
}

cuberoot(8)

cuberoot("eight")

cuberoot(-27)

cuberoot(0)

## ----warning = TRUE------------------------------------------------------
cuberoot2 <- function(x){
    ## calculates the cube root of a number
    
    # check input
    if (!is.numeric(x)) { stop("x must be a number!") }
    
    # warn if input is less than zero
    if (x < 0) warning("x is less than zero.")
    
    # do calculation
    output <- sign(x) * abs(x)^(1/3)
    
    return(output)
}

cuberoot2(-27)

## ------------------------------------------------------------------------
z

z[1]            # first element

z[c(1, 4)]      # first and last element

## ------------------------------------------------------------------------
3:9             # all integers from 3 to 9

## ------------------------------------------------------------------------
z[1:3]          # first three items (range)

## ------------------------------------------------------------------------
z[length(z)]    # last item

## ------------------------------------------------------------------------
z[-3]           # all but the third item

## ------------------------------------------------------------------------
# is the item missing?
is.na(z)

# is the item NOT missing? = is the item present?
!is.na(z)

z[!is.na(z)]    # exclude NAs

## ------------------------------------------------------------------------
z[z > 4]

## ------------------------------------------------------------------------
festing[1, 2]  # item in first row, second column

festing[3, ]   # all of third row

festing[, 4]   # all of fourth column

festing[1:4, c(1,3)] # rows 1-4, column 1 and 3

## ------------------------------------------------------------------------
festing$strain

festing[, "strain"]

## ------------------------------------------------------------------------
# select only NIH mice
festing[festing$strain=="NIH", ]

# alternate method
subset(festing, subset=strain=="NIH")

# also exclude the column "batch"
subset(festing, subset=strain=="NIH", select=-batch)

## ------------------------------------------------------------------------
z

# preallocate vector
z.new <- rep(NA, length(z))

z.new

# This is inefficient computationally
# z.new <- NA

## ------------------------------------------------------------------------
for (i in 1:length(z) ){
    # multiply by two and save in z.new
    z.new[i] <- z[i] * 2
    
    # print iteration number
    print(c("i"=i, "z"=z[i], "z.new"=z.new[i]))
    }

z.new

## ------------------------------------------------------------------------
z * 2

## ------------------------------------------------------------------------
# combined into matrix
z.all <- cbind(z, z.new)

z.all

# add one to all elements
z.all + 1

## ------------------------------------------------------------------------
# sum the values across rows
apply(z.all, 1, sum)

# calculate the max value in each column
apply(z.all, 2, max, na.rm=TRUE)

## ------------------------------------------------------------------------
# calculate mean for each treatment group
with(festing, tapply(value, treatment, mean))

# calculate SD for each treatment group by strain
with(festing, tapply(value, list(strain, treatment), sd))

## ----eval=FALSE----------------------------------------------------------
## y.var ~ x.var

## ------------------------------------------------------------------------
plot(value ~ treatment, data=festing)

## ------------------------------------------------------------------------
par(mfrow=c(1,2), # plot two graphs in a 1 by 2 grid
    mar=c(4,4,2.5,1), # decrease the margin size
    las=1)  # default is las=0

plot(value ~ treatment, data=festing, ylim=c(0, 1100),
     ylab="Levels of Gst", xlab="", col="darkgrey",
     main="Effect of DS on Gst", boxwex=0.5)
abline(h=100, lty=2)
mtext("A", side=3, line=1, font=2, cex=1.5, adj=0)

# alternate way of passing data and specifying variables
with(festing, {
         plot(x=treatment, y=value, ylim=c(0, 1100),
              ylab="Levels of Gst", xlab="", col="darkgrey",
              main="Effect of DS on Gst", boxwex=0.5)
})
mtext("B", side=3, line=1, font=2, cex=1.5, adj=0)

## ----eval=FALSE----------------------------------------------------------
## plot(...)
## dev.copy2pdf("nice_plot.pdf")
## dev.off()

## ----eval=FALSE----------------------------------------------------------
## pdf("nice_plot.pdf", height=5, width=5)
## plot(...)
## dev.off()

## ----eval=FALSE----------------------------------------------------------
## plot()
## boxplot()
## barplot()
## pairs()
## interaction.plot()
## plot.design()
## dotchart()
## contour()
## coplot()

## ------------------------------------------------------------------------
library(lattice)

xyplot(value ~ treatment|batch, data=festing,
       groups=strain)

## ------------------------------------------------------------------------
trellis.par.set(superpose.symbol=list(col="black", pch=1:4),
                superpose.line=list(col="black", lty=1:4),
                axis.components=list(right=list(tck=0),
                                     top=list(tck=0)))

xyplot(value ~ treatment|factor(batch), data=festing,
       groups=strain, type=c("g","p","l"), ylim=c(0, 1100),
       xlab="", ylab="Levels of Gst", main="Effect of DS on Gst",
       scales=list(alternating=FALSE), between=list(x=1),
       auto.key=list(columns=1, lines=TRUE,
           title="Strain", space="right")) 

## ----eval=FALSE----------------------------------------------------------
## par.set <- list(superpose.symbol=list(col="black", pch=1:4),
##                 superpose.line=list(col="black", lty=1:4),
##                 axis.components=list(right=list(tck=0),
##                                      top=list(tck=0)))
## 
## xyplot(..., par.settings=par.set)

## ----eval=FALSE----------------------------------------------------------
## pdf("nice_plot.pdf")
## print(
##     xyplot(...)
## )
## dev.off()

## ----eval=FALSE----------------------------------------------------------
## xyplot()
## dotplot()
## barchart()
## levelplot()
## splom()
## contourplot()
## densityplot()
## qqmath()
## wireframe()

## ------------------------------------------------------------------------
set.seed(1)
rnorm(n=5, mean=10, sd=2)

## ----echo=FALSE----------------------------------------------------------
set.seed(1)
norm.data <- rnorm(n=100, mean=10, sd=2)
norm.dist <- pnorm(12, mean=10, sd=2)
norm.dens <- dnorm(seq(3,17,0.01), mean=10, sd=2)
norm.quant <- qnorm(0.05, mean=10, sd=2)


par(mfrow=c(2,2),
    mar=c(4,1,4,1))
BHH2::dotPlot(norm.data, pch=16, main=~rnorm(),
              xlab="X",xlim=c(3,17))
box()
legend("topleft", legend="Given mean and SD,\ngenerate data",
       bty="n")
mtext("A", side=3, font=2, line=2, cex=1.5, adj=0)

plot(norm.dens ~ seq(3,17,0.01), type="l",
     main=~pnorm(), xlab="X", ylab="", yaxt="n")
polygon(c(3,seq(3,12,0.01),12),
        c(0,dnorm(seq(3,12,0.01), mean=10, sd=2),0),
        col="grey")
legend("topleft", legend="Given X,\nwhat is AUC",
       bty="n")
mtext("B", side=3, font=2, line=2, cex=1.5, adj=0)

plot(norm.dens ~ seq(3,17,0.01), type="l",
     main=~dnorm(), xlab="X", ylab="",
     yaxt="n")
legend("topleft", legend="Given X,\nwhat is the\nheight",
       bty="n")
points(y=norm.dens[seq(3,17,0.01) == 12], x=12, pch=16, cex=1.25)
mtext("C", side=3, font=2, line=2, cex=1.5, adj=0)

plot(norm.dens ~ seq(3,17,0.01), type="l",
     main=~qnorm(), xlab="X", ylab="", yaxt="n")
polygon(c(3,seq(3,6.71,0.01),6.71),
        c(0,dnorm(seq(3,6.71,0.01), mean=10, sd=2),0),
        col="grey")
legend("topleft", legend="Given AUC,\nwhat is X",
       bty="n")
mtext("D", side=3, font=2, line=2, cex=1.5, adj=0)

## ------------------------------------------------------------------------
pnorm(12, mean=10, sd=2) # from -Inf to 12

## ------------------------------------------------------------------------
1 - pnorm(12, mean=10, sd=2) # total area equals 1

pnorm(12, mean=10, sd=2, lower.tail=FALSE) # from 12 to Inf

## ------------------------------------------------------------------------
pt(2.1, df=10, lower.tail=FALSE) * 2

## ------------------------------------------------------------------------
pt(2.149999, df=10, lower.tail=FALSE) * 2

## ------------------------------------------------------------------------
pt(2.1, df=10, lower.tail=FALSE)

## ------------------------------------------------------------------------
# first few lines of the data
head(festing)

# fit model with one predictor variable
mod1 <- aov(value ~ treatment, data=festing)

summary(mod1)

## ------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(mod1)

## ------------------------------------------------------------------------
# main effects only
mod2 <- aov(value ~ strain + treatment, data=festing)
summary(mod2)

## ------------------------------------------------------------------------
# main and interaction effects
mod3 <- aov(value ~ strain * treatment, data=festing)

# alternative specification
mod3b <- aov(value ~ strain + treatment + strain:treatment,
            data=festing)

summary(mod3)

## ------------------------------------------------------------------------
mod4 <- aov(value ~ batch + strain * treatment, data=festing)
summary(mod4)

# hierarchical sum of squares
car::Anova(mod4)

## ------------------------------------------------------------------------
# calculated predicted values for mod3 and mod4
pred.mod3 <- predict(mod3)     # same as fitted(mod3)
pred.mod4 <- predict(mod4)     # same as fitted(mod4)

par(mfrow=c(1,2),
    las=1)

plot(pred.mod3 ~ festing$value, ylab="Predicted", xlab="Actual",
     xlim=c(300, 1200), ylim=c(300, 1200), main="Model 3")
abline(0, 1, lty=2)

plot(pred.mod4 ~ festing$value, ylab="Predicted", xlab="Actual",
     xlim=c(300, 1200), ylim=c(300, 1200), main="Model 4")
abline(0, 1, lty=2)

