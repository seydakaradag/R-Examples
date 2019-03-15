## ############################################################################
## R Introduction and Best Practice
## Descriptive statistics
## May 2016
## ############################################################################

#rm(list=ls())

## ============================================================================
## global variables
## ============================================================================

path.raw <- "C:/Projects/Vodafone_TR/"
#path.raw <- "C:/data-sync/teradata-work/teradata-project/training-knowledge-exchange/r-training-general/"
path.r <- paste0(path.raw, "r-scripts/")
path.dat <- paste0(path.raw, "data/")
path.tmp <- paste0(path.raw, "tmp/")

## ============================================================================
## read data from file
## ============================================================================

load(file=paste0(path.dat, "twm_customer_analysis_train.Rdata")) ## contains dat.train
#load(file=paste0(path.dat, "twm_customer_analysis_test.Rdata"))  ## contains dat.test


## ############################################################################
## ############################################################################
##
## Descriptive Statistics
##
## ############################################################################
## ############################################################################

## ============================================================================
## mean
## ============================================================================

## load Computers dataset from Ecdat package
library(Ecdat)
data(Computers)
str(Computers)

## calculate mean:
mean(Computers$price)

## mean with missings (introduce a missing value):
myprice <- Computers$price
myprice[1] <- NA

mean(myprice)
mean(myprice, na.rm=TRUE)

## ============================================================================
## median
## ============================================================================

## compute the median:
median(Computers$price)

## median with missings:
median(myprice)
median(myprice, na.rm=TRUE)

## ============================================================================
## frequency and mode
## ============================================================================

## frequency table (absolute counts):
table(dat.train$marital_status)

## relative frequencies:
prop.table(table(dat.train$marital_status))

## frequency table with missings:
mymarstat <- dat.train$marital_status
mymarstat[1] <- NA

table(mymarstat)
table(mymarstat, useNA="ifany")
table(mymarstat, useNA="always")


## ============================================================================
## variance and standard deviation
## ============================================================================

## calculate variance:
var(Computers$price)

## calculate variance with missings:
var(myprice)
var(myprice, na.rm=TRUE)

## standard deviation:
sd(Computers$price)

## standard deviations with missings:
sd(myprice)
sd(myprice, na.rm=TRUE)


## ============================================================================
## range
## ============================================================================

## minimum and maximum:
min(Computers$price)
max(Computers$price)

## range as vector of min and max:
range(Computers$price)

## range as difference between min and max:
diff(range(Computers$price))

## range with missings:
range(myprice)
range(myprice, na.rm=TRUE)

## ============================================================================
## inter-quartile range
## ============================================================================

## inter-quartile range
IQR(Computers$price)

## inter-quartile range with missings:
IQR(myprice)    ## throws an error with missings
IQR(myprice, na.rm=TRUE)

## quartiles and quantiles:
quantile(Computers$price)
quantile(Computers$price, probs=c(0, .33, .67, 1))


## ============================================================================
## first visualizations
## ============================================================================

## more on visualization and plotting in R later...

## ------------------------------------------------------------------------
## histogram
## ------------------------------------------------------------------------

## histogram:
hist(dat.train$income, nclass=20, col="grey", xlab="Income")
hist(dat.train$age, nclass=20, col="grey", xlab="Age")

hist(Computers$price, nclass=20, col="grey", xlab="Price")

## ------------------------------------------------------------------------
## boxplot
## ------------------------------------------------------------------------

## boxplot:
boxplot(Computers$price, col="grey")

## ------------------------------------------------------------------------
## barplot
## ------------------------------------------------------------------------

## barplot: needs a frequency table as data source
barplot(table(dat.train$marital_status))

barplot(table(Computers$cd))


## ============================================================================
## standardizing
## ============================================================================

## check mean and standard deviation:
mean(Computers$price)
sd(Computers$price)

## check first value:
Computers$price[1]

## scale variable and check mean and standard deviation:
zprice <- scale(Computers$price)
mean(zprice)
sd(zprice)

zprice[1]

## note: scale returns a matrix, with attributes:
str(zprice)

## get all or specific attributes:
attributes(zprice)
attr(zprice, "scaled:center")


## ============================================================================
## data transformations: log, power, Box-Cox
## ============================================================================

## skewed income:
hist(dat.train$income, nclass=20, col="grey")

## log transformation:
hist(log(dat.train$income+1), nclass=20, col="grey")

## root transformation:
hist(sqrt(dat.train$income), nclass=20, col="grey")
hist(dat.train$income^(1/2), nclass=20, col="grey")
hist(dat.train$income^(1/4), nclass=20, col="grey")

## box-cox transformation:
library(car)
boxCox(income+1 ~ 1, lambda=seq(-3,3,.1), data=dat.train)
boxCox(income+1 ~ 1, lambda=seq(0,1,.1), data=dat.train)
tmp <- boxCox(income+1 ~ 1, lambda=seq(0,1,.1), data=dat.train)
tmp$x[which.max(tmp$y)]

hist(bcPower(dat.train$income+1, .34), nclass=20, col="grey")
hist(bcPower(dat.train$income+1, .1), nclass=20, col="grey")


## ============================================================================
## covariance and correlation
## ============================================================================

#myprice <- Computers$price
#myprice[1] <- NA

## covariances between two variables:
cov(Computers$price, Computers$hd)

## covariance with missings:
cov(myprice, Computers$hd)
cov(myprice, Computers$hd, use="complete.obs")

## correlations between two variables:
cor(Computers$price, Computers$hd)

## correlation with missings:
cor(myprice, Computers$hd)
cor(myprice, Computers$hd, use="complete.obs")

## correlations between multiple variables in data frames:
comp.tmp <- Computers          ## make copy of Computers data frame
comp.tmp$myprice <- myprice    ## add myprice to that copy
str(comp.tmp)

cor(comp.tmp[c("myprice", "hd", "speed")])
cor(comp.tmp[c("myprice", "hd", "speed")], use="complete.obs")
cor(comp.tmp[c("myprice", "hd", "speed")], use="pairwise.complete.obs")
## correlations between hd and speed is slightly different, one more case is used

## get numeric variables and do a full correlation matrix:
var.num <- sapply(dat.train, is.numeric)
var.num
cor(dat.train[var.num])


## ------------------------------------------------------------------------
## heatmaps and correlation plot:
## ------------------------------------------------------------------------

## heatmap:
cormat <- cor(dat.train[var.num])
heatmap(cormat, symm=TRUE)

cols <- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(cormat, col=cols, symm=TRUE)

## corrplot with library corrplot:
library(corrplot)
corrplot(cormat, type="upper", order="hclust", tl.col="black", tl.srt=45)

## -----------------------------------
## heterogenous correlation matrix:
## -----------------------------------

library(polycor)

hetcor(Computers, ML=FALSE, std.err=FALSE, use="complete.obs")
## computes heterogenous correlation matrix (pearson, polyserial, polychoric)
## very slow; doesn't always work (depends on data)


## ============================================================================
## scatterplot
## ============================================================================

## plot of two numeric variables:
plot(Computers$hd, Computers$price)

## ploting a forumla (more later):
plot(Computers$price ~ Computers$hd)

## plotting a formula with data argument:
plot(price ~ hd, data=Computers)


## ============================================================================
## exercise DATA FRAME DESC STATS and MULTIVAR DESC STATS:
## ============================================================================

## - install and load the package Ecdat
## - load the data set Computers
## - calculate some more descriptive statistics
## - additionally, try to show the distribution of some variables (e.g., histogram, density)
## - calculate correlations between the variables of interest

## ============================================================================
## solution to exercise DATA FRAME DESC STATS and MULTIVAR DESC STATS:
## ============================================================================

## save script as ex12a.R
## e-mail solution script to john.odonoghue@teradata.com