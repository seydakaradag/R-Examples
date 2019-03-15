## ############################################################################
## R Introduction and Best Practice
## Missing Values
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

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## data
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load(file=paste0(path.dat, "twm_customer_analysis_train.Rdata")) ## contains dat.train


## ############################################################################
## ############################################################################
##
## Missing Values
##
## ############################################################################
## ############################################################################


## ============================================================================
## missing values: imputation
## ============================================================================

library(Ecdat)
data(Computers)

## create some missing values:
comp.miss <- Computers
comp.miss$price[1:260] <- NA
comp.miss$speed[201:300] <- NA
comp.miss$cd[251:400] <- NA

dim(comp.miss)
dim(na.omit(comp.miss))

mean(comp.miss$price)
mean(comp.miss$price, na.rm=TRUE)
mean(Computers$price)

## a lot of packages for imputation
## one possibility:


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## using package VIM
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(VIM)


## ------------------------------------------------------------------------
## inspect the missing values
## ------------------------------------------------------------------------

## overview of missing values:
aggr(comp.miss)
summary(aggr(comp.miss))

## histogram with missing values:
histMiss(comp.miss$price)
histMiss(comp.miss$speed)

## scatter plots:
marginplot(comp.miss[c("price", "hd")])
## values in price not missing at random: associated with hd size (at minimum)

marginmatrix(comp.miss[c("price", "hd", "ram")])


## ------------------------------------------------------------------------
## imputation with plausible values
## ------------------------------------------------------------------------

## doesn't make sense in this case!

## make a copy of the data set for comparison:
comp.valimp <- comp.miss
comp.valimp$price[is.na(comp.miss$price)] <- 0

## mean of variable with missings and imputed variables:
mean(comp.miss$price, na.rm=TRUE)
mean(comp.valimp$price)
## pulls down the mean!

## mean of original (unknown) variable:
mean(Computers$price)

## ------------------------------------------------------------------------
## MEAN imputation
## ------------------------------------------------------------------------

## make a copy of the data set for comparison:
comp.meanimp <- comp.miss
comp.meanimp$price[is.na(comp.miss$price)] <- mean(comp.miss$price, na.rm=TRUE)

## mean of variable with missings and imputed variables:
mean(comp.miss$price, na.rm=TRUE)
mean(comp.meanimp$price)

## mean of original (unknown) variable:
mean(Computers$price)


## for factors: mode (most common value):
comp.meanimp$cd[is.na(comp.miss$cd)] <- names(sort(-table(comp.miss$cd)))[1]

aggr(comp.meanimp) ## needs to be redone for all variables individually

## ------------------------------------------------------------------------
## HOT DECK imputation
## ------------------------------------------------------------------------

## implementation of hot deck algorithm
## involves sorting a dataset according to any of a number of variables
## The technique then finds the first missing value and uses the cell value
## immediately prior to the data that are missing to impute the missing value

## find highest correlation:
cor(comp.miss[c("price", "speed", "hd", "ram", "screen", "ads", "trend")], use="pair")

## impute:
comp.hotdeck <- hotdeck(comp.miss,
    variable=c("price", "speed", "cd"),
    ord_var=c("ram", "hd", "screen", "speed"))

aggr(comp.hotdeck)

## mean of variable with missings and imputed variables:
mean(comp.miss$price, na.rm=TRUE)
mean(comp.hotdeck$price)

## mean of original (unknown) variable:
mean(Computers$price)

## ------------------------------------------------------------------------
## KNN imputation
## ------------------------------------------------------------------------

## nearest neighbour algorithm
## based on dist_var (distance variable) finds a k closest neighbours for
## observation and imput missing values based on those in the neighbourhood

comp.knn <- kNN(comp.miss,
                variable=c("price", "speed", "cd"),
                k=5, dist_var=c("ram", "hd", "screen", "speed"))

aggr(comp.knn)

## mean of variable with missings and imputed variables:
mean(comp.miss$price, na.rm=TRUE)
mean(comp.knn$price)

## mean of original (unknown) variable:
mean(Computers$price)

## ------------------------------------------------------------------------
## REGRESSION imputation
## ------------------------------------------------------------------------

## needs to be done for each variable individually:
comp.reg <- regressionImp(price ~ ram + hd + screen, data=comp.miss)
comp.reg <- regressionImp(speed ~ ram + hd + screen, data=comp.reg)
comp.reg <- regressionImp(cd ~ ram + hd + screen, data=comp.reg)

aggr(comp.reg)

## mean of variable with missings and imputed variables:
mean(comp.miss$price, na.rm=TRUE)
mean(comp.reg$price)

## mean of original (unknown) variable:
mean(Computers$price)

## ------------------------------------------------------------------------
## DECISION TREE imputation (bagged trees in caret)
## ------------------------------------------------------------------------

## unfortunately, not implemented in the VIM package
## bagged trees implemented in caret package:

library(caret)

## only workes for numeric variables:
vars.num <- c("price", "speed", "hd", "ram", "screen")

## estimate imputation parameters:
dataprep.est <- preProcess(comp.miss[vars.num], method="bagImpute")
dataprep.est

## create new dataset with imputed variables:
comp.btree <- comp.miss
comp.btree[vars.num] <- predict(dataprep.est, comp.btree[vars.num])

aggr(comp.btree)

## mean of variable with missings and imputed variables:
mean(comp.miss$price, na.rm=TRUE)
mean(comp.btree$price)

## mean of original (unknown) variable:
mean(Computers$price)


## ------------------------------------------------------------------------
## RANDOM FOREST imputation
## ------------------------------------------------------------------------

## unfortunately, not implemented in the VIM package
## implemented in missForest package:

library(missForest)

## computationally intensive:
comp.rf.all <- missForest(comp.miss)
str(comp.rf.all)

## OOB error estimate:
comp.rf.all$OOBerror

## data:
comp.rf <- comp.rf.all$ximp

aggr(comp.rf)

## mean of variable with missings and imputed variables:
mean(comp.miss$price, na.rm=TRUE)
mean(comp.rf$price)

## mean of original (unknown) variable:
mean(Computers$price)


## ------------------------------------------------------------------------
## IRMI imputation
## ------------------------------------------------------------------------

## iterative robust model-based imputation (M. Templ, A. Kowarik, P. Filzmoser (2011))
comp.irmi <- irmi(comp.miss)

aggr(comp.irmi)

## mean of variable with missings and imputed variables:
mean(comp.miss$price, na.rm=TRUE)
mean(comp.irmi$price)

## mean of original (unknown) variable:
mean(Computers$price)

## ------------------------------------------------------------------------
## PCA imputation
## ------------------------------------------------------------------------

## unfortunately, not implemented in the VIM package
## implemented in missMDA package:

library('missMDA')

## only works for numeric columns
vars.num <- colnames(comp.miss)[sapply(comp.miss, is.numeric)]
vars.num

## store copy of comp.miss
comp.PCA <- comp.miss

comp.PCA.num <- imputePCA(comp.miss[vars.num])

## need to specify completeObs
attributes(comp.PCA.num)
comp.PCA[vars.num] <- comp.PCA.num$completeObs

aggr(comp.PCA)

## mean of variable with missings and imputed variables:
mean(comp.miss$price, na.rm=TRUE)
mean(comp.PCA$price)

## mean of original (unknown) variable:
mean(Computers$price)


## ============================================================================
## exercise IMPUTATION:
## ============================================================================

## Tasks:
## - use the Computers dataset from the exercises before
## - create a new data frame:
##   - replace the variable price in rows 201:400 with NA
##   - replace the variable hd in rows 101:300 with NA
##
## - visualize the missing values (use the VIM package)
## - impute the missing values
## - compare the means and standard deviations of
##   - original variables
##   - variables with missings
##   - imputed variables
##
## Important functions (help pages):
##         ?aggr
## ?histMiss
## kNN, hotdeck, regressionImp

## ============================================================================
## solution to exercise IMPUTATION:
## ============================================================================

library(VIM)
library(Ecdat)
data(Computers)

## create some missing values:
comp.ex <- Computers
comp.ex$price[201:400] <- NA
comp.ex$hd[101:300] <- NA

## inspect missing values:

## overview of missing values:
aggr(comp.ex)
summary(aggr(comp.ex))

## histogram with missing values:
histMiss(comp.ex$price)
histMiss(comp.ex$speed)

## impute wit kNN:
comp.knn <- kNN(comp.ex, variable=c("price", "hd"),
                k=5, dist_var=c("ram", "hd", "screen", "speed"))

## impute with IRMI:
comp.irmi <- irmi(comp.ex)

## means of price with missings, imputed price and original price:
mean(comp.ex$price, , na.rm=TRUE)
mean(comp.knn$price)
mean(comp.irmi$price)
mean(Computers$price)

## sds of price with missings, imputed price and original price:
sd(comp.ex$price, na.rm=TRUE)
sd(comp.knn$price)
sd(comp.irmi$price)
sd(Computers$price)


## means of hd with missings, imputed hd and original hd:
mean(comp.ex$hd, , na.rm=TRUE)
mean(comp.knn$hd)
mean(comp.irmi$hd)
mean(Computers$hd)

## means of hd with missings, imputed hd and original hd:
sd(comp.ex$hd, na.rm=TRUE)
sd(comp.knn$hd)
sd(comp.irmi$hd)
sd(Computers$hd)






