## ############################################################################
## R Introduction and Best Practice
## Dimensionality Reduction:
## Principal Components Analysis (PCA) and Factor Analysis (FA)
## June 2015
## ############################################################################

#rm(list=ls())

## ============================================================================
## global variables
## ============================================================================

path.raw <- "/Users/in186000/data-td-sync/teradata-project/training-knowledge-exchange/r-training-general/"
#path.raw <- "C:/data-sync/teradata-work/teradata-project/training-knowledge-exchange/r-training-general/"
path.r <- paste0(path.raw, "")
path.dat <- paste0(path.raw, "data/")

## ============================================================================
## load packages
## ============================================================================

library(gdata)   ## trim()

## ############################################################################
## ############################################################################
##
## Principal Components Analysis (PCA)
##
## ############################################################################
## ############################################################################

## ============================================================================
## load data (R-package)
## ============================================================================

library(FactoMineR)
data(decathlon)
decathlon <- data.frame(decathlon)
names(decathlon) <- tolower(names(decathlon))
names(decathlon)
str(decathlon)

## only 41 observations; really not enough!
## hence: no split in train and test set!

## ============================================================================
## data preparation (continued)
## ============================================================================

var.wch <- names(decathlon)[1:10]
dat.dec <- decathlon[var.wch]
head(dat.dec)

## scale variables:
dat.dec.scale <- data.frame(scale(dat.dec))
head(dat.dec.scale)

## save means and variances of original (training) data:
dat.dec.mean <- apply(dat.dec, 2, mean)
dat.dec.sd <- apply(dat.dec, 2, sd)

## correlation matrix:
dat.dec.cor <- cor(dat.dec.scale, use="pairwise")
dat.dec.cor

## ============================================================================
## how many principal components?
## ============================================================================

## calculate eigenvalues (and eigenvectors):
ev <- eigen(cor(dat.dec.scale))
ev <- eigen(dat.dec.cor)
ev

## raw eigenvalues:
ev$values

## eigenvalues as percentage of variance explained:
ev$values / sum(ev$values)

## cumulative sum:
cumsum(ev$values / sum(ev$values))

## 90% (92.06%) of variance in the first 7 principal components

## ============================================================================
## calculating PCA
## ============================================================================

## --------------------------------------------------------------------------
## calculate PCA of all components
## --------------------------------------------------------------------------

fit.pca1 <- prcomp(dat.dec.scale, center=TRUE, scale.=TRUE, tol=NULL)
fit.pca1
summary(fit.pca1)

## standard deviation of components:
fit.pca1$sdev

## standard deviation relative to first component:
fit.pca1$sdev / fit.pca1$sdev[1]

## --------------------------------------------------------------------------
## omit components with low variance
## --------------------------------------------------------------------------

## omit components where the sd is smaller than tol times the sd of the first component:
fit.pca2 <- prcomp(dat.dec.scale, center=TRUE, scale.=TRUE, tol=.67/1.8088)
summary(fit.pca2)

#fit.pca2$rotation
## the linear combination that is used to calculate the pc comps

## standardized loadings (=correlation between variables and components):
t(fit.pca2$rotation) * (fit.pca2$sdev) 

## biplot: plot components and subjects:
biplot(fit.pca2)

## plotting the first two components:
plot(fit.pca2$x)
text(fit.pca2$x, labels=rownames(dat.dec.scale), adj=c(0, 1), cex=0.5, srt=45)

## plotting other components:
plot(fit.pca2$x[, c(2, 3)])
text(fit.pca2$x[, c(2, 3)], labels=rownames(dat.dec.scale), adj=c(0, 1), cex=0.5, srt=45)

## ============================================================================
## scoring
## ============================================================================

## --------------------------------------------------------------------------
## scoring the training data: already calculated
## --------------------------------------------------------------------------

## scores for training data:
fit.pca2$x

## using predict function:
pred.pca.train <- predict(fit.pca2)
head(pred.pca.train)


## --------------------------------------------------------------------------
## scoring new data
## --------------------------------------------------------------------------

## create "new" data (just old data in this case):
dat.dec.test <- dat.dec[21:30,]

## standardize with means and sds from training data:
dat.dec.test.scale <- scale(dat.dec.test, center=dat.dec.mean, scale=dat.dec.sd)

## --------------------------------------------------------------------------
## using predict function with "new" data:
## --------------------------------------------------------------------------

pred.pca.test <- predict(fit.pca2, newdata=dat.dec.test.scale)
pred.pca.test

## --------------------------------------------------------------------------
## get original variables back (matrix multiplication):
## --------------------------------------------------------------------------

pred.dat.test.scale <- pred.pca.test %*% t(fit.pca2$rotation)
cor(pred.dat.test.scale[,"x100m"], dat.dec.test.scale[,"x100m"])
cor(pred.dat.test.scale[,"x400m"], dat.dec.test.scale[, "x400m"])

## predictions for original variables, but still scaled:
pred.dat.test.scale[,"x100m"] 

## have to be "un-standardized" to be meaningful: for one variable:
pred.dat.test.scale[,"x100m"] * dat.dec.sd[1] + dat.dec.mean[1]

## "un-standardizing" for all vars:
pred.dat.test <- t(t(pred.dat.test.scale) * dat.dec.sd + dat.dec.mean)

cor(pred.dat.test[,"x100m"], dat.dec.test[, "x100m"])
cor(pred.dat.test[,"x400m"], dat.dec.test[, "x400m"])

data.frame(pred.dat.test[,"x100m"], dat.dec.test[, "x100m"])
data.frame(pred.dat.test[,"x400m"], dat.dec.test[, "x400m"])


## ============================================================================
## model evaluation
## ============================================================================

## - predict variables in test set
## - calculate squared correlation of original and predicted data = var explained

cor(pred.dat.test[,"x100m"], dat.dec.test.scale[, "x100m"])^2
cor(pred.dat.test[,"x400m"], dat.dec.test.scale[, "x400m"])^2

## for all variables:
allcors <- mapply(
  FUN=function(i, j) cor(i, j)^2,  ## first argument is a column of... 
  data.frame(pred.dat.test),       ## ...this, and second argument is a column of...
  data.frame(dat.dec.test.scale))  ## ...this data frame.
allcors
mean(allcors)


## ============================================================================
## Exercise PCA olive
## ============================================================================

# Tasks:
# - Use the oliveoil dataset from the pdfCluster package: data(oliveoil)
# - to perform PCA (with all variables except the first two)
# - retain 95% of the variance
# - and do a biplot
# 
# 
# Important functions: 
#   scale, prcomp, biplot,

## ============================================================================
## Solution to Exercise PCA olive
## ============================================================================

library(pdfCluster)
data(oliveoil)
head(oliveoil)
dim(oliveoil)

## select variables:
dat.pca.ex <- oliveoil[-c(1:2)]

## PCA with all components:
fit.pca1.ex <- prcomp(dat.pca.ex, center=TRUE, scale.=TRUE, tol=NULL)
summary(fit.pca1.ex)

## 5 components for 95%,
## 7 components for 99%

## omit components where the sd is smaller than tol times the sd of the first component:
fit.pca2.ex <- prcomp(dat.pca.ex, center=TRUE, scale.=TRUE, tol=.57/1.9291)
summary(fit.pca2.ex)

## standardized loadings (=correlation between variables and components):
round(t(fit.pca2.ex$rotation) * (fit.pca2.ex$sdev), 3)
library(corrplot)
corrplot(t(fit.pca2.ex$rotation) * (fit.pca2.ex$sdev))

## biplot: plot components and subjects:
biplot(fit.pca2.ex)	

## plotting the first two components:
plot(fit.pca2.ex$x)


## ============================================================================
## Exercise PCA wine
## ============================================================================

# Tasks:
# - Use the wine dataset from the FactoMineR package: data(wine, package="FactoMineR")
# - to perform PCA (with all variables except the first two)
# - retain 95% of the variance
# - and do a biplot
# 
# 
# Important functions: 
#   scale, prcomp, biplot


## ============================================================================
## Solution to Exercise PCA wine
## ============================================================================

data(wine, package="FactoMineR")
head(wine)
dim(wine)

## select variables:
dat.pca.ex <- wine[-c(1:2)]

## PCA with all components:
fit.pca1.ex <- prcomp(dat.pca.ex, center=TRUE, scale.=TRUE, tol=NULL)
summary(fit.pca1.ex)

## 9 components for 95%,
## 14 components for 99%

## omit components where the sd is smaller than tol times the sd of the first component:
fit.pca2.ex <- prcomp(dat.pca.ex, center=TRUE, scale.=TRUE, tol=.63/3.879)
summary(fit.pca2.ex)

## standardized loadings (=correlation between variables and components):
round(t(fit.pca2.ex$rotation) * (fit.pca2.ex$sdev), 3)
corrplot(t(fit.pca2.ex$rotation) * (fit.pca2.ex$sdev))

## biplot: plot components and subjects:
biplot(fit.pca2.ex)	

## plotting the first two components:
plot(fit.pca2.ex$x)



## ############################################################################
## ############################################################################
##
## Factor Analysis (FA)
##
## ############################################################################
## ############################################################################

## ============================================================================
## data preparation (continued)
## ============================================================================

## using the same data as for PCA:

head(dat.dec.scale)

## means and sds of original (training) data:
dat.dec.mean <- apply(dat.dec, 2, mean)
dat.dec.sd <- apply(dat.dec, 2, sd)

## correlation matrix
dat.dec.cor <- cor(dat.dec.scale, use="pairwise")
dat.dec.cor

## ============================================================================
## how many factors?
## ============================================================================

## calculate eigenvalues (and eigenvectors):
ev <- eigen(cor(dat.dec.scale))

## raw eigenvalues:
ev$values

## scree-plot (do-it-yourself):
plot(ev$values, type="b", lwd=3, las=1, 
		ylab="eigenvalue", 
		xlab="number of factor")

## look for an "ellbow" in the plot (from right to left: rise of explained variance)

## scree plot with different packages:
library(psy)
scree.plot(dat.dec.scale)

library(nFactors)
ap <- parallel(subject=nrow(dat.dec.scale), var=ncol(dat.dec.scale), rep = 100, cent=0.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

## ============================================================================
## factor analysis
## ============================================================================

## --------------------------------------------------------------------------
## unrotated solution
## --------------------------------------------------------------------------

## fit factor model:
fit.fa1 <- factanal(dat.dec.scale, factors=4, rotation="none")
fit.fa1

## --------------------------------------------------------------------------
## rotated solution
## --------------------------------------------------------------------------

## orthogonal rotation:
fit.fa2 <- factanal(dat.dec.scale, factors=4, rotation="varimax")
fit.fa2

## factor 1: leg speed
## factor 2: arm strength
## factor 3: endurance
## factor 4: pole vault

## plots for interpretation:
lds <- fit.fa2$loadings[,c(1,2)]
plot(lds,type="n") # set up plot
text(lds,labels=names(dat.dec.scale),cex=.7) # add variable names 

## oblique rotation:
fit.fa3 <- factanal(dat.dec.scale, factors=4, rotation="promax") ## more rotations in package GPArotation
fit.fa3

## rotation probably not needed here... (low factor correlations)

## ============================================================================
## scoring
## ============================================================================

## --------------------------------------------------------------------------
## create "new" data (just old data in this case):
## --------------------------------------------------------------------------

dat.dec.test <- dat.dec[21:30,]

## standardize with means and sds from training data:
dat.dec.test.scale <- scale(dat.dec.test, center=dat.dec.mean, scale=dat.dec.sd)

## --------------------------------------------------------------------------
## predicting
## --------------------------------------------------------------------------

## saved scores for training data:
fit.fa2 <- factanal(dat.dec.scale, factors=4, rotation="varimax", scores="regression")
fit.fa2$scores

## problem:
## factanal doesn't save the factor-score coefficients
## (https://stat.ethz.ch/pipermail/r-help/2002-April/020278.html)

## calculation "by hand" for training data:
fit.fa2.coef <- solve(fit.fa2$correlation) %*% fit.fa2$loadings
pred.fa.train <- as.matrix(dat.dec.scale) %*% fit.fa2.coef
pred.fa.train

## calculation "by hand" for test data:
pred.fa.test <- as.matrix(dat.dec.test.scale) %*% fit.fa2.coef
pred.fa.test

## --------------------------------------------------------------------------
## get original variables back (matrix multiplication):
## --------------------------------------------------------------------------

## get original variables back (matrix multiplication):
pred.dat.test.scale <- pred.fa.test %*% t(fit.fa2$loadings)
cor(pred.dat.test.scale[,"x100m"], dat.dec.test.scale[,"x100m"])
cor(pred.dat.test.scale[,"shot.put"], dat.dec.test.scale[, "shot.put"])

## have to be "un-standardized" to be meaningful: for one variable:
pred.dat.test.scale[,"x100m"] * dat.dec.sd[1] + dat.dec.mean[1]

## "un-standardizing" for all vars:
pred.dat.test <- t(t(pred.dat.test.scale) * dat.dec.sd + dat.dec.mean)

## ============================================================================
## model evaluation
## ============================================================================

## - predict variables in test set
## - calculate squared correlation of original and predicted data = var explained

cor(pred.dat.test[,"x100m"], dat.dec.test.scale[, "x100m"])^2
cor(pred.dat.test[,"shot.put"], dat.dec.test.scale[, "shot.put"])^2

## for all variables:
allcors <- mapply(
  FUN=function(i, j) cor(i, j)^2, 
  data.frame(pred.dat.test), 
  data.frame(dat.dec.test.scale))
mean(allcors)

## for the (whole) training set:
allcors <- mapply(
  FUN=function(i, j) cor(i, j)^2, 
  data.frame(as.matrix(pred.fa.train) %*% t(fit.fa2$loadings)), 
  data.frame(dat.dec.scale))
mean(allcors)

## ============================================================================
## Exercise FA olive
## ============================================================================


## ============================================================================
## Solution to Exercise FA olive
## ============================================================================

library(pdfCluster)
data(oliveoil)

head(oliveoil)

## only numerical variables:
var.wch.ex <- names(oliveoil)[-(1:2)]
dat.fa.ex <- oliveoil[var.wch.ex]

head(dat.fa.ex)

## number of factors:

## calculate eigenvalues (and eigenvectors):
ev <- eigen(cor(dat.fa.ex))

## raw eigenvalues:
ev$values

## scree-plot (do-it-yourself):
plot(ev$values, type="b", lwd=3, las=1, 
    ylab="eigenvalue", 
    xlab="number of factor")

## look for an "ellbow" in the plot (from right to left: rise of explained variance)

## scree plot with different packages:
library(psy)
scree.plot(dat.fa.ex)

library(nFactors)
ap <- parallel(subject=nrow(dat.fa.ex), var=ncol(dat.fa.ex), rep = 100, cent=0.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

## orthogonal rotation (unrotated solution):
fit.fa2.ex <- factanal(dat.fa.ex, factors=4, rotation="varimax")
fit.fa2.ex

## interpretation:
## f1: palm-acids
## f2: linolenic, archidic and eicosenoic acid
## f3: lionleic acid
## f4: stearic acid

## promax rotation (rotated solution):
fit.fa3.ex <- factanal(dat.fa.ex, factors=4, rotation="promax")
fit.fa3.ex

## interpretation:
## f1: palm-acids
## f2: linolenic, archidic and eicosenoic acid
## f3: lionleic acid
## f4: stearic acid
## f1 and f3 are negatively correlated




## ============================================================================
## Exercise FA wine
## ============================================================================


## ============================================================================
## Solution to Exercise FA wine
## ============================================================================

data(wine, package="FactoMineR")
dim(wine)

## only numerical variables:
var.wch.ex <- names(wine)[-(1:2)]
dat.fa.ex.allnum <- wine[var.wch.ex]

library(corrplot)
corrplot(cor(dat.fa.ex.allnum), type="upper", order="hclust", tl.col="black", tl.srt=45)

cols <- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(cor(dat.fa.ex.allnum), col=cols, symm=TRUE)

## select variables:
varnames.include <- c(
    "Acidity",
    "Spice.before.shaking",
    "Bitterness",
    "Odor.Intensity.before.shaking",
    "Flower.before.shaking",
    "Nuance",
    "Alcohol",
    "Aroma.persistency",
    "Aroma.intensity",
    "Quality.of.odour",
    "Fruity.before.shaking",
    "Aroma.quality.before.shaking",
    "Harmony",
    "Intensity",
    "Aroma.quality",
    "Balance",
    "Smooth")


dat.fa.ex <- dat.fa.ex.allnum[(names(dat.fa.ex.allnum) %in% varnames.include)]
dim(dat.fa.ex)
dim(dat.fa.ex.allnum)


## number of factors:

## calculate eigenvalues (and eigenvectors):
ev <- eigen(cor(dat.fa.ex))

## raw eigenvalues:
ev$values

## scree-plot (do-it-yourself):
plot(ev$values, type="b", lwd=3, las=1, 
    ylab="eigenvalue", 
    xlab="number of factor")

## look for an "ellbow" in the plot (from right to left: rise of explained variance)

## scree plot with different packages:
library(psy)
scree.plot(dat.fa.ex)

library(nFactors)
ap <- parallel(subject=nrow(dat.fa.ex), var=ncol(dat.fa.ex), rep = 100, cent=0.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)


## orthogonal rotation (unrotated solution):
fit.fa2.ex <- factanal(dat.fa.ex, factors=3, rotation="varimax")
fit.fa2.ex

## interpretation:
## f1: Aroma & Odour
## f2: Balance & Smoothness, Harmony
## f3: Spicyness

## promax rotation (rotated solution):
fit.fa3.ex <- factanal(dat.fa.ex, factors=3, rotation="promax")
fit.fa3.ex

## interpretation:
## f1: Balance, Smoothness & Harmony
## f2: Aroma & Frutiness
## f3: Spicyness
## Balance and Spicyness are negatively correlated



