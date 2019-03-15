## ############################################################################
## R Introduction and Best Practice
## Cluster Analysis
## July 2016
## ############################################################################

#rm(list=ls())

## ============================================================================
## global variables
## ============================================================================

## John
path.raw <- "C:/Projects/Vodafone_TR/"
#path.raw <- "C:/data-sync/teradata-work/teradata-project/training-knowledge-exchange/r-training-general/"
path.r <- paste0(path.raw, "r-scripts/")
path.dat <- paste0(path.raw, "data/")
path.tmp <- paste0(path.raw, "tmp/")


## ============================================================================
## load packages
## ============================================================================

library(gdata)   ## trim()

## ============================================================================
## load data (from file)
## ============================================================================

load(file=paste0(path.dat, "twm_customer_analysis_train.Rdata")) ## contains dat.train
load(file=paste0(path.dat, "twm_customer_analysis_test.Rdata"))  ## contains dat.test


## ############################################################################
## ############################################################################
##
## Cluster Analysis
##
## ############################################################################
## ############################################################################

## ============================================================================
## Partitioning methods: kmeans clustering
## ============================================================================

library(cluster)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## very simple kmeans example
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## load data
data(ruspini)

## inspect data
head(ruspini)
plot(ruspini)

## fit 4 cluster-solution
rusp.kmeans <- kmeans(ruspini, centers=4)
rusp.kmeans

## inspect solution
plot(ruspini, col=rusp.kmeans$cluster, pch=as.character(rusp.kmeans$cluster), cex=0.7)
points(rusp.kmeans$centers, col = 1:5, pch = 19)

## of note: clusters can differ for each run!
## better: use nstart argument for multiple runs

## fit 4 cluster-solution (10 times):
rusp.kmeans <- kmeans(ruspini, centers=4, nstart=10)
rusp.kmeans

plot(ruspini, col=rusp.kmeans$cluster, pch=as.character(rusp.kmeans$cluster), cex=0.7)

## ------------------------------------------------------------------------
## how many clusters?
## ------------------------------------------------------------------------

ngroups <- 2:10
fit.kmeans.ng <- lapply(ngroups, function(i) kmeans(ruspini, centers=i, nstart=10))

#within ss -> within cluster sum of squares
#(how spread out the clusters are from their respective centres)
wss <- sapply(fit.kmeans.ng, function(i) sum(i$withinss))
wss
plot(wss ~ ngroups, type="b")

library(fpc)
pamk.best <- pamk(ruspini, krange=ngroups)
pamk.best
pamk.best$nc

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## more sophisticated kmeans example
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## select variables to use for clustering:
## use only continous variables in this case

#cat("\"", names(dat.train), "\"\n", sep="\", \"")

var.num <- c( "income", "age", "years_with_bank", "nbr_children",
		"avg_ck_bal", "avg_sv_bal", "avg_cc_tran_amt", "avg_cc_tran_cnt",
		"avg_ck_tran_amt", "avg_ck_tran_cnt", "avg_sv_tran_amt",
		"avg_sv_tran_cnt")
var.wch <- c( "income", "age", "avg_ck_bal", "avg_sv_bal", "avg_cc_tran_cnt",
		"avg_ck_tran_cnt", "avg_sv_tran_cnt")

## scale data:
dat.kmeans <- data.frame(scale(dat.train[var.wch]))
head(dat.kmeans)
summary(dat.train[var.wch])
summary(dat.kmeans)
fit.kmeans.g3 <- kmeans(dat.kmeans, centers=3, nstart=10)
fit.kmeans.g3

## ------------------------------------------------------------------------
## how many clusters?
## ------------------------------------------------------------------------

set.seed(42)
ngroups <- 5:20
fit.kmeans.ng <- lapply(ngroups, function(i) kmeans(dat.kmeans, centers=i, nstart=10))
wss <- sapply(fit.kmeans.ng, function(i) sum(i$withinss))
wss
plot(wss ~ ngroups, type="b")

library(fpc)
pamk.best <- pamk(dat.kmeans, krange=ngroups)
pamk.best
pamk.best$nc

## ------------------------------------------------------------------------
## understanding the results
## ------------------------------------------------------------------------

set.seed(42)

fit.kmeans.g8 <- kmeans(dat.kmeans, centers=8, nstart=25)
fit.kmeans.g8

fit.kmeans.g8$cluster

## size of clusters:
table(fit.kmeans.g8$cluster)

## mean of variables in clusters:
aggregate(dat.train[var.wch], by=list(fit.kmeans.g8$cluster), mean)
aggregate(dat.kmeans[var.wch], by=list(fit.kmeans.g8$cluster), mean)

## boxplots:
par(mfrow=c(2,1), mar=c(3,2,1,1))
for (i in 1:2)
  boxplot(dat.train[[var.wch[i]]] ~ fit.kmeans.g8$cluster, main=var.wch[i])

## line plots:
g8means.scaled <- aggregate(dat.kmeans[var.wch], by=list(fit.kmeans.g8$cluster), mean)
g8means.scaled.transposed <- t(g8means.scaled[,-1])

par(mfrow=c(1,1), mar=c(7,3,1,1))
matplot(g8means.scaled.transposed, type="l", xaxt="n")
axis(side=1, at=1:nrow(g8means.scaled.transposed), labels=rownames(g8means.scaled.transposed), las=3)
legend("topright", legend=1:8, col=1:6, lty=1:5) ## see matplot help for col/lty

## and so on...

## ------------------------------------------------------------------------
## scoring other data
## ------------------------------------------------------------------------

library(clue)
dat.test$kmeans.g8 <- cl_predict(fit.kmeans.g8, newdata=scale(dat.test[var.wch]))
dat.test$kmeans.g8

table(dat.test$kmeans.g8)

## ============================================================================
## distance metrics
## ============================================================================

## euclidean distance:
dist(dat.kmeans[1:5,])

## manhattan distance:
dist(dat.kmeans[1:5,], method="manhattan")

## minkowski distance:
dist(dat.kmeans[1:5,], method="minkowski", p=1)
dist(dat.kmeans[1:5,], method="minkowski", p=2)

library(cluster)

## euclidean distance:
daisy(dat.kmeans[1:5,])

## manhattan distance:
daisy(dat.kmeans[1:5,], metric="manhattan")

## gower distance:
daisy(dat.kmeans[1:5,], metric="gower")
daisy(dat.train[1:5, c("gender", "marital_status")], metric="gower")

## ============================================================================
## Partitioning methods: k-medoid clustering
## ============================================================================

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## very simple k-medoid example
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## fit 4-cluster solution:
rusp.pam <- pam(ruspini, 4)
rusp.pam

plot(rusp.pam, which.plots=1)
plot(rusp.pam, which.plots=2)

rusp.si <- silhouette(rusp.pam)
mean(rusp.si[,3])

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## more sophisticated k-medoid example
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fit.pam.g8 <- pam(dat.kmeans, k=8, metric="euclidean", stand=TRUE)
fit.pam.g8
str(fit.pam.g8)

plot(fit.pam.g8, which.plots=1)
plot(fit.pam.g8, which.plots=2)

# average silhouette width
fit.pam.g8.si <- silhouette(fit.pam.g8)
mean(fit.pam.g8.si[,3])

## ------------------------------------------------------------------------
## understand results
## ------------------------------------------------------------------------

fit.pam.g8$clustering

## size of clusters:
table(fit.pam.g8$clustering)

## mean of variables in clusters:
aggregate(dat.train[var.wch], by=list(fit.pam.g8$clustering), mean)
aggregate(dat.kmeans[var.wch], by=list(fit.pam.g8$clustering), mean)

## boxplots:
par(mfrow=c(2,1), mar=c(3,2,1,1))
for (i in 1:2)
	boxplot(dat.train[[var.wch[i]]] ~ fit.pam.g8$clustering, main=var.wch[i])

## line plots:
g8means.scaled <- aggregate(dat.kmeans[var.wch], by=list(fit.pam.g8$clustering), mean)
g8means.scaled.transposed <- t(g8means.scaled[,-1])

par(mfrow=c(1,1), mar=c(7, 3, 1, 1))
matplot(g8means.scaled.transposed, type="l", xaxt="n")
axis(side=1, at=1:nrow(g8means.scaled.transposed), labels=rownames(g8means.scaled.transposed), las=3)
legend("topleft", legend=1:8, col=1:6, lty=1:5) ## see matplot help for col/lty

## ------------------------------------------------------------------------
## scoring other data
## ------------------------------------------------------------------------

library(clue)
dat.test.scaled <- dat.test$pam.g8
dat.test$pam.g8 <- cl_predict(fit.pam.g8, newdata=scale(dat.test[var.wch]))

#Warning message:
#  In cl_predict.pam(fit.pam.g8, newdata = dat.test.scaled) :
#  Standardization is currently not supported.
#This just means you also need to scale your input data here (in this case the test set)

dat.test$pam.g8

table(dat.test$pam.g8)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clustering of non-numeric data
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## select non-numeric data for demonstration:
names(dat.train)
var.wch.fact <- c("gender", "marital_status", "age")

distmat <- daisy(dat.train[var.wch.fact], metric="gower")
str(distmat)

fit.pam.fact.g6 <- pam(distmat, k=6)
fit.pam.fact.g6

plot(fit.pam.fact.g6)

ngroups <- 2:10
pamk.best <- pamk(distmat, krange=ngroups)
pamk.best
pamk.best$nc

## inspect results:
table(dat.train[,"gender"], fit.pam.fact.g6$clustering)

table(dat.train[,"marital_status"], fit.pam.fact.g6$clustering)
prop.table(table(dat.train[,"marital_status"], fit.pam.fact.g6$clustering), margin=2) # gives marginal percentages for each cluster
round(
  prop.table(table(dat.train[,"marital_status"], fit.pam.fact.g6$clustering), margin=2)
  ,2)

library(Hmisc)
table(cut2(dat.train[,"age"],g=3), fit.pam.fact.g6$clustering)
## mean of variables in clusters:
aggregate(dat.train["age"], by=list(fit.pam.fact.g6$clustering), mean)

c("single young women", "other women", "married older women", "married older men"
  ,"single young men", "separated women", "other men")

## ============================================================================
## Exercise CLUSTERING (PARTITIONING METHODS) TWM
## ============================================================================

# Tasks:
# - Use the data file twm_customer_analysis_train.Rdata
# - Cluster the subscribers based on the variables
#   . income
#   . age
#   . avg_cc_tran_amt
# - find the optimal number of clusters
# - interpret the results
#
# Important functions:
#   scale, kmeans, pam, pamk, matplot, cl_predict (library clue)

## ============================================================================
## Solution to Exercise CLUSTERING (PARTITIONING METHODS) TWM
## ============================================================================


## ============================================================================
## Hierarchical Clustering
## ============================================================================

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## very simple hierarchical clustering example
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## calculate distance matrix:
ruspini.dist <- dist(ruspini, method="euclidean")

## calulate clusters:
rusp.hclust <- hclust(ruspini.dist, method="average")
rusp.hclust

#rusp.hclust <- hclust(ruspini.dist^2, method="ward")

## plot dendrogram:
plot(rusp.hclust)
abline(h=38, lty=2, col="red", lwd=4)
#abline(h=10000, lty=2, col="red", lwd=4)

## y-axis of the dendrogram:
## height tells you at which distance clusters were joined together
##?plot.hclust

## more fun with denodrogram using package dendextend from Tal Galili:
library(dendextend)
library(dendextendRcpp)
dend1 <- as.dendrogram(rusp.hclust)
plot(dend1)

dend1 <- set(dend1, "labels_col", k=4) ## (could add specific colors before k argument)
plot(dend1)

dend1 <- set(dend1, "branches_k_color", k=4)
plot(dend1)
circlize_dendrogram(dend1)

## specify number of clusters:
rusp.hclust.g2 <- cutree(rusp.hclust, 2)
rusp.hclust.g2

rusp.hclust.g3 <- cutree(rusp.hclust, 3)
rusp.hclust.g4 <- cutree(rusp.hclust, 4)
rusp.hclust.g5 <- cutree(rusp.hclust, 5)
rusp.hclust.g6 <- cutree(rusp.hclust, 6)

plot(ruspini, col=rusp.hclust.g6, pch=as.character(rusp.hclust.g6), cex=0.7)
plot(ruspini, col=rusp.hclust.g5, pch=as.character(rusp.hclust.g5), cex=0.7)
plot(ruspini, col=rusp.hclust.g4, pch=as.character(rusp.hclust.g4), cex=0.7)
plot(ruspini, col=rusp.hclust.g3, pch=as.character(rusp.hclust.g3), cex=0.7)
plot(ruspini, col=rusp.hclust.g2, pch=as.character(rusp.hclust.g2), cex=0.7)



## ------------------------------------------------------------------------
## different linkage criterion
## ------------------------------------------------------------------------

## calulate clusters:
rusp.hclust <- hclust(ruspini.dist, method="single")
plot(rusp.hclust)

## with dendextend package (save dendrogram for later)
dend2 <- as.dendrogram(rusp.hclust)
plot(dend2)
circlize_dendrogram(dend2)

dend2 <- set(dend2, "labels_col", k=4) ## (could add specific colors before k argument)
dend2 <- set(dend2, "branches_k_color", k=4)
plot(dend2)

## calulate clusters:
rusp.hclust <- hclust(ruspini.dist^2, method="ward.D") ## use ward.D2 with un-squared dist mat
## Euclidean distance has to be squared to correctly use Ward's method!
plot(rusp.hclust)



## compare cluster solutions (dendrograms) with dendextend package:
dend_diff(dend1, dend2)  ## plotting trees side by side and highlighting distinct edges in red

## tanglegram:
tanglegram(dend1, dend2, common_subtrees_color_branches=TRUE)

## numerically (lower is better):
entanglement(dend1, dend2)

dendlist = list(dend1,dend2)
## Baker's gamma:
cor.dendlist(dendlist(dend1,dend2))

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## more sophisticated hierarchical clustering example
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## same variables as in k-means
var.wch <- c( "income", "age", "avg_ck_bal", "avg_sv_bal", "avg_cc_tran_cnt",
              "avg_ck_tran_cnt", "avg_sv_tran_cnt")

## calculate distance matrix:
dat.kmeans.dist <- dist(dat.kmeans, method="euclidean")

## calulate clusters:
fit.hclust <- hclust(dat.kmeans.dist^2, method="ward.D")
#fit.hclust <- hclust(dat.kmeans.dist, method="average")

## ------------------------------------------------------------------------
## how many clusters?
## ------------------------------------------------------------------------

## plot dendrogram:
plot(fit.hclust)
abline(h=200, lty=2, col="red")

fit.hclust.g8 <- cutree(fit.hclust, h=200)
fit.hclust.g8 <- cutree(fit.hclust, k=8)

## ------------------------------------------------------------------------
## understanding the results
## ------------------------------------------------------------------------

## size of clusters:
table(fit.hclust.g8)

## mean of variables in clusters:
aggregate(dat.train[var.wch], by=list(fit.hclust.g8), mean)
aggregate(dat.kmeans[var.wch], by=list(fit.hclust.g8), mean)

## boxplots:
par(mfrow=c(2,1), mar=c(3,2,1,1))
for (i in 1:2)
	boxplot(dat.train[[var.wch[i]]] ~ fit.hclust.g8, main=var.wch[i])

## line plots:
means.scaled <- aggregate(dat.kmeans[var.wch], by=list(fit.hclust.g8), mean)
means.scaled.transposed <- t(means.scaled[,-1])
matplot(means.scaled.transposed, type="l", xaxt="n")
axis(side=1, at=1:nrow(means.scaled.transposed), labels=rownames(means.scaled.transposed), las=3)
legend("topright", legend=1:8, col=1:6, lty=1:5) ## see matplot help for col/lty

## ------------------------------------------------------------------------
## scoring other data
## ------------------------------------------------------------------------

## - not directly possible with hierarchical clustering methods
## - use another classification algorithm (like Random Forest, SVM, KNN)

## ============================================================================
## Exercise CLUSTERING (HIERARCHICAL METHODS) TWM
## ============================================================================

# Tasks:
# - Use the data file twm_customer_analysis_train.Rdata
# - Cluster the subscribers based on the variables
#   . income
#   . age
#   . avg_cc_tran_amt
# - find the optimal number of clusters
# - interpret the results
#
# Important functions:
#   scale, dist, daisy, hclust, cutree, matplot

## ============================================================================
## Solution to Exercise HIERARCHICAL (PARTITIONING METHODS) TWM
## ============================================================================

