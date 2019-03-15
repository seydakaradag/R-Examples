## ############################################################################
## R Introduction and Best Practice
## CART Modelling: Decision Trees and Random Forests
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
## packages
## ============================================================================

if (!("gdata" %in% installed.packages())){
  install.packages("gdata")
}
library(gdata)

## ############################################################################
## ############################################################################
##
## Decision Trees (CART modelling)
##
## ############################################################################
## ############################################################################

## ============================================================================
## data (from file)
## ============================================================================

load(file=paste0(path.dat, "twm_customer_analysis_train.Rdata")) ## contains dat.train
load(file=paste0(path.dat, "twm_customer_analysis_test.Rdata"))  ## contains dat.test

## ============================================================================
## with library rpart
## ============================================================================

library(rpart)

## ------------------------------------------------------------------------
## basic call
## ------------------------------------------------------------------------

fit.tree0 <- rpart(ccacct ~ income + age + years_with_bank + nbr_children + gender +
                     marital_status + ckacct + svacct + avg_sv_bal + avg_ck_bal +
                     avg_sv_tran_amt + avg_ck_tran_amt +
                     avg_sv_tran_cnt + avg_ck_tran_cnt,
                   data=dat.train)

## inspect tree and cross-validation error:
print(fit.tree0)
printcp(fit.tree0)

## plotting the tree:
plot(fit.tree0)
text(fit.tree0)

library(rattle)
#plot(1)
fancyRpartPlot(fit.tree0)

## or just save to a file:
pdf(paste0(path.tmp,"rpart.pdf"))
fancyRpartPlot(fit.tree0, palettes=c("Blues","Oranges"))
dev.off()


## ------------------------------------------------------------------------
## more parameters to configure tree
## ------------------------------------------------------------------------

fit.tree1 <- rpart(ccacct ~ income + age + years_with_bank + nbr_children + gender +
                     marital_status + ckacct + svacct + avg_sv_bal + avg_ck_bal +
                     avg_sv_tran_amt + avg_ck_tran_amt +
                     avg_sv_tran_cnt + avg_ck_tran_cnt,
                   data=dat.train,
                   method="class",            ## classification tree (=default; "anova" for reg.tree)
                   parms=list(
                     split="information"), ## split on information gain (default: gini)
                   control=rpart.control(
                     minsplit=60,       ## minimum n to attempt split (=minbucket*3)
                     minbucket=20,      ## minimum n in leaf node (=minsplit/3)
                     cp=0.01,           ## min complexity parameter decrease for split
                     xval=5,            ## number of cross validations to determine error
                     maxdepth=10))      ## maximum depth of the tree

print(fit.tree1)
printcp(fit.tree1)

## excluding most savings-related variables (perfect separation):
## (not necessary, but just for getting a larger tree):
fit.tree1 <- rpart(ccacct ~ income + age + years_with_bank + nbr_children + gender +
                     marital_status + ckacct + svacct + avg_ck_bal +
                     avg_ck_tran_amt + avg_ck_tran_cnt,
                     data=dat.train,
                     method="class",            ## classification tree (=default; "anova" for reg.tree)
                     parms=list(
                     split="information"), ## split on information gain (default: gini)
                     control=rpart.control(
                         minsplit=60,       ## minimum n to attempt split (=minbucket*3)
                         minbucket=20,      ## minimum n in leaf node (=minsplit/3)
                         cp=0.01,           ## min complexity parameter decrease for split
                         xval=5,            ## number of cross validations to determine error
                         maxdepth=10))      ## maximum depth of the tree

## inspect tree and cross-validation error:
print(fit.tree1)
printcp(fit.tree1)

## very detailed tree summary, with surrogate splits:
summary(fit.tree1)

## ------------------------------------------------------------------------
## variable importance
## ------------------------------------------------------------------------

fit.tree1$variable.importance
#fit.tree0$variable.importance

par(mar=c(6,8,2,5))
barplot(fit.tree1$variable.importance, horiz=TRUE, las=1, cex.names=.7)
par(mar=c(5, 4, 4, 2) + 0.1)

## ------------------------------------------------------------------------
## plotting the tree
## ------------------------------------------------------------------------

## basic plot (ugly):
plot(fit.tree1)
text(fit.tree1, col="red",cex=1.3)

## nicer plot:
#plot(1)
fancyRpartPlot(fit.tree1)

## or just save to a file:
pdf(paste0(path.tmp,"rpart.pdf"))
fancyRpartPlot(fit.tree1)
dev.off()

## ------------------------------------------------------------------------
## pruning the tree
## ------------------------------------------------------------------------

## show cross-validation error:
head(dat.train)
printcp(fit.tree1)

## plot cross-validation error:
plotcp(fit.tree1)

## prune the tree:
## option 1: to the smallest cross-validation error
## option 2: the one the most to the left of the minimum one, that still lies within the error bars: does as good as the best one, statistically
fit.tree1.prune <- prune(fit.tree1, cp = 0.028)  ## slighly bigger than the smallest value

## smallest cross-validation error:
fit.tree1$cptable[which.min(fit.tree1$cptable[,"xerror"]),"CP"]

## pruned tree:
print(fit.tree1.prune)
printcp(fit.tree1.prune)

## plot the tree:
fancyRpartPlot(fit.tree1.prune)

pdf(paste0(path.tmp,"rpart.pdf"))
fancyRpartPlot(fit.tree1.prune)
dev.off()


## ------------------------------------------------------------------------
## multiclass classification
## ------------------------------------------------------------------------

fit.tree4m <- rpart(marital_status ~ income + age + years_with_bank + nbr_children + gender +
                    ccacct + ckacct + svacct,
                    data=dat.train,
                    method="class",            ## classification tree (=default; "anova" for reg.tree)
                    parms=list(
                      split="information"), ## split on information gain (default: gini)
                    control=rpart.control(
                      minsplit=60,       ## minimum n to attempt split (=minbucket*3)
                      minbucket=20,      ## minimum n in leaf node (=minsplit/3)
                      cp=0.01,           ## min complexity parameter decrease for split
                      xval=5,            ## number of cross validations to determine error
                      maxdepth=10))      ## maximum depth of the tree

print(fit.tree4m)

## ------------------------------------------------------------------------
## variable importance
## ------------------------------------------------------------------------

fit.tree4m$variable.importance

par(mar=c(6,8,2,5))
barplot(fit.tree4m$variable.importance, horiz=TRUE, las=1, cex.names=.7)
par(mar=c(5, 4, 4, 2) + 0.1)

## ------------------------------------------------------------------------
## plotting the tree
## ------------------------------------------------------------------------

fancyRpartPlot(fit.tree4m)

## ------------------------------------------------------------------------
## prediction
## ------------------------------------------------------------------------

## prediction for ccacct:
## '''''''''''''''''''''''''''''''''''''

class(fit.tree1)
?predict.rpart

head(predict(fit.tree1))
head(predict(fit.tree1, type="class"))

predict(fit.tree1)[,2]  ## prediction for ccacct=1 (2nd column)

predict(fit.tree1,newdata=dat.test)[,2]  ## prediction for ccacct=1 (2nd column)

## prediction for marital_status
## '''''''''''''''''''''''''''''''''''''

head(predict(fit.tree4m))
head(predict(fit.tree4m, type="class"))
predict(fit.tree4m,newdata=dat.test)

## ------------------------------------------------------------------------
## model diagnostics on training data
## ------------------------------------------------------------------------

## for ccacct:
## '''''''''''''''''''''''''''''''''''''

library(caret)
library(ROCR)

dat.this <- dat.train
pred.this <- predict(fit.tree1.prune, newdata=dat.this)[,2]
true.this <- dat.this$ccacct
cutoff <- 0.5

pred.this.num <- as.numeric(pred.this > cutoff)
pred.this.class <- factor(pred.this.num, levels=c(0,1), labels=c("0", "1"))


## confusion matrix and metrics:
confusionMatrix(pred.this.class, reference=true.this, positive="1")

## lift curve:
pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
perf <- performance(prediction=pred.this.rocr, measure="lift", x.measure="rpp")
plot(perf)

## ROC (receiver operating curve):
pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
perf <- performance(prediction=pred.this.rocr, measure="tpr", x.measure="fpr")
plot(perf)
abline(a=0,b=1, lty=2)

## area under the curve (ROC):
perf <- performance(prediction=pred.this.rocr, measure="auc")
perf@y.values[[1]]
text(0.8, 0.2, paste("AUC = ", round(100*perf@y.values[[1]], 2), "%", sep=""),col=2)

## sensitivity (=recall) by cutoff:
par(oma=c(0, 0, 0, 0), mar=c(6, 5, 2, 5))
pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
perf <- performance(prediction=pred.this.rocr, measure="sens")
plot(perf, xlim=c(0,1), ylim=c(0,1))

## specificity by cutoff:
pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
perf <- performance(prediction=pred.this.rocr, measure="spec")
plot(perf, add=TRUE, col="red"); axis(side=4); mtext(text="Specificity", side=4, line=3, col="red")#, srt=90)
plot(perf)

## precision by cutoff:
pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
perf <- performance(prediction=pred.this.rocr, measure="prec")
plot(perf)
par(mar=c(5, 4, 4, 2) + 0.1)

## Recall-Precision curve
#pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
#perf <- performance(pred.this.rocr, "prec", "rec");
#plot(perf)


## ------------------------------------------------------------------------
## model diagnostics on test data
## ------------------------------------------------------------------------

dat.this <- dat.test
pred.this <- predict(fit.tree1.prune, newdata=dat.this)[,2]
true.this <- dat.this$ccacct
cutoff <- 0.5

pred.this.num <- as.numeric(pred.this > cutoff)
pred.this.class <- factor(pred.this.num, levels=c(0,1), labels=c("0", "1"))

## confusion matrix and metrics:
confusionMatrix(pred.this.class, reference=true.this, positive="1")

## lift curve:
pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
perf <- performance(prediction=pred.this.rocr, measure="lift", x.measure="rpp")
plot(perf)

## ROC (receiver operating curve):
pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
perf <- performance(prediction=pred.this.rocr, measure="tpr", x.measure="fpr")
plot(perf)
abline(a=0,b=1, lty=2)

## area under the curve (ROC):
perf <- performance(prediction=pred.this.rocr, measure="auc")
perf@y.values

## sensitivity (=recall) by cutoff:
par(oma=c(0, 0, 0, 0), mar=c(6, 5, 2, 5))
pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
perf <- performance(prediction=pred.this.rocr, measure="sens")
plot(perf)

## specificity by cutoff:
pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
perf <- performance(prediction=pred.this.rocr, measure="spec")
plot(perf, add=TRUE, col="red"); axis(side=4); mtext(text="Specificity", side=4, line=3, col="red")#, srt=90)
plot(perf)

## precision by cutoff:
pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
perf <- performance(prediction=pred.this.rocr, measure="prec")
plot(perf)

## Recall-Precision curve
#pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
#perf <- performance(pred.this.rocr, "prec", "rec");
#plot(perf)

## ------------------------------------------------------------------------
## model diagnostics for multiclass prediciton
## ------------------------------------------------------------------------

dat.this <- dat.test
pred.this.class <- predict(fit.tree4m, newdata=dat.this, type="class")
true.this <- dat.this$marital_status

## confusion matrix and metrics:
confusionMatrix(pred.this.class, reference=true.this)

## classAgreement from e1071:
library(e1071)
confmat <- table("predicted"=pred.this.class,
                 "true.values"=true.this)
confmat
classAgreement(confmat)


## ============================================================================
## with library party
## ============================================================================

## nonparametric regression trees for nominal, ordinal, numeric,
## censored, and multivariate responses

library(party)

## ------------------------------------------------------------------------
## basic call
## ------------------------------------------------------------------------

fit.ctree0 <- ctree(ccacct ~ income + age + years_with_bank + nbr_children + gender +
                      marital_status + ckacct + svacct,
                    data=dat.train)

print(fit.ctree0)
#summary(fit.ctree0)

## plotting:

plot(fit.ctree0)
plot(fit.ctree0, type="simple")

## ------------------------------------------------------------------------
## more parameters
## ------------------------------------------------------------------------

fit.ctree1 <- ctree(ccacct ~ income + age + years_with_bank + nbr_children + gender +
                      marital_status + ckacct + svacct + avg_sv_bal + avg_ck_bal +
                      avg_sv_tran_amt + avg_ck_tran_amt +
                      avg_sv_tran_cnt + avg_ck_tran_cnt,
                    controls=ctree_control(
                      teststat="quad",   ## "quad" or "max"; quad is faster for factors (default)
                      testtype="Bonferroni", ## how the hypothesis for the split is tested
                      mincriterion=.95,   ## min (1-p) value to perform split (here: p<0.05)
                      minsplit=60,        ## minimum n to attempt split
                      minbucket=20,       ## minimum n in leaf node
                      maxdepth=10),       ## max depth of trees
                    data=dat.train)

print(fit.ctree1)


## ------------------------------------------------------------------------
## plotting the tree
## ------------------------------------------------------------------------

plot(fit.ctree1)
plot(fit.ctree1, type="simple")

## ------------------------------------------------------------------------
## multiclass classification
## ------------------------------------------------------------------------

fit.ctree4m <- ctree(marital_status ~ income + age + years_with_bank + nbr_children + gender +
                       ccacct + ckacct + svacct,
                     controls=ctree_control(
                       teststat="quad",   ## "quad" or "max"; quad is faster for factors (default)
                       testtype="Bonferroni", ## how the hypothesis for the split is tested
                       mincriterion=.95,   ## min (1-p) value to perform split (here: p<0.05)
                       minsplit=60,        ## minimum n to attempt split
                       minbucket=20,       ## minimum n in leaf node
                       maxdepth=10),       ## max depth of trees
                     data=dat.train)

print(fit.ctree4m)

## ------------------------------------------------------------------------
## plotting the tree
## ------------------------------------------------------------------------

plot(fit.ctree4m)
plot(fit.ctree4m, type="simple")

## ------------------------------------------------------------------------
## prediction
## ------------------------------------------------------------------------

## prediction for ccacct:
## '''''''''''''''''''''''''''''''''''''

class(fit.ctree1)
#?predict.BinaryTree ## (no help here; need to google)
## help: see http://www.inside-r.org/packages/cran/party/docs/ctree

## prediction of classes and probabilities (comes as a list):
predict(fit.ctree1)
x <- predict(fit.ctree1, type="prob")

## prediction probabilities come as list:
head(x)
class(x)
str(x)

## needs some work before they can be used:
sapply(x, function(i) i[[2]])  ## get second element of each list element

## prediction for new data (test data):
predict(fit.ctree1, newdata=dat.test)
predict(fit.ctree1, newdata=dat.test, type="prob")


## ------------------------------------------------------------------------
## model diagnostics on training data (just confusion matrix)
## ------------------------------------------------------------------------

## for ccacct:
## '''''''''''''''''''''''''''''''''''''

dat.this <- dat.train
pred.raw <- predict(fit.ctree1, type="prob", newdata=dat.this)
pred.this <- sapply(pred.raw, function(i) i[[2]])  ## second entry of each list
true.this <- dat.this$ccacct
cutoff <- 0.5

pred.this.num <- as.numeric(pred.this > cutoff)
pred.this.class <- factor(pred.this.num, levels=c(0,1), labels=c("0", "1"))

## confusion matrix and metrics:
confusionMatrix(pred.this.class, reference=true.this, positive="1")

## ------------------------------------------------------------------------
## model diagnostics on test data (just confusion matrix)
## ------------------------------------------------------------------------

dat.this <- dat.test
pred.raw <- predict(fit.ctree1, type="prob", newdata=dat.this)
pred.this <- sapply(pred.raw, function(i) i[[2]])  ## second entry of each list
true.this <- dat.this$ccacct
cutoff <- 0.5

pred.this.num <- as.numeric(pred.this > cutoff)
pred.this.class <- factor(pred.this.num, levels=c(0,1), labels=c("0", "1"))

## confusion matrix and metrics:
confusionMatrix(pred.this.class, reference=true.this, positive="1")

## ============================================================================
## Exercise DECISION TREE
## ============================================================================

## - Using a decision tree, try to predict whether a subject has a savings account (svacct)
## - on a training data set
## - predict values for a testing data set
## - build the confusion matrix
## - and inspect model quality

## ============================================================================
## solution to exercise DECISION TREE
## ============================================================================



## ############################################################################
## ############################################################################
##
## Random Forests
##
## ############################################################################
## ############################################################################

## ============================================================================
## Random Forests
## ============================================================================

library(randomForest)

## ------------------------------------------------------------------------
## basic call
## ------------------------------------------------------------------------

fit.rf0 <- randomForest(ccacct ~ income + age + years_with_bank + nbr_children + gender +
                          marital_status + ckacct + svacct,
                        data=dat.train)

## ------------------------------------------------------------------------
## more parameters
## ------------------------------------------------------------------------

fit.rf1 <- randomForest(ccacct ~ income + age + years_with_bank + nbr_children + gender +
                          marital_status + ckacct + svacct + avg_sv_bal + avg_ck_bal +
                          avg_sv_tran_amt + avg_ck_tran_amt +
                          avg_sv_tran_cnt + avg_ck_tran_cnt,
                        ntree=1000,                 ## number of trees to grow
                        sampsize=nrow(dat.train),   ## sample size for bootstrap samples
                        nodesize=20,                ## min size for leaf node
                        maxnodes=10,                ## max number of nodes for each tree
                        importance=TRUE,            ## should importance of predictors be assessed?
                        data=dat.train)
fit.rf1

## inspect number of trees:
plot(fit.rf1)

## ------------------------------------------------------------------------
## variable importance
## ------------------------------------------------------------------------

fit.rf1$importance
## mean decrease in accuracy for each class, all classes combined, mean gini decrease

## sort: with order() function:
var.imp <- fit.rf1$importance[ order(fit.rf1$importance[,"MeanDecreaseGini"]), c("MeanDecreaseAccuracy", "MeanDecreaseGini") ]
var.imp

## plot importance by MeanDecreaseGini:
par(mar=c(6,8,2,5))
barplot(var.imp[,2], horiz=TRUE, las=1, cex.names=.7)

## plot importance by MeanDecreaseAccuracy:
par(mar=c(6,8,2,5))
barplot(var.imp[,1], horiz=TRUE, las=1, cex.names=.7)

## ------------------------------------------------------------------------
## marginal effects plots
## ------------------------------------------------------------------------

## marginal effect of a variable on the class probability:
partialPlot(fit.rf1, pred.data=dat.train, x.var="avg_sv_bal", which.class="1")
abline(h=0, lty=2)

partialPlot(fit.rf1, pred.data=dat.train, x.var="income", which.class="1")
abline(h=0, lty=2); abline(v=2708, lty=2)

partialPlot(fit.rf1, pred.data=dat.train, x.var="age", which.class="1")
abline(h=0, lty=2)

## for categorical variables:
partialPlot(fit.rf1, pred.data=dat.train, x.var="ckacct", which.class="1")
partialPlot(fit.rf1, pred.data=dat.train, x.var="gender", which.class="1")

## ------------------------------------------------------------------------
## most important predictors and their means, medians
## ------------------------------------------------------------------------

## select the 14 most important predictors:
var.imp.sel <- var.imp[1:14,]
var.wch <- dimnames(var.imp.sel)[[1]]  ## get their names

library(plyr)
var.mean <- ddply(dat.train[c(var.wch, "ccacct")], ~ccacct, numcolwise(mean), na.rm=TRUE)
var.med <- ddply(dat.train[c(var.wch, "ccacct")], ~ccacct, numcolwise(median), na.rm=TRUE)

#dat.desc.split <- split(dat.train[,var.wch], dat.train$ccacct)
#var.mean.sel <- sapply(dat.desc.split, function(i) numcolwise(mean)(i, na.rm=TRUE))
#var.med.sel <- sapply(dat.desc.split, function(i) numcolwise(median)(i , na.rm=TRUE))

options(width=120)
rbind(var.mean, var.med)
options(width=100)

par(oma=c(5,0,0,0))
barplot(as.matrix(var.mean[-1]), beside=TRUE, las=2)

par(oma=c(5,0,0,0))
barplot(as.matrix(var.med[-1]), beside=TRUE, las=2)#, ylim=c(0,200))

## ------------------------------------------------------------------------
## multiclass classification
## ------------------------------------------------------------------------

fit.rf4m <- randomForest(marital_status ~ income + age + years_with_bank +
                           nbr_children + gender + ckacct + svacct + ccacct,
                         ntree=1000,                 ## number of trees to grow
                         sampsize=nrow(dat.train),   ## sample size for bootstrap samples
                         nodesize=20,                ## min size for leaf node
                         maxnodes=10,                ## max number of nodes for each tree
                         importance=TRUE,            ## should importance of predictors be assessed?
                         data=dat.train)

fit.rf4m

## ------------------------------------------------------------------------
## variable importance
## ------------------------------------------------------------------------

fit.rf4m$importance

## sort: with order() function:
var.imp <- fit.rf4m$importance[ order(fit.rf4m$importance[,"MeanDecreaseGini"]), c("MeanDecreaseAccuracy", "MeanDecreaseGini") ]
var.imp

## plot importance by MeanDecreaseGini:
par(mar=c(6,8,2,5))
barplot(var.imp[,2], horiz=TRUE, las=1, cex.names=.7)

## ------------------------------------------------------------------------
## prediction
## ------------------------------------------------------------------------

## prediction for ccacct:
## '''''''''''''''''''''''''''''''''''''

methods("predict")
?predict.randomForest

## predictions of training data:
predict(fit.rf1)
head(predict(fit.rf1, type="prob"))

## predictions of new data (test data):
predict(fit.rf1, newdata=dat.test)
head(predict(fit.rf1, newdata=dat.test, type="prob"))

## get probabilities for outcome of interest:
predict(fit.rf1, newdata=dat.test, type="prob")[,2]

## ------------------------------------------------------------------------
## model diagnostics on training data
## ------------------------------------------------------------------------

dat.this <- dat.train
pred.this <- predict(fit.rf1, newdata=dat.this, type="prob")[,2]
true.this <- dat.this$ccacct
cutoff <- 0.5

pred.this.num <- as.numeric(pred.this > cutoff)
pred.this.class <- factor(pred.this.num, levels=c(0,1), labels=c("0", "1"))

## confusion matrix and metrics:
confusionMatrix(pred.this.class, reference=true.this, positive="1")

## lift curve:
pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
perf <- performance(prediction=pred.this.rocr, measure="lift", x.measure="rpp")
plot(perf)

## ROC (receiver operating curve):
pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
perf <- performance(prediction=pred.this.rocr, measure="tpr", x.measure="fpr")
plot(perf)
abline(a=0,b=1, lty=2)

## area under the curve (ROC):
perf <- performance(prediction=pred.this.rocr, measure="auc")
perf@y.values

## sensitivity (=recall) by cutoff:
par(oma=c(0, 0, 0, 0), mar=c(6, 5, 2, 5))
pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
perf <- performance(prediction=pred.this.rocr, measure="sens")
plot(perf)

## specificity by cutoff:
pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
perf <- performance(prediction=pred.this.rocr, measure="spec")
plot(perf, add=TRUE, col="red"); axis(side=4); mtext(text="Specificity", side=4, line=3, col="red")#, srt=90)
plot(perf)

## precision by cutoff:
pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
perf <- performance(prediction=pred.this.rocr, measure="prec")
plot(perf)

## Recall-Precision curve
#pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
#perf <- performance(pred.this.rocr, "prec", "rec");
#plot(perf)


## ------------------------------------------------------------------------
## model diagnostics on test data
## ------------------------------------------------------------------------

dat.this <- dat.test
pred.this <- predict(fit.rf1, newdata=dat.this, type="prob")[,2]
true.this <- dat.this$ccacct
cutoff <- 0.5

pred.this.num <- as.numeric(pred.this > cutoff)
pred.this.class <- factor(pred.this.num, levels=c(0,1), labels=c("0", "1"))

## confusion matrix and metrics:
confusionMatrix(pred.this.class, reference=true.this, positive="1")


## lift curve:
pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
perf <- performance(prediction=pred.this.rocr, measure="lift", x.measure="rpp")
plot(perf)

## ROC (receiver operating curve):
pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
perf <- performance(prediction=pred.this.rocr, measure="tpr", x.measure="fpr")
plot(perf)
abline(a=0,b=1, lty=2)

## area under the curve (ROC):
perf <- performance(prediction=pred.this.rocr, measure="auc")
perf@y.values

## sensitivity (=recall) by cutoff:
par(oma=c(0, 0, 0, 0), mar=c(6, 5, 2, 5))
pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
perf <- performance(prediction=pred.this.rocr, measure="sens")
plot(perf)

## specificity by cutoff:
pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
perf <- performance(prediction=pred.this.rocr, measure="spec")
plot(perf, add=TRUE, col="red"); axis(side=4); mtext(text="Specificity", side=4, line=3, col="red")#, srt=90)
plot(perf)

## precision by cutoff:
pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
perf <- performance(prediction=pred.this.rocr, measure="prec")
plot(perf)

## Recall-Precision curve
#pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
#perf <- performance(pred.this.rocr, "prec", "rec");
#plot(perf)


## ============================================================================
## Exercise RANDOM FOREST
## ============================================================================

## - Using a random forest, try to predict whether a subject has a savings account (svacct)
## - on a training data set
## - predict values for a testing data set
## - build the confusion matrix
## - and inspect model quality

## ============================================================================
## solution to exercise RANDOM FOREST
## ============================================================================



## ############################################################################
## ############################################################################
##
## CART using the caret package
##
## ############################################################################
## ############################################################################

## use 5-fold cross-validation with 2 repeats this time:
tc <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 2,  		## repeated 3 times
  classProbs=FALSE)

## classProbs has problems with factor level names that are
## not valid R variable names (like "0")

## ============================================================================
## Decision Trees with caret
## ============================================================================

set.seed(42)
fit.rpart.caret <- train(ccacct ~ income + age + years_with_bank + nbr_children + gender +
                           marital_status + ckacct + svacct + avg_sv_bal + avg_ck_bal +
                           avg_sv_tran_amt + avg_ck_tran_amt +
                           avg_sv_tran_cnt + avg_ck_tran_cnt,
                         data=dat.train,
                         trControl=tc,
                         method="rpart")

fit.rpart.caret

pred.this.class <- predict(fit.rpart.caret, newdata=dat.test)
pred.this.class

confusionMatrix(pred.this.class, reference=dat.test$ccacct, positive="1")

## ============================================================================
## Random Forest with caret
## ============================================================================

set.seed(42)
fit.rf.caret <- train(ccacct ~ income + age + years_with_bank + nbr_children + gender +
                        marital_status + ckacct + svacct + avg_sv_bal + avg_ck_bal +
                        avg_sv_tran_amt + avg_ck_tran_amt +
                        avg_sv_tran_cnt + avg_ck_tran_cnt,
                      data=dat.train,
                      trControl=tc,
                      method="rf")

fit.rf.caret

pred.this.class <- predict(fit.rf.caret, newdata=dat.test)
pred.this.class
#predict(fit.rf.caret, newdata=dat.test, type="prob")

confusionMatrix(pred.this.class, reference=dat.test$ccacct, positive="1")