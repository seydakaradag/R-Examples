## ############################################################################
## ############################################################################
## Naive Bayes Classification
## August 22, 2016
## ############################################################################
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
## load packages
## ============================================================================

library(gdata)   ## trim()


## ============================================================================
## data (from file)
## ============================================================================

dat.train <- read.csv(file=paste0(path.dat, "twm_customer_analysis_train.csv"))
dat.test <- read.csv(file=paste0(path.dat, "twm_customer_analysis_test.csv"))

## data preparation:
## ''''''''''''''''''''''''''''''''''''

## convert names to lower case:
names(dat.train) <- tolower(names(dat.train))
names(dat.test) <- tolower(names(dat.test))

## for training set:
levels(dat.train$gender) <- trim(levels(dat.train$gender))
levels(dat.train$state_code) <- trim(levels(dat.train$state_code))

dat.train$marital_status <- factor(dat.train$marital_status)
levels(dat.train$marital_status) <- list(
		"single"=1, "married"=2, "separated"=3, "other"=4)

dat.train$female <- factor(dat.train$female)
dat.train$single <- factor(dat.train$single)
dat.train$married <- factor(dat.train$married)
dat.train$separated <- factor(dat.train$separated)
dat.train$ccacct <- factor(dat.train$ccacct)
dat.train$ckacct <- factor(dat.train$ckacct)
dat.train$svacct <- factor(dat.train$svacct)

## for testing set:
levels(dat.test$gender) <- trim(levels(dat.test$gender))
levels(dat.test$state_code) <- trim(levels(dat.test$state_code))

dat.test$marital_status <- factor(dat.test$marital_status)
levels(dat.test$marital_status) <- list(
		"single"=1, "married"=2, "separated"=3, "other"=4)

dat.test$female <- factor(dat.test$female)
dat.test$single <- factor(dat.test$single)
dat.test$married <- factor(dat.test$married)
dat.test$separated <- factor(dat.test$separated)
dat.test$ccacct <- factor(dat.test$ccacct)
dat.test$ckacct <- factor(dat.test$ckacct)
dat.test$svacct <- factor(dat.test$svacct)


## ############################################################################
## ############################################################################
##
## Naive Bayes Classification
##
## ############################################################################
## ############################################################################

## ============================================================================
## Training the Naive Bayes Classifier (e1017)
## ============================================================================

library(e1071) ## naiveBayes()

## train model:
fit.nb1 <- naiveBayes(ccacct ~ income + age + years_with_bank + nbr_children +
				gender + marital_status + avg_ck_bal + avg_sv_bal,
		data=dat.train)

## inspect results:
fit.nb1 # mean and sd for numeric variables, conditional probabilities given the class for categorical variables

## apriori distribution:
fit.nb1$apriori
prop.table(fit.nb1$apriori)

## posterior distributions for discrete predictors:
fit.nb1$tables$gender
fit.nb1$tables$marital_status

## mean and standard deviation for metric predictors:
fit.nb1$tables$avg_sv_bal

## ------------------------------------------------------------------------
## Predicting
## ------------------------------------------------------------------------

## predicting on training data:
predict(fit.nb1, newdata=dat.train)

## predicting on test data:
predict(fit.nb1, newdata=dat.test)

#methods(predict)
#?predict.naiveBayes

predict(fit.nb1, newdata=dat.test, type="raw")

## ------------------------------------------------------------------------
## model diagnostics on training data
## ------------------------------------------------------------------------

library(caret)
library(ROCR)

dat.this <- dat.train
pred.this <- predict(fit.nb1, newdata=dat.this, type="raw")[,2]
cutoff <- .5

pred.this.num <- as.numeric(pred.this > cutoff)
pred.this.class <- factor(pred.this.num, levels=c(0,1), labels=c("0", "1"))

## confusion matrix and metrics:
confusionMatrix(pred.this.class, reference=dat.this$ccacct, positive="1")

## lift curve (differently)
pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
perf <- performance(prediction=pred.this.rocr, measure="lift", x.measure="rpp")
plot(perf)

## ROC (receiver operating curve):
pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
perf <- performance(prediction=pred.this.rocr, measure="tpr", x.measure="fpr")
plot(perf)
abline(a=0,b=1, lty=2)

## area under the curve (ROC):
perf <- performance(prediction=pred.this.rocr, measure="auc")
perf@y.values

## sensitivity (=recall) by cutoff:
pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
perf <- performance(prediction=pred.this.rocr, measure="sens")
plot(perf, ylim=c(0,1))

## specificity by cutoff:
pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
perf <- performance(prediction=pred.this.rocr, measure="spec")
plot(perf, add=TRUE, col="red")
plot(perf)

## precision by cutoff:
pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
perf <- performance(prediction=pred.this.rocr, measure="prec")
plot(perf)

## Recall-Precision curve
#pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
#perf <- performance(pred.this.rocr, "prec", "rec");
#plot(perf)


## ============================================================================
## Training the Naive Bayes Classifier (klaR)
## ============================================================================

library(klaR) ## NaiveBayes()

fit.nb2 <- NaiveBayes(ccacct ~ income + age + years_with_bank + nbr_children +
				gender + marital_status + avg_ck_bal + avg_sv_bal,
		data=dat.train)
str(fit.nb2)

## more using estimated densities (kernels):

fit.nb2 <- NaiveBayes(ccacct ~ income + age + years_with_bank + nbr_children +
				gender + marital_status + avg_ck_bal + avg_sv_bal,
		usekernel=TRUE,
		data=dat.train)

## inspect results:
str(fit.nb2)

## apriori distribution:
fit.nb2$apriori

## posterior distributions for discrete predictors:
fit.nb2$tables$gender
fit.nb2$tables$marital_status

## mean and standard deviation for metric predictors,
## or distribution information when using estimated densities (kernels):
fit.nb2$tables$avg_sv_bal

## ------------------------------------------------------------------------
## Predicting
## ------------------------------------------------------------------------

#class(fit.nb2)
#?predict.NaiveBayes

## predict with training data:

fit.nb2.pred.train <- predict(fit.nb2)
str(fit.nb2.pred.train)

fit.nb2.pred.train$class
head(fit.nb2.pred.train$posterior)

## probabilities for ccacct=1 (2nd column):
fit.nb2.pred.train$posterior[,2]

## predict with testing data:
fit.nb2.pred.test <- predict(fit.nb2, newdata=dat.test)
str(fit.nb2.pred.test)

## probabilities for ccacct=1 (2nd column):
fit.nb2.pred.test$posterior[,2]

## [[?]] warnings:
## Numerical 0 probability for all classes with observation 50
# apply(fit.nb2.pred.train$posterior, 1, sum)

## ------------------------------------------------------------------------
## model diagnostics on training data
## ------------------------------------------------------------------------

dat.this <- dat.train
pred.this <- predict(fit.nb2, newdata=dat.this)$posterior[,2]
cutoff <- .5

pred.this.num <- as.numeric(pred.this > cutoff)
pred.this.class <- factor(pred.this.num, levels=c(0,1), labels=c("0", "1"))

## confusion matrix and metrics:
confusionMatrix(pred.this.class, reference=dat.this$ccacct, positive="1")


## lift curve (differently)
pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
perf <- performance(prediction=pred.this.rocr, measure="lift", x.measure="rpp")
plot(perf)

## ROC (receiver operating curve):
pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
perf <- performance(prediction=pred.this.rocr, measure="tpr", x.measure="fpr")
plot(perf)
abline(a=0,b=1, lty=2)

## area under the curve (ROC):
perf <- performance(prediction=pred.this.rocr, measure="auc")
perf@y.values

## sensitivity (=recall) by cutoff:
pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
perf <- performance(prediction=pred.this.rocr, measure="sens")
plot(perf, ylim=c(0,1))

## specificity by cutoff:
pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
perf <- performance(prediction=pred.this.rocr, measure="spec")
plot(perf, add=TRUE, col="red")
plot(perf)

## precision by cutoff:
pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
perf <- performance(prediction=pred.this.rocr, measure="prec")
plot(perf)

## Recall-Precision curve
#pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
#perf <- performance(pred.this.rocr, "prec", "rec");
#plot(perf)


## ------------------------------------------------------------------------
## model diagnostics on test data
## ------------------------------------------------------------------------

dat.this <- dat.test
pred.this <- predict(fit.nb2, newdata=dat.this)$posterior[,2]
cutoff <- .5

pred.this.num <- as.numeric(pred.this > cutoff)
pred.this.class <- factor(pred.this.num, levels=c(0,1), labels=c("0", "1"))

## confusion matrix and metrics:
confusionMatrix(pred.this.class, reference=dat.this$ccacct, positive="1")


## lift curve (differently)
pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
perf <- performance(prediction=pred.this.rocr, measure="lift", x.measure="rpp")
plot(perf)

## ROC (receiver operating curve):
pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
perf <- performance(prediction=pred.this.rocr, measure="tpr", x.measure="fpr")
plot(perf)
abline(a=0,b=1, lty=2)

## area under the curve (ROC):
perf <- performance(prediction=pred.this.rocr, measure="auc")
perf@y.values

## sensitivity (=recall) by cutoff:
pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
perf <- performance(prediction=pred.this.rocr, measure="sens")
plot(perf, ylim=c(0,1))

## specificity by cutoff:
pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
perf <- performance(prediction=pred.this.rocr, measure="spec")
plot(perf, add=TRUE, col="red")
plot(perf)

## precision by cutoff:
pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
perf <- performance(prediction=pred.this.rocr, measure="prec")
plot(perf)

## Recall-Precision curve
#pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
#perf <- performance(pred.this.rocr, "prec", "rec");
#plot(perf)

## ============================================================================
## exercise NAIVE BAYES
## ============================================================================

## Use a Naive Bayes model to predict
## * whether somebody has a savings account (svacct)
## * how sensitive is the model?
## * how specific is the model?
## * draw the gains curve for the model.

## ============================================================================
## solution to exercise NAIVE BAYES
## ============================================================================

library(klaR) ## NaiveBayes()

## fit naive bayes model with empirical (estimated) densities:
fit.nbex <- NaiveBayes(svacct ~ income + age + years_with_bank + nbr_children +
				gender + marital_status + avg_ck_bal + avg_sv_bal + ccacct + ckacct,
		usekernel=TRUE,
		data=dat.train)

## inspect model:
str(fit.nbex)

## apriori distribution:
fit.nbex$apriori

## posterior distributions for discrete predictors:
fit.nbex$tables$gender
fit.nbex$tables$marital_status

## mean and standard deviation for metric predictors:
fit.nbex$tables$avg_sv_bal

## ------------------------------------------------------------------------
## prediction
## ------------------------------------------------------------------------

predict(fit.nbex, newdata=dat.test)

## ------------------------------------------------------------------------
## model diagnostics on training data
## ------------------------------------------------------------------------

dat.this <- dat.train
pred.this <- predict(fit.nbex, newdata=dat.this)$posterior[,2]
cutoff <- .5

pred.this.num <- as.numeric(pred.this > cutoff)
pred.this.class <- factor(pred.this.num, levels=c(0,1), labels=c("0", "1"))

## confusion matrix and metrics:
confusionMatrix(pred.this.class, reference=dat.this$ccacct, positive="1")

## lift curve (differently)
pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
perf <- performance(prediction=pred.this.rocr, measure="lift", x.measure="rpp")
plot(perf)

## ROC (receiver operating curve):
pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
perf <- performance(prediction=pred.this.rocr, measure="tpr", x.measure="fpr")
plot(perf)
abline(a=0,b=1, lty=2)

## area under the curve (ROC):
perf <- performance(prediction=pred.this.rocr, measure="auc")
perf@y.values

## sensitivity (=recall) by cutoff:
pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
perf <- performance(prediction=pred.this.rocr, measure="sens")
plot(perf, ylim=c(0,1))

## specificity by cutoff:
pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
perf <- performance(prediction=pred.this.rocr, measure="spec")
plot(perf, add=TRUE, col="red")
plot(perf)

## precision by cutoff:
pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
perf <- performance(prediction=pred.this.rocr, measure="prec")
plot(perf)

## Recall-Precision curve
#pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
#perf <- performance(pred.this.rocr, "prec", "rec");
#plot(perf)


## ------------------------------------------------------------------------
## model diagnostics on test data
## ------------------------------------------------------------------------

dat.this <- dat.test
pred.this <- predict(fit.nbex, newdata=dat.this)$posterior[,2]
cutoff <- .5

pred.this.num <- as.numeric(pred.this > cutoff)
pred.this.class <- factor(pred.this.num, levels=c(0,1), labels=c("0", "1"))

## confusion matrix and metrics:
confusionMatrix(pred.this.class, reference=dat.this$ccacct, positive="1")

## lift curve (differently)
pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
perf <- performance(prediction=pred.this.rocr, measure="lift", x.measure="rpp")
plot(perf)

## ROC (receiver operating curve):
pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
perf <- performance(prediction=pred.this.rocr, measure="tpr", x.measure="fpr")
plot(perf)
abline(a=0,b=1, lty=2)

## area under the curve (ROC):
perf <- performance(prediction=pred.this.rocr, measure="auc")
perf@y.values

## sensitivity (=recall) by cutoff:
pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
perf <- performance(prediction=pred.this.rocr, measure="sens")
plot(perf, ylim=c(0,1))

## specificity by cutoff:
pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
perf <- performance(prediction=pred.this.rocr, measure="spec")
plot(perf, add=TRUE, col="red")
plot(perf)

## precision by cutoff:
pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
perf <- performance(prediction=pred.this.rocr, measure="prec")
plot(perf)

## Recall-Precision curve
#pred.this.rocr <- prediction(predictions=pred.this, labels=dat.this$ccacct)
#perf <- performance(pred.this.rocr, "prec", "rec");
#plot(perf)

## [[here]]

## ############################################################################
## ############################################################################
##
## Naive Bayes using the caret package
##
## ############################################################################
## ############################################################################

## use 5-fold cross-validationthis time:
tc <- trainControl(
		method = "cv",
		number = 5,
		classProbs=FALSE)

## classProbs has problems with factor level names that are
## not valid R variable names (like "0")

set.seed(42)
fit.rpart.caret <- train(ccacct ~ income + age + years_with_bank + nbr_children +
				gender + marital_status + ckacct + svacct,
		data=dat.train,
		trControl=tc,
		method="nb")

fit.rpart.caret

predict(fit.rpart.caret, newdata=dat.test)
#predict(fit.rpart.caret, newdata=dat.test, type="prob")

## [[to do]] caret

## ////////////////////////////////////////////////////////////////////////////

xdat <- data.frame(
		"outlook"=c("rain", "rain", "sunny", "sunny", "sunny"),
		"wind"=c("wind", "windless", "wind", "windless", "wind"),
		"cloud"=c("cloud", "cloud", "clear", "clear", "cloud"))

xdat