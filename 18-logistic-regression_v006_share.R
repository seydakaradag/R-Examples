## ############################################################################
## R Introduction and Best Practice
## Logistic Regression
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
## libraries
## ============================================================================

library(gdata)
library(MASS)

## ############################################################################
## ############################################################################
##
## Logistic Regression (GLM)
##
## ############################################################################
## ############################################################################


## ============================================================================
## data (from file)
## ============================================================================

load(file=paste0(path.dat, "twm_customer_analysis_train.Rdata")) ## contains dat.train
load(file=paste0(path.dat, "twm_customer_analysis_test.Rdata"))  ## contains dat.test

## ============================================================================
## logistic regression
## ============================================================================

(varnames_all <- names(dat.train))
varnames_all
(varnames_model <- c("ccacct", "income", "age", "years_with_bank", "nbr_children", "gender",
                     "marital_status", "ckacct", "svacct", "avg_sv_bal", "avg_ck_bal",
                     "avg_sv_tran_amt", "avg_ck_tran_amt",
                     "avg_sv_tran_cnt", "avg_ck_tran_cnt"))

## ------------------------------------------------------------------------
## predict who has credit card account based on master data
## ------------------------------------------------------------------------

str(dat.train[,varnames_model])

fit.glm0 <- glm(ccacct ~ .,
		data = dat.train[,varnames_model],
		family=binomial(link="logit"))

## warning message:
## fitted probabilities numerically 0 or 1 occurred
## means that there is perfect separation (model too complex for sample size: one or more variables perfectly separate the 0's and 1's)
## coefficients inflated, standard errors are wrong
## nice posting: http://stats.stackexchange.com/questions/11109/how-to-deal-with-perfect-separation-in-logistic-regression

## simpler model:
varnames_model_simp <- setdiff(varnames_model, "avg_sv_bal")
fit.glm0 <- glm(ccacct ~ .,
		data = dat.train[, varnames_model_simp],
		family=binomial(link="logit"))

## summary of model:
summary(fit.glm0)
AIC(fit.glm0)
BIC(fit.glm0)

## ------------------------------------------------------------------------
## coefficients and confidence intervals
## ------------------------------------------------------------------------

## extract coefficients and confidence intervals:
round(summary(fit.glm0)$coef,3)   ## for every year_with_bank (n.s.), log odds decrease by 0.045
round(confint(fit.glm0), 3)

## odds ratio:
exp(coef(fit.glm0))   ## for every_year_with_bank (n.s.), odds for having a CC decrease by 4.5%

## ------------------------------------------------------------------------
## Pseudo R^2 values
## ------------------------------------------------------------------------

class(fit.glm0)

## various pseudo R-squared measures:
library(pscl)
pR2(fit.glm0)            ## pseudo R^2

library(fmsb)
NagelkerkeR2(fit.glm0)   ## Nagelkerke R^2

## ------------------------------------------------------------------------
## model assumptions
## ------------------------------------------------------------------------

## multicollinearity:
library(car)
vif(fit.glm0)
#no value > 10

## deviance residuals
## (contribution of each point to the likelihood of the mode,
## for determining individual points that do not fit well)
plot(residuals(fit.glm0))

## leverage and cook's distance still make sense, too:

## basic leverage plot:
plot(hatvalues(fit.glm0), type="h")
abline(h=2*mean(hatvalues(fit.glm0)), lty=2)
abline(h=3*mean(hatvalues(fit.glm0)), lty=3)

## cook's distance:
plot(fit.glm0, which=4)
plot(fit.glm0, which=5)

## fitted (predicted) probabilities (on training data):
(pred.glm0 <- predict(fit.glm0, type="response"))

## histogram and density of probabilities:
hist(pred.glm0, breaks=20, probability=TRUE)
lines(density(pred.glm0, adjust=1), col=3, lwd=2)

## ------------------------------------------------------------------------
## Hosmer-Lemeshow-Test
## ------------------------------------------------------------------------

library(ResourceSelection)

## needs binary, numeric(!) response (0/1):
hoslem.test(as.numeric(dat.train$ccacct)-1, pred.glm0, g=10)
## less than 0.05

## ------------------------------------------------------------------------
## prediction and scoring
## ------------------------------------------------------------------------

## predictions on new data:
(pred.glm0.test <- predict(fit.glm0, newdata=dat.test, type="response"))

## histogram of probabilities:
hist(pred.glm0.test,breaks=20)

## predicted classes:
(pred.glm0.test.class.raw <- (pred.glm0.test > .5))

## as numeric values:
as.numeric(pred.glm0.test.class.raw)

## as factor with correct levels:
(pred.glm0.class <- factor(as.numeric(pred.glm0.test.class.raw), levels=c(0,1), labels=c("0", "1")))


## ============================================================================
## stepwise logistic regression
## ============================================================================

(scopedef <- list(
  lower = ~ 1,
  upper = as.formula(
    paste("~ ",
      paste(varnames_model_simp[varnames_model_simp!='ccacct'], collapse = " + ")
    , sep = " ")
  )
))

nullmodel <- glm(ccacct ~ 1, data = dat.train,
		family=binomial(link="logit"))

fit.glm1 <- stepAIC(nullmodel, scope=scopedef, direction="both", trace=TRUE)
summary(fit.glm1)
AIC(fit.glm1)
BIC(fit.glm1)

pR2(fit.glm1)            ## pseudo R^2
NagelkerkeR2(fit.glm1)   ## Nagelkerke R^2

## ============================================================================
## model diagnostics on training data (simplified)
## ============================================================================

library(car)

## multicollinearity:
vif(fit.glm1)

### correlations of model matrix:
#cor(model.matrix(fit.glm1)[,-1])

## deviance residuals
## (contribution of each point to the likelihood of the mode,
## for determining individual points that do not fit well)
plot(residuals(fit.glm1))

## cook's distance and leverage:
plot(fit.glm1, which=5)
plot(fit.glm1, which=5, labels.id=dat.train$cust_id)

## standard diagnostic plots, most not relevant:
dev.new()
par(mfrow=c(2,2))
plot(fit.glm1)
par(mfrow=c(1,1))
dev.off()

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## model diagnostics using caret and ROCR packages
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ------------------------------------------------------------------------
## model diagnostics on training data
## ------------------------------------------------------------------------

library(caret)
library(ROCR)

dat.this <- dat.train
pred.this <- predict(fit.glm1, newdata=dat.this, type="response")
true.this <- dat.this$ccacct
cutoff <- 0.5

pred.this.num <- as.numeric(pred.this > cutoff)
(pred.this.class <- factor(pred.this.num, levels=c(0,1), labels=c("0", "1")))

## confusion matrix and metrics:
confusionMatrix(pred.this.class, reference=true.this, positive="1")
## ?confusionMatrix has explanations of each metric
## Wiki article also good:

## gains library, produces nice tables for lift, gains, bad rate, etc.
library(gains)
gains.this <- gains(actual=as.numeric(true.this)-1, predicted=pred.this,
                    groups=100, ties.method="first", optimal=TRUE)
gains.this

## lift curve using gains
dev.new()
plot(y=gains.this$opt.cume.lift/100
     , x = gains.this$depth
     , type = "b", lwd=2, col="green"
     , ylim = c(0,max(gains.this$opt.lift)/100)
     , ylab = "lift"
     , xlab = "cutoff"
     , main = "lift curve for fit.glm1")
lines(y=gains.this$cume.lift/100,x=gains.this$depth, lwd=2,type='b')
lines(y=gains.this$lift/100, x=gains.this$depth, type = "b", lty=2)
abline(h=1, lwd=2, col="red")
legend("bottomleft",legend=c("perfect","model","non-cumulative","random"),lwd=c(2,2,1,2),lty=c(1,1,2,1),col=c(3,1,1,2),cex=0.8)
dev.off()

## lift curve:
pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
perf <- performance(prediction=pred.this.rocr, measure="lift", x.measure="rpp")
abline(h=1, col="red")
plot(perf)
## rpp is just the cutoff for positive prediictions

## ROC (receiver operating curve):
pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
perf <- performance(prediction=pred.this.rocr, measure="tpr", x.measure="fpr")
plot(perf)
abline(a=0,b=1, lty=2)
## tpr also known as sensitivity, fpr known as 1 -specificity

## area under the curve (ROC):
perf <- performance(prediction=pred.this.rocr, measure="auc")
perf@y.values
text(0.8, 0.2, paste("AUC = ", round(100*perf@y.values[[1]], 2), "%", sep=""),col=2)


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
## precision also known as positive predicted value (TP/TP+FP)
## same as lift but with different scale

## Recall-Precision curve
#pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
#perf <- performance(pred.this.rocr, "prec", "rec");
#plot(perf)
## also known as positive predicted value vs. true positive rate
## agreement between model and data

## ------------------------------------------------------------------------
## model diagnostics on test data
## ------------------------------------------------------------------------

library(caret)
library(ROCR)

dat.this <- dat.test
pred.this <- predict(fit.glm1, newdata=dat.this, type="response")
true.this <- dat.this$ccacct
cutoff <- 0.5

pred.this.num <- as.numeric(pred.this > cutoff)
pred.this.class <- factor(pred.this.num, levels=c(0,1), labels=c("0", "1"))

## confusion matrix and metrics:
confusionMatrix(pred.this.class, reference=true.this, positive="1")

## gains library, produces nice tables for lift, gains, bad rate, etc.
library(gains)
gains.this <- gains(actual=as.numeric(true.this)-1, predicted=pred.this,
                    groups=100, ties.method="first", optimal=TRUE)

## lift curve using gains
dev.new()
plot(y=gains.this$opt.cume.lift/100
     , x= gains.this$depth
     , type="b", lwd=2, col="green"
     , ylim=c(0,max(gains.this$opt.lift)/100)
     , ylab = "lift"
     , xlab = "cutoff"
     , main = "lift curve for fit.glm1")
lines(y=gains.this$cume.lift/100,x=gains.this$depth, lwd=2,type='b')
lines(y=gains.this$lift/100, x=gains.this$depth, type = "b", lty=2)
abline(h=1, lwd=2, col="red")
legend("bottomleft",legend=c("perfect","model","non-cumulative","random"),lwd=c(2,2,1,2),lty=c(1,1,2,1),col=c(3,1,1,2),cex=0.8)
dev.off()

## lift curve using prediction / performance
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
pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
perf <- performance(prediction=pred.this.rocr, measure="sens")
plot(perf)

## specificity by cutoff:
pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
perf <- performance(prediction=pred.this.rocr, measure="spec")
plot(perf, add=TRUE, col="red")
plot(perf)

## precision by cutoff:
pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
perf <- performance(prediction=pred.this.rocr, measure="prec")
plot(perf)

# # Recall-Precision curve
# pred.this.rocr <- prediction(predictions=pred.this, labels=true.this)
# perf <- performance(pred.this.rocr, "prec", "rec");
# plot(perf)

## ============================================================================
## Exercise LOGISTIC REGRESSION
## ============================================================================

## - using the training set, build a logistic regression model predicting svacct
## - and inspect the model fit (build the confusion matrix)
## - in the training and in the test set

## ============================================================================
## Solution to exercise LOGISTIC REGRESSION
## ============================================================================

## define scope:
scopedef <- list(
  lower = ~1,
  upper = ~ income + age + years_with_bank + nbr_children + gender +
    marital_status + ckacct + ccacct +
    #avg_ck_bal +
    avg_cc_bal +
    avg_ck_tran_amt + avg_cc_tran_amt +
    avg_ck_tran_cnt + avg_cc_tran_cnt)

## visualise data:
par(mfrow=c(2,2))
library(Hmisc)
sapply(which(names(dat.train) %in% varnames_model),
       function(i) {
         if(is.numeric(dat.train[,i ])) {
           plot(y = dat.train$svacct,
                x = cut2(dat.train[, i], g=10),
                main = names(dat.train)[i])
         }
       }
)
par(mfrow=c(1,1))

## estimate a null model (just intercept):
nullmodel <- glm(svacct ~ 1, data = dat.train,
                 family=binomial(link="logit"))

## do the stepwise regression:
fit.glm2ex <- stepAIC(nullmodel, scope=scopedef, direction="both", trace=TRUE)
summary(fit.glm2ex)

pR2(fit.glm2ex)            ## pseudo R^2
NagelkerkeR2(fit.glm2ex)   ## Nagelkerke R^2

library(caret)
## confusion matrix on training data:

dat.this <- dat.train
pred.this <- predict(fit.glm2ex, newdata=dat.this, type="response")
true.this <- dat.this$svacct
cutoff <- 0.5

pred.this.num <- as.numeric(pred.this > cutoff)
pred.this.class <- factor(pred.this.num, levels=c(0,1), labels=c("0", "1"))

## confusion matrix and metrics:
confusionMatrix(pred.this.class, reference=true.this, positive="1")

## lift curve
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
text(0.8, 0.2, paste("AUC = ", round(100*perf@y.values[[1]], 2), "%", sep=""),col=2)


## confusion matrix on test data:
dat.this <- dat.test
pred.this <- predict(fit.glm2ex, newdata=dat.this, type="response")
true.this <- dat.this$svacct
cutoff <- 0.5

pred.this.num <- as.numeric(pred.this > cutoff)
pred.this.class <- factor(pred.this.num, levels=c(0,1), labels=c("0", "1"))

## confusion matrix and metrics:
confusionMatrix(pred.this.class, reference=true.this, positive="1")

## lift curve
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
text(0.8, 0.2, paste("AUC = ", round(100*perf@y.values[[1]], 2), "%", sep=""),col=2)


## ============================================================================
## estimation using the caret package
## ============================================================================

# ## glm function:
# fit.glm1 <- glm(ccacct ~ income + age + years_with_bank + nbr_children +
#         gender + marital_status + ckacct + svacct +
#         avg_ck_bal + avg_ck_tran_amt + avg_sv_tran_amt +
#         avg_ck_tran_cnt + avg_sv_tran_cnt,
# 		data = dat.train,
# 		family=binomial(link="logit"))
# summary(fit.glm1)
#
# ## more here

dat.all <- rbind(dat.train, dat.test)

## use the .632+ bootstrap with 3 repeats this time (performs better for x-val):
tc <- trainControl(
    method = "boot632",
    number = 25,
    repeats = 1,  		## repeated once
    classProbs=TRUE)

## fit glm using caret:
fit.caret.glm1 <- train(as.numeric(as.character(ccacct)) ~
        income + age + years_with_bank + nbr_children +
        gender + marital_status + ckacct + svacct +
        avg_ck_bal + avg_ck_tran_amt + avg_sv_tran_amt +
        avg_ck_tran_cnt + avg_sv_tran_cnt,
		data=dat.all,
    trControl=tc,
		method="glm",
		family="binomial")
fit.caret.glm1

summary(fit.caret.glm1)
summary(fit.caret.glm1$finalModel)

## fit stepwise glm using caret:
fit.caret.glm1.step <- train(as.numeric(as.character(ccacct)) ~
        income + age + years_with_bank + nbr_children +
        gender + marital_status + ckacct + svacct +
        avg_ck_bal + avg_ck_tran_amt + avg_sv_tran_amt +
        avg_ck_tran_cnt + avg_sv_tran_cnt,
		data=dat.all,
    trControl=tc,
		method="glmStepAIC",
		family="binomial")
fit.caret.glm1.step

summary(fit.caret.glm1.step)
confusionMatrix(round(fit.caret.glm1.step$finalModel$fitted.values,0),dat.all$ccacct)

## ============================================================================
## save workspace
## ============================================================================

## save.image(file=paste0(path.r, "r-train-td-hun_v001c.Rdata"))
## load(file=paste0(path.r, "r-train-td-hun_v001c.Rdata"))