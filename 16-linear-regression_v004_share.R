## ############################################################################
## R Introduction and Best Practice
## Linear Regression
## May 2016
## ############################################################################

#rm(list=ls())

path.raw <- "C:/Projects/Vodafone_TR/"
#path.raw <- "C:/data-sync/teradata-work/teradata-project/training-knowledge-exchange/r-training-general/"
path.r <- paste0(path.raw, "r-scripts/")
path.dat <- paste0(path.raw, "data/")
path.tmp <- paste0(path.raw, "tmp/")

## ============================================================================
## libraries
## ============================================================================

library(gdata)

## ############################################################################
## ############################################################################
## Linear regression
## ############################################################################
## ############################################################################

## ============================================================================
## data (from file)
## ============================================================================

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## data
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat.train <- read.csv(paste0(path.dat, "twm_customer_analysis_train.csv"))
dat.test <- read.csv(paste0(path.dat, "twm_customer_analysis_test.csv"))

## inspect data:
str(dat.train)

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

#save(dat.train, file=paste0(path.dat, "twm_customer_analysis_train.Rdata"))
#save(dat.test, file=paste0(path.dat, "twm_customer_analysis_test.Rdata"))

#load(file=paste0(path.dat, "twm_customer_analysis_train.Rdata")) ## contains dat.train
#load(file=paste0(path.dat, "twm_customer_analysis_test.Rdata"))  ## contains dat.test


## select subset of data that have a savings account:
dat.sv <- subset(dat.train,
		subset= (dat.train$svacct!=0) & (dat.train$avg_sv_bal!=0))

dim(dat.sv)


## ============================================================================
## linear regression
## ============================================================================

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## single predictor
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## estimate a model
## and store results in a variable:

lm1 <- lm(avg_sv_bal ~ income, data=dat.sv)
summary(lm1)

## apply different functions to access results:

summary(lm1)$adj.r.squared

coef(lm1)
coef(summary(lm1))

summary(lm1)$coefficient[,2]   # standard error, without wrapper (not advised)
coef(summary(lm1))[,2]         # better: wrapper is more robust

## plot data:
plot(avg_sv_bal ~ income, data=dat.sv)    # plot data
abline(lm1, col="red", lwd=3)   # add regression line

resid(lm1)
hist(resid(lm1), col="grey", nclass=20)

## really does not look very good...
## estimate another model with a log-transformed variable:

lm2 <- lm(log(avg_sv_bal) ~ income, data=dat.sv)
summary(lm2)

plot(log(avg_sv_bal) ~ income, data=dat.sv) # plot data
abline(lm2, col="red", lwd=3)     # add regression line

hist(resid(lm2), col="grey", nclass=20)

## plot with ggplot:
library(ggplot2)
ggplot(dat.sv, aes(x=income, y=log(avg_sv_bal))) + geom_point() +
    geom_smooth(method="lm")
ggplot(data.frame(dat.sv, "resid"=resid(lm2)), aes(x=resid)) +
    geom_histogram(binwidth=.2)

## plot with ggplot and fortify:
head(fortify(lm2))
ggplot(fortify(lm2), aes(x=income, y=.fitted+.resid)) + geom_point() + geom_line(aes(y=.fitted), color="red")
ggplot(fortify(lm2), aes(x=.resid)) + geom_histogram()


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## multiple predictors
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## this time, include a categorial variable:
## are dummy-coded automatically (if is.factor())
## (we will see what that means in a minute)

## estimate a model with 2 predictors,
## one continuous, one categorial:

lm3 <- lm(log(avg_sv_bal) ~ income + avg_ck_bal+ marital_status,
          data=dat.sv)
summary(lm3)

## note:
## - first level is always used as a reference
## - use relevel() to change that
dat.sv$marital_status <- relevel(dat.sv$marital_status, ref="married")

## re-estimate the model:
lm3 <- lm(log(avg_sv_bal) ~ income + avg_ck_bal+ marital_status,
          data=dat.sv)
summary(lm3)

## back to "single" as a reference level:
dat.sv$marital_status <- relevel(dat.sv$marital_status, ref="single")


## compare models:

anova(lm2, lm3)   # F test comparing models

## no significant difference here...


## ============================================================================
## exercise LINREG
## ============================================================================

## - load the dataset Computers from the Ecdat package
## - fit the model price ~ speed
## - plot these two variables, as well as the regression line
## - fit the model price ~ speed + ram
## - compare these models

## ============================================================================
## solution to exercise LINREG:
## ============================================================================

library(Ecdat)                               ## load package
data(Computers)                              ## load dataset from package
lmex1 <- lm(price ~ speed, data=Computers)   ## fit linear regression model
summary(lmex1)                               ## model summary

## plot:
plot(price ~ speed, data=Computers)          ## scatterplot
plot(price ~ jitter(speed), data=Computers)
abline(lmex1, col="red")                     ## add regression line to plot

## plot with jitter:
plot(price ~ jitter(speed), data=Computers)          ## scatterplot
abline(lmex1, col="red")                     ## add regression line to plot

## plot of residuals:
hist(resid(lmex1), nclass=20, col="grey")

lmex2 <- lm(price ~ speed + ram, data=Computers)  ## second model
summary(lmex2)

## plot of residuals:
hist(resid(lmex2), nclass=20, col="grey")

## compare models:
anova(lmex1, lmex2)


## ============================================================================
## Stepwise regression
## ============================================================================

## based on AIC (the lower, the better)
## variables are selected into the model
## or removed from it

## for forward selection, a scope has to be defined:
## (as a list)

scopedef <- list(
		lower = ~1,
		upper = ~(income + age + years_with_bank + nbr_children +
					gender + 	marital_status + state_code +
					avg_cc_bal + avg_ck_bal + avg_cc_tran_amt +
					avg_cc_tran_cnt + avg_ck_tran_amt + avg_ck_tran_cnt +
					avg_sv_tran_amt + avg_sv_tran_cnt + cc_rev)^2)

## maximal model: all variables up to the second interaction

## starting point: null model (intercept only):

nullmodel <- lm(log(avg_sv_bal) ~ 1, data = dat.sv)

library(MASS)
lm.step <- stepAIC(nullmodel, scope=scopedef,
		direction="both", trace=TRUE)

summary(lm.step)

## plot residuals:
hist(resid(lm.step), nclass=20, col="grey")

## Quantile-Quantile Plot against normal distribution
library(car)
qqPlot(resid(lm.step))

## QQ-Plot with ggplot:
ggplot(fortify(lm.step)) + stat_qq(aes(sample=.stdresid)) + geom_abline(slope=1, linetype=2)
## (can actually omit slope parameter)

## ============================================================================
## saving and loading the model
## ============================================================================

#path.raw <- "C:/data-sync/teradata-work/teradata-project/training-knowledge-exchange/r-on-aster-italy/"
#path.r <- paste0(path.raw, "r-scripts/")

## save model:
save(lm.step, file=paste0(path.tmp, "model-file.Rdata"))

## save more than 1 object:
save(lm.step, nullmodel, file=paste0(path.tmp, "model-file.Rdata"))

## use list:
save(list=c("lm.step", "nullmodel"), file=paste0(path.tmp, "model-file.Rdata"))

## save all that is in the workspace:
## (not recommended for just saving the model, but convenient for data dumps)
save.image(file=paste0(path.tmp, "r-train-dump.Rdata"))


## loading the model file (or any other data dump):
load(file=paste0(path.tmp, "model-file.Rdata"))

load(file=paste0(path.tmp, "r-train-dump.Rdata"))

## ============================================================================
## Scoring: Function predict() or fitted()
## ============================================================================

?predict

class(lm.step)

methods("predict")
?predict.lm


## for the model (no new data):
predict(lm.step)
#predict(lm.step, type="response")

## using new data:
## (has to be of same structure (contain all relevant variables))

## same selection in the test set:
dat.sv.test <- subset(dat.test,
    subset= (dat.test$svacct!=0) & (dat.test$avg_sv_bal!=0))

pred.test <- predict(lm.step, newdata=dat.sv.test)
head(pred.test)


## fake data:
newdata <- data.frame(
		"avg_sv_tran_amt" = c(1.3, 5.5),
		"income" = c(10000, 20000))
predict(lm.step, newdata=newdata, type="response", interval="confidence", level=0.997)

## missing values: will predict NA by default

newdata <- data.frame(
		"avg_sv_tran_amt" = c(1.3, NA),
		"income" = c(10000, 20000))
newdata
predict(lm.step, newdata=newdata, type="response")

## attach prediction to original data frame:
cbind(newdata, "prediction"=predict(lm.step, newdata=newdata))
data.frame(newdata, "prediction"=predict(lm.step, newdata=newdata))

## ============================================================================
## plotting predicted values
## ============================================================================

par(mfrow=c(1,2))
plot(log(dat.sv$avg_sv_bal) ~ dat.sv$avg_sv_tran_amt)
points(predict(lm.step) ~ dat.sv$avg_sv_tran_amt, col="red", pch=3)

plot(log(dat.sv$avg_sv_bal) ~ dat.sv$income)
points(predict(lm.step) ~ dat.sv$income, col="red", pch=3)

## additionally: 3D Plot

dev.new()
library(rgl)
plot3d(log(dat.sv$avg_sv_bal) ~ dat.sv$income + dat.sv$avg_sv_tran_amt)
points3d(fitted(lm.step) ~ dat.sv$income+ dat.sv$avg_sv_tran_amt, col="red")

## with ggplot:
head(fortify(lm.step))
ggplot(fortify(lm.step), aes(x=avg_sv_tran_amt, y=.fitted+.resid)) +
    geom_point(alpha=.5) +
    geom_point(aes(y=.fitted), color="red", alpha=.7)

ggplot(fortify(lm.step), aes(x=income, y=.fitted+.resid)) +
    geom_point(alpha=.5) +
    geom_point(aes(y=.fitted), color="red", alpha=.7)


## ============================================================================
## Exercise STEPWISE REGRESSION
## ============================================================================

## Tasks:
## - load the dataset Computers from the Ecdat package
## - use stepwise regression to fit the best model to predict price
## - using this model, predict the price for a computer with
##   16mb RAM, premium=yes, speed=66MHz, ads=300, screen=15 inches, cd-drive, 300mb harddisk, multi=yes


## ============================================================================
## Solution to exercise STEPWISE REGRESSION
## ============================================================================

## load data set:
library(Ecdat)
data(Computers)
str(Computers)

hist(log(log(Computers$price)),nclass=40)
hist(Computers$price),nclass=40)


## define scope:
scopedef <- list(
  lower= ~1,
  upper= ~speed+hd+ram+screen+cd+multi+premium+ads)

## estimate null model: just intercept
nullmodel <- lm(price ~ 1, data=Computers)

## do stepwise regression:
fit.price.step <- stepAIC(nullmodel, scope=scopedef, direction="both", trace=TRUE)
summary(fit.price.step)

## create new data with desired inputs:
newdat <- data.frame(
  "ram"=16,
  "premium"="yes",
  "speed"=66,
  "ads"=300,
  "screen"=15,
  "cd"="yes",
  "hd"=300,
  "multi"="yes")

## predict price
predict(fit.price.step,newdata=newdat)
summary(fit.price.step)

## ============================================================================
## Regression diagnostics
## ============================================================================

## Assumptions:
## - normality of residuals (for significance testing)
## - no structure in residuals
## - no outliers
## - no significant autocorrelation of residuals
## - no (perfect) multicollinearity
## - no heteroscedasticity

## ------------------------------------------------------------------------
## standard regression plots:
## ------------------------------------------------------------------------

par(mfrow=c(2,2)) # to view the 4 plot simultaneously
plot(lm.step)

## - no structure in residuals (top left)
## - normally distributed residuals, almost (top right)
## - no outliers that influence regression very much (bottom right)
##   (although one has to be careful here!)

## ------------------------------------------------------------------------
## normality of residuals
## ------------------------------------------------------------------------

par(mfrow=c(1,1))

library(nortest)   # contains tests for normality
library(car)
qqPlot(resid(lm.step))   # in library car
hist(resid(lm.step))

## ------------------------------------------------------------------------
## influence of single points: hat values (leverage)
## ------------------------------------------------------------------------


## calculate and plot hat values (histogram):
hatvalues(lm.step)
hist(hatvalues(lm.step), nclass=20, col="grey")
abline(v=2*mean(hatvalues(lm.step)))
abline(v=3*mean(hatvalues(lm.step)))

## average hat value: (k+1)/n
length(coef(lm.step))/nrow(dat.sv)
mean(hatvalues(lm.step))
## hatvalue > 2 * mean(hatvalues) needs attention
## but rather conservative with large datasets
## and rather critical with small datasets (hence 3*mean here)
## Belsley, Kuh, and Welsch (1980), Regression Diagnostics

## basic leverage plot:
plot(hatvalues(lm.step), type="h")
abline(h=2*mean(hatvalues(lm.step)), lty=2)
abline(h=3*mean(hatvalues(lm.step)), lty=3)

## leverage plot:
leveragePlots(lm.step)

## According to Sall (1990):
## Leverage plots can show the point-by-point composition of the sum of squares
## for a hypothesis test.
## They are valuable in revealing the degree of fit, the parameter estimates,
## the residuals, a measure of the variance of the fit, influential points,
## nonfitting points, nonlinearities and even collinearity
## Sall, J. (1990). Leverage Plots for General Linear Hypotheses.
## The American Statistician, 44(4), 308–315. doi:10.1080/00031305.1990.10475750

## cook's distance:
## measures the effect of deleting one observation
## (larger for large residuals or large leverage):
plot(lm.step)
plot(lm.step, which=4)

## residuals and cook's distance:
plot(lm.step, which=5)


## ------------------------------------------------------------------------
## normally distributed residuals
## ------------------------------------------------------------------------

shapiro.test(resid(lm.step))
lillie.test(resid(lm.step))  # in library nortest

## not by significance testing, but over-sensitive for big sample sizes

length(resid(lm.step))

## ------------------------------------------------------------------------
## autocorrelation of residuals:
## ------------------------------------------------------------------------

library(lmtest)      # test for linear models
dwtest(lm.step)      # Durbin-Watson test
bgtest(lm.step,      # Breusch-Godfrey test
  order=2)           # for higher-order correlations

## ------------------------------------------------------------------------
## multicollinearity:
## ------------------------------------------------------------------------

## variance inflaction factor:
vif(lm.step)


## ------------------------------------------------------------------------
## heteroscedasticity:
## ------------------------------------------------------------------------

## statistical tests:
hmctest(lm.step)
hmctest(lm.step, plot=TRUE)  ## plot HMC statistics over all possible breakpoints (comparing variance)
bptest(lm.step)
gqtest(lm.step)

ncvTest(lm.step)  ## score test for non-constant error variance with fitted values

## but easiest is graphically:
plot(lm.step, which=1)

# plot abs studentized residuals vs. fitted values
spreadLevelPlot(lm.step)  ## slope should be 0; if not, suggests possible power transformation


## see also:
## http://www.statmethods.net/stats/rdiagnostics.html
## http://polisci.msu.edu/jacoby/icpsr/regress3/lectures/week4/14.Heteroskedastic.pdf

## ------------------------------------------------------------------------
## robust linear regression
## ------------------------------------------------------------------------

## refit lm.step with robust regression and compare fitted values (predictions):
lm.step

## robust regression using an M estimator:
rlm.step <- rlm(log(avg_sv_bal) ~ avg_sv_tran_amt + income, data=dat.sv, method="MM")

coef(lm.step)
coef(rlm.step)

par(mfrow=c(2,4))
plot(lm.step)
plot(rlm.step)

tmp <- data.frame(
    "y.lm"= predict(lm.step),
    "y.rlm"= predict(rlm.step),
    "y.absdiff"=abs(predict(rlm.step)-predict(lm.step)),
    "y.exp.lm"= exp(predict(lm.step)-1),
    "y.exp.rlm"= exp(predict(rlm.step)-1),
    "y.exp.absdiff"= exp(predict(rlm.step)-1) - exp(predict(lm.step)-1))

par(mfrow=c(1,1))
hist(tmp$y.exp.absdiff, col="grey", nclass=20)
sort(tmp$y.exp.absdiff)

## robust regression predicts smaller values here...
## max difference 150 USD


## see
## http://www.ats.ucla.edu/stat/r/dae/rreg.htm

## ============================================================================
## save workspace
## ============================================================================

## save.image(file=paste0(path.r, "r-train-td-hun_v001b.Rdata"))
## load(file=paste0(path.r, "r-train-td-hun_v001b.Rdata"))