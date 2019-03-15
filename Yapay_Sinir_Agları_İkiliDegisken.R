#NN Yapay sinir aðlarý
install.packages("neuralnet")
require(neuralnet)
library(MASS)
library(grid)
?neuralnet


set.seed(1234)#
ind <- sample(2, nrow(HATCO), replace = TRUE, prob = c(0.7, 0.3))
train.data <- HATCO[ind == 1, ]
test.data <- HATCO[ind == 2, ]


nn = neuralnet(binx19~x6+x9+x11+x12+x16,data = train.data, hidden = c(3,3),err.fct = "ce",
               algorithm = "rprop+", linear.output = FALSE)

plot(nn)
nn$net.result
nn$weights
nn$result.matrix
nn$covariate
#mavi görünenler errorler ve weightleri belirlemis olduk.


ypred = compute(nn,test.data[,c("x6","x9","x11","x12","x16")])
str(ypred)

ypred <- ifelse (ypred$net.result>0.5,1,0)


#dogruluk test sonuçlario
library(gmodels)
CrossTable(test.data$binx19, ypred)
tab <- table(Predicted = ypred, Actual = test.data$binx19)
tab
