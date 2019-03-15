#NN Yapay sinir aglari
install.packages("neuralnet")
require(neuralnet)
library(MASS)
library(grid)
?neuralnet


set.seed(1234)#
ind <- sample(2, nrow(HATCO), replace = TRUE, prob = c(0.7, 0.3))
train.data <- HATCO[ind == 1, ]
test.data <- HATCO[ind == 2, ]


nn = neuralnet(x19~x6+x9+x11+x12+x16,data = train.data, hidden = c(3,3),err.fct = "sse",
               algorithm = "rprop+", linear.output = FALSE)

plot(nn)
nn$net.result
nn$weights
nn$result.matrix
nn$covariate
#mavi görünenler errorler ve weightleri belirlemis olduk.


ypred = compute(nn,test.data[,c("x6","x9","x11","x12","x16")])
str(ypred)


ypred <- ypred$net.result*(max(test.data$x19)-min(test.data$x19))+min(test.data$x19)
actual <- (test.data$x19)*(max(test.data$x19)-min(test.data$x19))+min(test.data$x19)

MSE <- sum((ypred - actual)^2)/nrow(test.data)
MSE

plot(test.data$x22,ypred,col='blue', main = 'Real vs Predicted',pch=1, cex=0.9, 
     type = 'p', xlab = 'Actual', ylab = 'Predicted' )


