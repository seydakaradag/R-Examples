#################################################################
##        Support Vector Machine (SVM)                         ##
##        Social Network Ads Dat                               ##
#################################################################

# Load SVM Package 
install.packages("e1071")
library(e1071)

install.packages("randomForest")
library(randomForest)


getwd() #çalistigimiz dizini gösterir.
 
# Importing the dataset
dataset = read.csv('Social_Media_Advertisements.csv')
dataset = dataset[3:5] #sadece 3 ve 5 arasindaki kolonlari aldik
head(dataset)

# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))
#bagimli degisken purchased kolonu. kisi bizden ürün alir mi almaz miyi bulacagiz.
#support vektor machine bagimli degisken faktör olarak gelmeli. yani kategorik.
#bu yuzden bagimli degiskeni faktöre degistirmek için factor fonksiyonu kullandik.

#Develop a prediction model using training and test sets 
set.seed(3)

#select random 300 rows as training set
trainrows = sample(1:nrow(dataset), 300)#1 ve 300 arasinda random
traindata = dataset[trainrows,]


#use the remaining rows as test set
testdata = dataset[-trainrows,]


#Normalization of features, Scaling values by subtracting the mean and dividing by the std. 
traindata[-3] = scale(traindata[-3])
testdata[-3] = scale(testdata[-3])
#normalizasyon için R'da scale fonksiyonu kullanilir. Estimated salary normalize edilmesi gereken bir kolon.
#scale z skoruna göre normalizasyon yapan bir fonksiyon. 
#bagimli degisken normalize edilmez. bagimli degiskenimiz 3.kolon. Bu yuzden -3 yazdik.



# Fitting SVM to the Training set
svmmodel = svm(formula = Purchased ~ ., 
               data = traindata, 
               type = 'C-classification', 
               kernel = 'linear')

#bagimli degisken solda ve bagimsizlar sagda. bütün kolonlari kullanacaksak,
#tek tek bagimsizlari yazmak yerine nokta koyabiliriz.

#Basic information about SVM Model
summary(svmmodel)
#cost ve gamma diye 2 deger üretecek. gamma içte olusan çemberin çapi.


#Predicting the Test set results
predictions = predict(svmmodel, newdata = testdata[-3])
#test data üzerinde test ediyoruz ama bagimli degiskeni çikardik. ve confusion matrix olusturduk.


#Confusion Matrix of predictions
table(testdata[, 3], predictions)
#test data üzerinde test ediyoruz ama bagimli degiskeni çikardik. ve confusion matrix olusturduk.



#Plotting the data
plot(testdata[,1], testdata[,2], 
     main = 'SVM Test Data Predictions',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = c(min(testdata[, 1]) - 1, max(testdata[, 1]) + 1), 
     ylim = c(min(testdata[, 2]) - 1, max(testdata[, 2]) + 1)
     )
#plot the predicted points
points(testdata, pch = '.', col = ifelse(predictions == 1, 'springgreen3', 'tomato'))
points(testdata, pch = 21, bg = ifelse(testdata[, 3] == 1, 'green4', 'red3'))

#Plotting the data. Clearly the class boundary is nonlinear
plot(svmmodel, traindata, xlab = 'Age', ylab = 'Estimated Salary')



##################################
#Kernel SVM Model
##################################

#sonuç yeterince iyi çikmadi. çünkü linear ayrilmadi. o yuzden radial yani curve ayirdik.
kernelmodel = svm(formula = Purchased ~ ., 
                  data = traindata, 
                  type = 'C-classification',
                  kernel = 'radial')

# Predicting the Test set results
kernelpred = predict(kernelmodel, newdata = testdata[-3])

#Plotting the data. Radial Basis kernel is fitting better
plot(kernelmodel, traindata, xlab = 'Age', ylab = 'Estimated Salary')

#Confusion Matrix of Kernel predictions
table(testdata[, 3], kernelpred)

#Basic information about SVM Model
summary(kernelmodel)


##################################
#Tuning SVM Kernel Parameters
##################################
#hangi parametreler optimum seçmeliyim ki dah abasarili sonuçlar seçeyim? Buna tunning olur.

#We can perform cross-validation using tune() to select the best choice of
# gamma and cost for an SVM with a radial kernel
set.seed(1)
tune.out=tune(svm, Purchased~., data=traindata, kernel ="radial",
              ranges=list(cost=c(0.1, 1, 10, 100, 1000), gamma=c(0.5,1,2,3,4)))

summary(tune.out)

#We can view the test set predictions for this model by applying the predict function 
pred=predict(tune.out$best.model, newdata=testdata)
table(testdata[,3], pred)


