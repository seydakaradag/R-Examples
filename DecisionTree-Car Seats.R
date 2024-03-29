
#################################################################
##        Fitting Classification Trees                         ##
##        Carseats Data                                        ##
#################################################################

install.packages("tree")
install.packages("ISLR")


#The tree library is used to construct classification and regression trees.
library (tree)
library (ISLR)

attach (Carseats) #veri k�mesini paketten alip R'a y�kledik

#Observe a few examples
head(Carseats)


#!!!amacimiz carseats datasindaki verilere g�re, o b�lgede yeni bir d�kkan a�arsak tutar mi tutmaz miyi belirlemek!!!!!!!!!
# �nce bagimli degisken olus



#We use the ifelse() function to create a variable, called High, which takes on a value of Yes 
#if the Sales variable exceeds 8, and takes on a value of No otherwise.
High = ifelse(Sales <=8,"No","Yes")
#�nce bagimli degisken tanimladik. 8000den fazla satis varsa �ok satis diyoruz.
#if else komutu ile basite yaptik.




#Finally, we use the data.frame() function to merge High with the rest of the Carseats data.
Carseats = data.frame(Carseats, High)
#yukarida olusturdugumuz High kolonunu dataya ekledik. yeni bir bagimli degisken yapmis olduk.


#Develop a prediction model using training and test sets 
set.seed(2)
#test ve training k�me ayiriyoruz. biri learning biri test i�in.
#seed ile bir sonraki �alistirmamizda ayni sayilari bulmasini sagladik.



#select random 200 rows as training set
trainrows = sample(1:nrow(Carseats), 200)
traindata = Carseats[trainrows,]
#200 tane rastgele se�ilen veri k�mesini training datasi olarak belirledik.



#use the remaining rows as test set
testdata = Carseats[-trainrows,]
#test i�in, b�t�n datadan trainingi �ikardik. kalanlar test.


#Decision Tree - Fit the Data to the model
#use the tree() function to fit a classification tree in order to predict tree() High using all variables but Sales. 
treepredmodel = tree(formula= High~Advertising+Price+Age+Education+Urban, data = traindata )
#tree fonksiyonu ile karar agaci �alistiriyoruz.  
# ==> ~ bu isaretin sol tarafindaki bizim bagimli degiskenimiz. digerleri bagimsiz ve training datasini kullaniyoruz. 


#The summary() function lists the variables that are used as internal nodes
#in the tree, the number of terminal nodes, and the (training) error rate.
summary(treepredmodel)
#modelin nasil bir model oldugunu g�rmek i�in summary ile modelin �zelliklerine bakiyoruz.
#summary ile misclassification error rate cikacak. modelin hata payini g�sterir.
#ilk �alistirmamda error rate 0.175 �ikti. 200 satirdan 35 ini yanlis classification yapmis.



# plot the trained tree 
plot(treepredmodel )
text(treepredmodel,pretty=0)
#�ikan modeli �izdirdik.


#Test the predictions of the trained model
testpredictions = predict(treepredmodel, t.estdata, type ="class")
#modeli bir daha egitmiyoruz. yeni bir veri k�mesi kullanarak test ediyoruz. kullanacagi veri k�mesini test olarak degistirdik.
#decision tree bir siniflandirma oldugundan type class



#Find the accuracy of the model
table(testpredictions, testdata$High)
#(15+41)/200
#burada bir confusion matrix  olusturuyoruz. hatali olan adet / toplam veri adedi bize basari oranini verir.


#overfitting sorununu ��zmek i�in kullandigimiz samplelari degistirebiliriz veya modeli basitlestirebiliriz.
#trainingde iyi ve testingde �ok k�t� sonu�lar �ikiyorsa buna overfitting deniyor.aradaki fark �ok b�y�k olmamali.
#overfittting cok sesifik �rneklere dalma


#underfitting is model �ok basitse ve yargi yaparken yeterinde dogru yapamiyorsa underfitting.
#underfitting ise asiri genelleme
#underfittingi engellemek i�in de modele farkli ve yeni degiskenler eklenmeli




