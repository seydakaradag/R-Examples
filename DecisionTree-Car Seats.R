
#################################################################
##        Fitting Classification Trees                         ##
##        Carseats Data                                        ##
#################################################################

install.packages("tree")
install.packages("ISLR")


#The tree library is used to construct classification and regression trees.
library (tree)
library (ISLR)

attach (Carseats) #veri kümesini paketten alip R'a yükledik

#Observe a few examples
head(Carseats)


#!!!amacimiz carseats datasindaki verilere göre, o bölgede yeni bir dükkan açarsak tutar mi tutmaz miyi belirlemek!!!!!!!!!
# önce bagimli degisken olus



#We use the ifelse() function to create a variable, called High, which takes on a value of Yes 
#if the Sales variable exceeds 8, and takes on a value of No otherwise.
High = ifelse(Sales <=8,"No","Yes")
#önce bagimli degisken tanimladik. 8000den fazla satis varsa çok satis diyoruz.
#if else komutu ile basite yaptik.




#Finally, we use the data.frame() function to merge High with the rest of the Carseats data.
Carseats = data.frame(Carseats, High)
#yukarida olusturdugumuz High kolonunu dataya ekledik. yeni bir bagimli degisken yapmis olduk.


#Develop a prediction model using training and test sets 
set.seed(2)
#test ve training küme ayiriyoruz. biri learning biri test için.
#seed ile bir sonraki çalistirmamizda ayni sayilari bulmasini sagladik.



#select random 200 rows as training set
trainrows = sample(1:nrow(Carseats), 200)
traindata = Carseats[trainrows,]
#200 tane rastgele seçilen veri kümesini training datasi olarak belirledik.



#use the remaining rows as test set
testdata = Carseats[-trainrows,]
#test için, bütün datadan trainingi çikardik. kalanlar test.


#Decision Tree - Fit the Data to the model
#use the tree() function to fit a classification tree in order to predict tree() High using all variables but Sales. 
treepredmodel = tree(formula= High~Advertising+Price+Age+Education+Urban, data = traindata )
#tree fonksiyonu ile karar agaci çalistiriyoruz.  
# ==> ~ bu isaretin sol tarafindaki bizim bagimli degiskenimiz. digerleri bagimsiz ve training datasini kullaniyoruz. 


#The summary() function lists the variables that are used as internal nodes
#in the tree, the number of terminal nodes, and the (training) error rate.
summary(treepredmodel)
#modelin nasil bir model oldugunu görmek için summary ile modelin özelliklerine bakiyoruz.
#summary ile misclassification error rate cikacak. modelin hata payini gösterir.
#ilk çalistirmamda error rate 0.175 çikti. 200 satirdan 35 ini yanlis classification yapmis.



# plot the trained tree 
plot(treepredmodel )
text(treepredmodel,pretty=0)
#çikan modeli çizdirdik.


#Test the predictions of the trained model
testpredictions = predict(treepredmodel, t.estdata, type ="class")
#modeli bir daha egitmiyoruz. yeni bir veri kümesi kullanarak test ediyoruz. kullanacagi veri kümesini test olarak degistirdik.
#decision tree bir siniflandirma oldugundan type class



#Find the accuracy of the model
table(testpredictions, testdata$High)
#(15+41)/200
#burada bir confusion matrix  olusturuyoruz. hatali olan adet / toplam veri adedi bize basari oranini verir.


#overfitting sorununu çözmek için kullandigimiz samplelari degistirebiliriz veya modeli basitlestirebiliriz.
#trainingde iyi ve testingde çok kötü sonuçlar çikiyorsa buna overfitting deniyor.aradaki fark çok büyük olmamali.
#overfittting cok sesifik örneklere dalma


#underfitting is model çok basitse ve yargi yaparken yeterinde dogru yapamiyorsa underfitting.
#underfitting ise asiri genelleme
#underfittingi engellemek için de modele farkli ve yeni degiskenler eklenmeli




