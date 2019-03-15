library(readxl)
HATCO <- read_excel("C:/R_data/HBAT.xlsx")
View(HATCO)


mean(x19)
HATCO$binx19 <- ifelse (x19 >= mean(x19),1,0)
attach(HATCO)
View (HATCO)


# Logistic Regression
log.model<-glm(binx19~x6+x9+x11+x12+x16,data = HATCO, family = "binomial")
summary(log.model)
#glm=general linear model (logisticde bu kullanilir)


#kikareyi anlamak için modelin en kötü degiskeni olan x11 ile modeli çalistiriyoruz.
log.model<-glm(binx19~x11,data = HATCO, family = "binomial")
summary(log.model)


#kikareyi düsürmek için degiskenleri ekliyoruz.
log.model<-glm(binx19~x6+x9+x11+x12+x16,data = HATCO, family = "binomial")
summary(log.model)
#model iyi çikti. bu degiskenlerden bir veya birkaçi modeli etkiliyor peki hangileri?
qchisq(0.95, df=5)#df serbestlik 
#esik degeri 11 cikti. kikare farklari 11 cikmali. kikare 


exp(2.1813)#x6 nin katsayisi. x6 en önemli degisken. 
#x6yi bir birim arttirirsam memnun olma olasiligi, olmama olasiligona göre 8 kat artar.


#x11e bakiyoruz.
exp (0.3614)
#1.4 cikti. önemli bir fark yaratmadi.



#Modelin dogrulugunu kontrol eidyoruz.
ypred <- predict(log.model,HATCO)
str(ypred)

#olasiligi binary sekline çeviriyoruz. ve çapraz tablosunu olusturuyoruz.
ypredbin <- ifelse(ypred>0.5,1,0)

#dogruluk test sonuçlario
library(gmodels)
CrossTable(HATCO$binx19, ypredbin)
tab <- table(Predicted = ypredbin, Actual = HATCO$binx19)
tab























