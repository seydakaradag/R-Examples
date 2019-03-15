
#datayi csv olarak import ettik.

sales <- read.csv ("C:/R_data/sales.csv",header = F)
head(sales)

sales


#sales datasini zaman serisine çeviriyoruz.aylik data oldugundan 12, haftalik olsaydi frekans 52 olurdu.
#start ile datanin basladigi noktasini gösteriyoruz.
sales <- ts(sales,frequency = 12, start = c(2010,1))
sales #kontroll ettik zaman serisine dönmüs oldu


#çizdirip trendine baktik.
plot(sales)


#exponential smoothing uygulyoruz. Exponentiallarin genel  methodu holtwinters() dir.
#holtwinters ekstra bisey belirtilmedikçe, datayi duragan yani sezonsallik ve trend olmadigini varsayar.
#datada sezonsallik ve trend varsa exponential smoothing çalismaz.
#holtwintersda beta trendi gösteriri. FAlse dersek trendi hesaba katmaz.
#Gamma ise sezonsalliktir ve false diyerek sezonsalligida datadan cikardik.
#alpha düzleme katsayisi
mdl <- HoltWinters(sales, beta = FALSE, gamma = FALSE, alpha = 0.3)
plot(mdl)


#alphayi vermezsek kendi bulur ve optimize eder.
mdl <- HoltWinters(sales, beta = FALSE, gamma = FALSE)
plot(mdl)


#kendi fonksiyonumuzu yaziyoruz. function() ile yazilir.
#fonksiyonlar süslü parantez içerisine yazilir.
#kendimiz fonsiyon yazarak, modeli dogrulayacak mad, mse, mape formüllerini yazdik.
#cat fonksiyonu ile string ifade yazdirdik ve \n ile alt satir, bir satir bosluk biraktiridk.
performance <- function(y,ypred){
  mse  <- mean((y-ypred)^2)
  mad  <- mean(abs(y-ypred))
  mape <- mean(abs(y-ypred)/abs(y))*100
  cat("MSE is   :",mse,"\n")
  cat("MAD is   :",mad,"\n")
  cat("MAPE is  :",mape,"\n")
}


#olusturdugumuz performance fonksyonunu çagiriyoruz. fonksiyonu yazarken y, ypred kullanmistik. y burada gercek deger yani sales datasi.
#ypred ise tahmin edilen. yani model. ancak modelin ilk kolonunu aliyoruz.
#konsol üzerinde mdl$fitted çalistirarak modelin çiktilarina bakabiliriz.
performance(sales,mdl$fitted[,1])


#zaman serisi modellerinde test ve trainingleri random seçemeyiz. örnegin ilk 2 seneyi trainin ve son 1 yili test için aldik.
#zaman serilerinde window komutu ile data ayristiriliyor.
#end, train kümesinin bitecegi yeri gösteriyor. datanin basindan 2011, 12. aya kadar train et
#start, 2012 ocak ayindan itibaren perform et
train <- window(sales, end = c(2011,12))
test <-  window(sales, start= c(2012,1))


#bir periyottan fazlasi için exponential smoothing iyi perform etmedi. bir periyot bu ornek için 1 ay demek.
#cunku sezonsalligi ve trendi göz ardi ettik.
mdl <- HoltWinters(train, beta = FALSE, gamma = FALSE)
ypred <- predict(mdl,12)
plot(mdl,ypred)


performance(test,ypred)



#sezonsallik olmasin trend olsun. BETAYI sildik, defaultta trendi ekledi.
#trendde en son negatif düsüs oldugundan, düsen bir trend çikti, kötü tahmin oldu
mdl <- HoltWinters(train,gamma = FALSE)
ypred <- predict(mdl,12)
plot(mdl,ypred)



performance(test,ypred)




#hem trendi hem de sezonsalligi isin içine kattik.
mdl <- HoltWinters(train)
ypred <- predict(mdl,12)
plot(mdl,ypred)


#performans sonuçlarina göre en iyi sonuçu sonuncu model üretti.
performance(test,ypred)




#machine learning metotlari ile tahmin yapmak istersek, regresyon kullanacagiz.
#sadece 1 önceki dataya bakarak bir sonrakini tahmin etmeye çalisacagiz.
#önce sales datasini zaman serisinden çikartacagiz.
#numeric vektöre dönüstürdük.
sales_vektor <- as.numeric(sales)


#input ve output belirtecegiz.

y <- sales_vektor[2:36]
x <- sales_vektor [1:35]

#machine learning için hep dat aframe kullanilir. simdi bunlari data frame koyuyoruz.

data <- data.frame(y,x)

#lineer regresyon yaptik
mdl <- lm(y ~ x, data = data )
ypred <-  predict(mdl,data)
performance (y,ypred)


#modeli çizdiriyoruz
plot(mdl)


#YENI ÖRNEK 
#store ve item bazinda tahminleme yapiyoruz. ikisi için ayni anda tahminleme çok zor, aggrigate bir tahminleme yapacagiz.

# train sales datasini import ettik
traindata <- read.csv('C:/R_data/train_sales.csv')
traindata

#1.yöntem: SQL ile gruplama
library(sqldf)
sqldf <- sqldf('select * from (select date,item,sum(sales) from traindata group by date,item) where item=1')
sqldf



#2. yöntem
sales <- read.csv('C:/R_data/train_sales.csv')
sales


#aggrigate bir tahminleme yapacagimiz için, gün ve SKU bazinda satislari toplattik.
#ve sadece ilk SKU özelinde tahminleme yapmak istedigimiziçin item=1 seçtik.
library (tidyverse)
sales %>%
    group_by (date,item)%>%
    summarise (sumSales = sum(sales))%>%
filter(item==1)-> sales_sum
view(sales_sum)


#grafigini çizdirdik.
sales_sum
ggplot(sales_sum) + geom_line(aes(x=as.Date(date),y=sumSales))


#Modelde istenen su:2013 ocak-kasim arasi data var elimde, aralik ayini tahminlemem
ts_sales <- ts(sales_sum,frequency = 365, start = c(2013,1,1))


ts_sales
plot(ts_sales[,3])

#bu modelde test ve train ayirmadik. eger birden fazla model çalistiriyor olsaydik ve içlerinden birini seçmeye çalisiyor olsaydik,
#testte en iyi sonucu vereni seçerdik.
train <- window(ts_sales)
test <-  window(ts_sales, start= c(2013,11,30))

mdl <- HoltWinters(ts_sales[,3])
ypred <- predict(mdl,30)
plot(mdl,ypred)


performance(ts_sales,mdl$fitted[,3])



######BUNU EVDE INCELE##########3
library(tidyverse)
sales <- read.csv("train_sales.csv")

sales %>%
  group_by(date,item) %>%
  summarise(sumSales = sum(sales)) %>%
  filter(item == 1) -> sales_sum


ggplot(sales_sum)+
  geom_line(aes(x = as.Date(date), y = sumSales))

shift_column <- function(x, shift = 30){
  data <- as.data.frame(x)
  data[,2] <- NaN
  data[(shift + 1):nrow(data),2] <- x[1:(nrow(data)-shift),1]
  return(data)
}
x1 <- shift_column(sales_sum[,3],shift = 30)
lag_feature <- function(x, lag = 3){
  data <- as.data.frame(x)
  for (i in 2:(lag+1)){
    data[,i] <- NaN
    data[i:nrow(data),i] <- x[1:(nrow(data)-i+1)]
  }
  return(data)
}
x2 <- lag_feature(x1[,2],5)
xx <- cbind(x1,x2)
xx <- xx[,-2]
mdl <- lm(sumSales ~ . ,data = xx)
summary(mdl)
ypred <- predict(mdl,xx)

library(randomForest)
xx <- na.omit(xx)
mdl <- randomForest(sumSales ~. , data = xx)
ypred <- predict(mdl,xx)


#hocayla sonradan yazdigim kisim, store ve item bazinda
data = data.frame()
for (i in 1:50){
  for (j in 1:10){
    sales %>% filter(item == i, store ==j) -> sales_sum
    xx <- ts(sales_sum[,5],frequency = 365)
    mdl <- HoltWinters(xx)
    ypred <- predict(mdl,30)
    data <-rbind(data,data.frame(item =i ,store = j,ypred))
    cat(i,"-",j,"\n")
  }
}

