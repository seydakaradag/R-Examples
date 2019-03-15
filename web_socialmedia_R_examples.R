

#Web ve sosyal medya analitigim için kullanilan ve Google APIsini kullanmaya yarayan paketleri kurduk ve libraryleri çagirdik.
library(googleAnalyticsR)
library(ggplot2)#grafik çizmek için kullanilan bir paket.
library(dplyr)

ga_auth() #bu fonksiyon ile Google ANalyticse baglaniyoruz.bu komutu çalistirinca bizi direk Google accountumuza yönlendiriyor ve bir
#kod generate ediyor. Bu generate ettigi kodu konsola yapistiriyoruz ve Token cahe file diye bir mesaj veriyor.

my_accounts <- ga_account_list()
View(my_accounts)
my_accounts$viewId
#Use my_accounts to find the viewId. Make sure to replace this with your viewId.
my_id



#Google APIden data çekmek için asagidaki kodu kullaniyoruz.
#Page View Query
df1 <- google_analytics(my_id, 
                        date_range = c("2013-12-10", "2017-02-07"),
                        metrics = c("sessions"),
                        dimensions = c("date"))


#graph sessions by date
ggplot(data=df1, aes(x=date, y=sessions)) +
geom_line(stat="identity")


#Page View Query
df2 <- google_analytics(my_id, 
                        date_range = c("2014-12-10", "2018-02-07"),
                        metrics = c("pageviews"),
                        dimensions = c("date"))


#add in year month columns to dataframe
df2$month<-format(df2$date,"%m")
df2$year<-format(df2$date,"%Y")

head(df2)

#include dplyr library to group data by month and year
library(dplyr)


#sessions by month by year using dplyr then graph using ggplot2 bar graph
df2 %>%
  group_by(year,month) %>%
  summarize(pageviews=sum(pageviews)) %>%
  #print table steps by month by year
  print (n=100) %>%
  #graph data by month by year
  ggplot(aes(x=month, y=pageviews, fill=year)) + 
  geom_bar(position='dodge', stat='identity')



#dünün tarihini bulma
yesterday <- Sys.Date()-1
yesterday



#Yeni bir data üzerinde örnek
MyData <- read.csv(file="C:/R_data/BounceRate_.csv",header=TRUE,sep = ",")
head(MyData)
View(MyData)
str(MyData)


#Bounce'u segment ile açiklayabiliyor muyuz bakmak için linear regression modeli uyguluyoruz.
#tek bir degisken ile bakiyoruz, simple linear regression
mod1 <- lm(formula = Bounce ~ Segment, data = MyData)


#Modelin anova çiktilarina bakiyoruz.
anova(mod1)
#anove test sonuçlarinda 3 yildiz cikti. Bu segment kategoriler arasinda anlamli bir fark oldugu anlamina geliyor.


mod1
#modeli çagirip sonuçlarina baktik.Her bir degiskenin katsayilari çikiyor.



ggplot(MyData, aes(x=Date,y = Bounce, fill=Segment, group=Segment)) + geom_bar(stat = "identity")



library(dplyr)
library(reshape)
library(cluster)

Product_views <- read.csv(" C:/R_data/Product_views.txt")
View(Product_views)
Product_trans <- read.csv(" c:/R_data/Product_trans.txt")
View(Product_trans)



library(dplyr)
library(reshape)
library(cluster)

Product_views$Product.SKU <- factor(Product_views$Product.SKU)
head(Product_views)
PV <- cast(Product_views, UserId ~ Product.SKU)
head(PV)
#cast fonksiyonu kullandik.


head(Product_trans)
pt <- Product_trans %>% select(Product.SKU, UserId)
head(pt)


model_data <- left_join(PV, pt)
View(model_data)
#LEFT JOIN fonksiyonu kullandik.iki datayi SQL'deki left join mantigi ile bagliyor.
#hangi alandan baglanmasi gerektigini yazabiliriz ancak yazmazsak kendisi de ortak alani anlayip baglar.


is.na(model_data$Product.SKU)
model_data$Product.SKU[is.na(model_data$Product.SKU)] <- "NoSale"



smp_size <- floor(0.75 * nrow(model_data))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(model_data)), size = smp_size)

## split the data
train <- model_data[train_ind, ]
test <- model_data[-train_ind, ]
## Bu veriler ile siniflandirma kodlari çalistirilabilir.



mydata1 <- model_data[2:11]
mydata1 <- scale(mydata1)

kmsil <- 0
sil <- 0
bss <- 0
tss <- 0
ratio <- 0
for (i in 2:10)
{  kms_r <- kmeans(mydata1, centers = i, nstart = 25)
bss[i] <- sum(kms_r$betweenss)
tss[i] <- sum(kms_r$totss)
ratio[i] <- bss[i]/tss[i]
kmsil = silhouette(kms_r$cluster, dist(mydata1))
sil[i] <- mean(kmsil[,3])   }

plot(1:10, ratio, type="b", xlab = "Number of Clusters", ylab = "Average B/T silhoutte")
plot(1:10, sil, type="b", xlab = "Number of Clusters", ylab = "Average B/T silhoutte")

fit <- kmeans(mydata1,4)
aggregate(mydata1,by=list(fit$cluster),FUN=mean)





#R Studio'nun sundugu özelliklerden biri olan Rpresentation dosyasinda, UTF-8 kullanilsa dahi, Türkçe karakter sorunu yasanabiliyor. Su linkte de bahsedildigi gibi bos bir R kodu eklemek sorununu çözebiliyor

#Link :
#https://www.aydinburak.net/single-post/2018/02/18/R-Studio-sunumlar%C4%B1nda-T%C3%BCrk%C3%A7e-karakter-sorunu


















