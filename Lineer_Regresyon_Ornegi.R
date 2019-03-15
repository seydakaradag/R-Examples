

library(readxl)
HATCO <- read_excel("C:/R_data/HBAT.xlsx")
View(HATCO)


# Doðrusal Regresyon Modeli
attach(HATCO)
Hatco.all<- lm(x19~x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18) #degiskenler dosyasindan bunulari seçtik.
summary(Hatco.all) #isimize yaramayacak degiskenleri bulduk Anove ve t test ile.


# VIF
round(cov2cor(vcov(Hatco.all)), 2) 
#Multicollinarity testi yaptik ve x11,x17,x18 korelasyonu var çikti. denemek için once x11 i modelden çikartiyoruz.



# X11 cikardiktan sonra model Multicollineritye bakiyoruz.
Hatco.all<- lm(x19~x6+x7+x8+x9+x10+x12+x13+x14+x15+x16+x17+x18)
summary(Hatco.all)




# X8 cikardiktan sonra model Multicollineritye bakiyoruz.
Hatco.all<- lm(x19~x6+x7+x9+x10+x12+x13+x14+x15+x16+x17+x18)
summary(Hatco.all)


# X15 cikardiktan sonra model Multicollineritye bakiyoruz.
Hatco.all<- lm(x19~x6+x7+x9+x10+x12+x13+x14+x16+x17+x18)
summary(Hatco.all)
#x6,x7,x12 ve x18 degiskenleri ile modele devam etmeye karar verdik. 
#x9un yaninda bir tane nokta çikardi R. bu degiskeni de alabilirsin demek istiyor.
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 bize yüzdelik hata payi gösteriyor.
#mesela * 0.05 burada %5lik risk aliyoruz demek.



# Durbin Watson Test
library(lmtest, pos=24)
dwtest(Hatco.all, alternative="greater", data=HATCO)
dwtest(Hatco.all, alternative="two.sided", data=HATCO)
# hatalar arasinda otokorelasyon cikmadi.


library(MASS)
# create general linear model with stepwise 
stepwisemodel <- lm(x19~x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18) 
# stepwise ile hangi degiskenlerin elimine edilecegini buluyoruz. yukarida yaptigimizin aynisi aslinda.
step<-stepAIC(stepwisemodel,direction = "backward")
step$anova
#adim adim her sfeerinde bir degiskeni çikartarak sonuçlari gösterir.
#AIC testine göre en son x6,x9,x11,x12,x16 degiskenlerini kullanmaliyiz.

#kalan degiskenler modeli tekrar çalistiriyoruz.
Hatco.all<- lm(x19~x6+x9+x11+x12+x16)
summary(Hatco.all)


# Kategorik degisken ile doðrusal regresyon modeli
#x3 firma büyüklügü. bu datada kategorik. factor ile kategorik degiskeni sürekli degiskene çevirdik.
#Factor fonksiyonu ile kategorik degiskeni numeri yaptik.
x3<-factor(x3)
linearmodel19<-lm(x19~x3)
summary(linearmodel19)
t.test(x19~x3)


#3 degerli kategorik degisken için yapiyoruz.
x1<-factor(x1)
linearmodel19<-lm(x19~x1)
summary(linearmodel19)


# Anova
boxplot(x19~x1,col=rainbow(7))
model<-aov(x19~x1)
summary(model)
#buraya göre anova gruplardan birinde önemli bir fark oldugunu gösteriyor ama hangi grup soylemiyor bunu tukey ile anliyoruz.
TukeyHSD(model)
#tukeye göre 3 ve 2 arasinda fark yok. 2-1 arasinda fark var. 3-1 arasinda fark var.
#boxplotta da ayni farki görsel olarak gördük.


#----------MODELI CIZDIRIYORUZ---------------------#
Hatco.all<- lm(x19~x6+x9+x11+x12+x16)
plot(HATCO$x19, fitted.values(Hatco.all))
HATCO$x23<-fitted.values(Hatco.all)
View(HATCO)
#x23 bizim tahminimizi gösteriyor.
#korelasyona bakacagiz.
attach(HATCO)
cor(x19,x23)
cor.test(x19,x23)
#korelasyonun karesi R-squarei verir.



# create general linear model with interaction
linearmodelint <- lm(x19~x7:x8+x6+x9)#7 ve 8 anlamsizdi. bunlari çarparak tekrar modele sokuyoruz.
summary(linearmodelint)




mean(x19)
HATCO$binx19 <- ifelse (x19 >= (mean(x19),1,0)
                        attach(HATCO)
                        View (HATCO)









