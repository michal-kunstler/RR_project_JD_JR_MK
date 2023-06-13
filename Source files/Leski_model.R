install.packages("corrplot")
install.packages("lmtest")
install.packages("car")
install.packages("VIF")
install.packages("tseries")
install.packages("stargazer")
install.packages("sandwich")
install.packages("viridis")
install.packages("gap")
library(gap)
library(readxl)
library(corrplot)
library(lmtest)
library(tseries)
library(stargazer)
library(sandwich)
library("viridis")

#wczytanie danych do modelu
getwd()
baza <- read_excel("baza_noworodki.xlsx")

#przegląd danych
head(baza,10)
summary(baza)

#badanie korelacji

korelacja <- cor(baza, use = "pairwise.complete.obs")
p.mat <- cor.mtest(baza)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
wykres_korelacji <- corrplot(korelacja, method="color", col=col(200), type="upper",
                             addCoef.col = "black", tl.col="black", tl.srt=45,
                             p.mat = p.mat$p, sig.level = 0.01, insig = "blank", diag=FALSE)
#widoczny brak korelacji zmiennej objasnianej ze zmiennymi populacja oraz labor_force, a same te zmienne
#są ze soba skorelowane (0,98), stad wniosek o wyrzuceniu zmiennej labor_force z modelu 
# wyrzucam także zmienna death_rate ze wzgledu na brak korelacji ze zmienną objaśnianą 

#przechodze do badania rozkladow zmiennych w modelu
hist(baza$ochrona_zdrowia) #do zastanowienia
hist(baza$GDPpc) #logarytm
hist(baza$GDP) #logarytm
hist(baza$populacja) #logarytm
hist(baza$noworodki) #logarytm
hist(baza$urban_pop) #raczej ok 
hist(baza$turysci) #logarytm
hist(baza$HIV) #ok, dyskretna

#zmienne o rozkladach skosnych zlogarytmuje i przetestuje na normalnosc
baza$l_ochrona_zdrowia <- log(baza$ochrona_zdrowia)
shapiro.test(baza$ochrona_zdrowia) #nie
shapiro.test(baza$l_ochrona_zdrowia) #poprawa


baza$l_noworodki <- log(baza$noworodki)
shapiro.test(baza$noworodki) #nie
shapiro.test(baza$l_noworodki) #nie
hist(baza$l_noworodki)

baza$l_GDP <- log(baza$GDP)
shapiro.test(baza$GDP) #nie
shapiro.test(baza$l_GDP) #tak

baza$l_GDPpc <- log(baza$GDPpc)
shapiro.test(baza$GDPpc) #nie
shapiro.test(baza$l_GDPpc) #poprawa

baza$l_populacja <- log(baza$populacja)
shapiro.test(baza$populacja) #nie
shapiro.test(baza$l_populacja) #ok



baza$l_urban_pop <- log(baza$urban_pop)
shapiro.test(baza$urban_pop)
shapiro.test(baza$l_urban_pop) # zecydowanie nie, szukam innych przeksztalcen
baza$up2 <- baza$urban_pop**2
baza$lup2 <- log(baza$up2)
hist(baza$lup2)
shapiro.test(baza$lup2) #niestety zadna z metod nie poprawia rozkladu normalnego, dlatego pozostane przy pierwotnej postaci


baza$l_turysci <- log(baza$turysci)
shapiro.test(baza$turysci) #nie
shapiro.test(baza$l_turysci) #prawie ok 

#po przeksztalceniach w bazie do modelu pozostawiam zmienne
baza = subset(baza,select=c(l_noworodki, l_ochrona_zdrowia, l_GDP, l_GDPpc, l_populacja,l_turysci,urban_pop, HIV ))
head(baza,10)

#histogramy zmiennych wchodzacych do modelu 
hist(baza$l_noworodki)
hist(baza$l_ochrona_zdrowia)
hist(baza$l_GDP)
hist(baza$l_GDPpc)
hist(baza$l_populacja)
hist(baza$l_turysci)
hist(baza$urban_pop) 
hist(baza$HIV)


#pierwsza postac modelu
model = lm( l_noworodki~ l_GDP + l_GDPpc + l_populacja + l_ochrona_zdrowia + l_turysci + urban_pop  + HIV, data = baza)
plot(model, which = 4)
summary(model)


baza1 = baza[-c(12,33,115),]
model1 = lm(l_noworodki ~ l_GDP + l_GDPpc + l_populacja + l_ochrona_zdrowia + l_turysci + urban_pop  + HIV, data = baza1)
plot(model1, which = 4)
summary(model1)

baza2 = baza1[-c(68,110,124),]
model2 = lm(l_noworodki ~ l_GDP + l_GDPpc + l_populacja + l_ochrona_zdrowia + l_turysci + urban_pop  + HIV, data = baza2)
plot(model2, which = 4)
summary(model2)


baza3 = baza2[-c(103,109,124),]
model3 = lm(l_noworodki ~ l_GDP + l_GDPpc + l_populacja + l_ochrona_zdrowia + l_turysci + urban_pop  + HIV, data = baza3)
plot(model3, which = 4)
summary(model3)



baza4 = baza3[-c(39,98),]
model4 = lm(l_noworodki ~ l_GDP + l_GDPpc + l_populacja + l_ochrona_zdrowia + l_turysci + urban_pop  + HIV, data = baza4)
plot(model4, which = 4)
summary(model4)


baza5 = baza4[-c(110),]
model5 = lm(l_noworodki ~ l_GDP + l_GDPpc + l_populacja + l_ochrona_zdrowia + l_turysci + urban_pop  + HIV, data = baza5)
plot(model5, which = 1)
plot(model5, which = 2)
plot(model5, which = 3)
plot(model5, which = 4)
plot(model5, which = 5)
summary(model5)



shapiro.test(baza5$l_ochrona_zdrowia)#ok
shapiro.test(baza5$l_GDP)#ok
shapiro.test(baza5$l_GDPpc)#nie
shapiro.test(baza5$l_populacja)#ok
shapiro.test(baza5$l_noworodki)#ok
shapiro.test(baza5$l_turysci)#ok
shapiro.test(baza5$urban_pop)
shapiro.test(baza5$death_rate) #nie
shapiro.test(baza5$HIV) #dyskretna



final1 = lm(l_noworodki ~ l_GDP + l_GDPpc + l_populacja + l_ochrona_zdrowia + l_turysci + urban_pop  + HIV, data = baza5)
summary(final1)

final2 = lm(l_noworodki ~ l_GDP + l_GDPpc + l_populacja + l_ochrona_zdrowia + l_turysci   + HIV, data = baza5)
summary(final2)

final3 = lm(l_noworodki ~  + l_GDPpc + l_populacja + l_ochrona_zdrowia + l_turysci  + HIV, data = baza5)
summary(final3)

#ostateczna wersja modelu 

#czas na testy 

resettest(final3, type = 'regressor')  #brak podstaw do odrzucenia H0 o prawidlowej formie funkcyjnej

plot(final3$residuals) 
hist(final3$residuals)
shapiro.test(final3$residuals) #ok 
jarque.bera.test(final3$residuals)#ok brak podstaw do odrzucenia H0 o normalnosci reszt
bptest(final3) #ok brak podstaw do odrzucenia H0 o homoskedastycznosci reszt
bgtest(final3)#ok brak podstaw do odrzucenia H0 o braku autokorelacji reszt


#publikacja

stargazer(final1, final2, final3,  type="text", dep.var.labels = c("smiertelnosc wsrod noworodkow"),
              covariate.labels = c("log GDP", "log GDP per capita","log populacji", 
                                  "log wydatkow panstwa na ochrone zdrowia",
                                  "log liczby turystow", "% ludzi mieszkajacych w miastach",
                                   "czy kraj ma probelm z HIV"),
                                    out="publikacja.html")

stargazer(final1,  type="text", dep.var.labels = c("smiertelnosc wsrod noworodkow"),
          covariate.labels = c("log GDP", "log GDP per capita","log populacji", 
                               "log wydatkow panstwa na ochrone zdrowia",
                               "log liczby turystow", "% ludzi mieszkajacych w miastach",
                               "czy kraj ma probelm z HIV"),
          out="publikacja.html")
