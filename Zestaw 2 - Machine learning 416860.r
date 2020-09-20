#Zestaw nr 2
# Dominik Dadaniak _ ZU - APD - I rok - 416 860

#otwieramy wyniki ankiety StackOverflow, zgodnie z poleceniem

#Pkt 1

>zestaw2 <- read.csv(file="C:/Users/xyq/Documents/Studia - APD - ZU/Uczenie maszynowe/Materiały/Zestaw 1/ankieta/survey_results_public.csv",header =TRUE)

#Wybieram pierwsze z brzegu 5 kolumn z danymi numerycznymi: YearsCode; Age1stCode; CompTotal; ConvertedComp; WorkWeekHrs


>zestaw2.df<-data.frame(zestaw2$YearsCode, zestaw2$Age1stCode, zestaw2$CompTotal, zestaw2$ConvertedComp, zestaw2$WorkWeekHrs)
 
 #Mamy naszą tabelkę <- Vide Z2_SS1 (screenshoty)
 
>library(dplyr)
 
>colnames (zestaw2.df) <- c ("YearsCode", "Age1stCode", "CompTotal", "ConvertedComp", "WorkWeekHrs")

#ponieważ mamy wartości "mniej niż 1 rok" zastąpie je na numeryczne - wartość "0", rozumianą jako ilość skończonych lat
 
>zestaw2.df$YearsCode <- ifelse(zestaw2.df$YearsCode == "Less than 1 year", "0", zestaw2.df$YearsCode)
 
 #pierwsze porządki za nami
 
 #ponieważ w Age1stCode mamy "Younger than 5 years" , stworzymy nową wartość - 4 , która będzie oznaczać 4 lub mniej skończonych lat
 
zestaw2.df$Age1stCode <- ifelse(zestaw2.df$Age1stCode == "Younger than 5 years", "4", zestaw2.df$Age1stCode)

# pozostaje nam tylko pozbyć się wierszy z NA

> zestaw2.df <- filter(zestaw2.df, zestaw2.df$YearsCode >= 0)
> zestaw2.df <- filter(zestaw2.df, zestaw2.df$Age1stCode >= 0)
> zestaw2.df <- filter(zestaw2.df, zestaw2.df$CompTotal >= 0)
> zestaw2.df <- filter(zestaw2.df, zestaw2.df$ConvertedComp >= 0)
> zestaw2.df <- filter(zestaw2.df, zestaw2.df$WorkWeekHrs >= 0)

#Die Ordnung muss sein! Alles klar jetzt <- Sehen Sie Z2_SS2, bitte. 

# no to teraz sobie zakładamy możliwe korelacje, zakładam, że ConvertedComp (y) jest zależne od CompTotal <- x1 oraz Age1stCode <- x2

>library(Hmisc)


>set.seed(1)

#nie ukrywam, że miałem kilka próbek i wybrałem najsilniejsze zauważone korelacje

>cor.test(as.numeric(zestaw2.df$ConvertedComp), as.numeric(zestaw2.df$CompTotal))

	Pearson's product-moment correlation

data:  as.numeric(zestaw2.df$ConvertedComp) and as.numeric(zestaw2.df$CompTotal)
t = 9.339, df = 55136, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.03140456 0.04807196
sample estimates:
       cor 
0.03974102 
'

> cor.test(as.numeric(zestaw2.df$ConvertedComp), as.numeric(zestaw2.df$CompTotal), method = "kendall")

	Kendall's rank correlation tau

data:  as.numeric(zestaw2.df$ConvertedComp) and as.numeric(zestaw2.df$CompTotal)
z = 128.84, p-value < 2.2e-16
alternative hypothesis: true tau is not equal to 0
sample estimates:
      tau 
0.367286 

'
#Uwaga Komputer może trochę mielić dane przy teście Kendall'a 

> cor.test(as.numeric(zestaw2.df$ConvertedComp), as.numeric(zestaw2.df$Age1stCode))

	Pearson's product-moment correlation

data:  as.numeric(zestaw2.df$ConvertedComp) and as.numeric(zestaw2.df$Age1stCode)
t = -8.9667, df = 55132, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 -0.04649303 -0.02982297
sample estimates:
        cor 
-0.03816065 
'
> cor.test(as.numeric(zestaw2.df$ConvertedComp), as.numeric(zestaw2.df$Age1stCode), method = "kendall")

	Kendall's rank correlation tau

data:  as.numeric(zestaw2.df$ConvertedComp) and as.numeric(zestaw2.df$Age1stCode)
z = -45.176, p-value < 2.2e-16
alternative hypothesis: true tau is not equal to 0
sample estimates:
       tau 
-0.1327038
'
# punkt 1 uważam za zrobiony, zmienna zależna i zmienne niezależne określone <- COMMIT

#Pkt2

#ad 1 - wybór zmiennej "tak" i "nie" i przekształcenie na 0 i 1

#bierzemy pierwszą kolumnę z brzegu - "Hobbyist"

> zestaw2 <- filter(zestaw2, zestaw2$YearsCode >= 0)
> zestaw2 <- filter(zestaw2, zestaw2$Age1stCode >= 0)
> zestaw2 <- filter(zestaw2, zestaw2$CompTotal >= 0)
> zestaw2 <- filter(zestaw2, zestaw2$ConvertedComp >= 0)
> zestaw2 <- filter(zestaw2, zestaw2$WorkWeekHrs >= 0)

# dla formalności wyrzucam niepotrzebne wiersze z pełnego zestawu, aby zachować zgodność w liczbie wierszy

>zestaw2.dfPrim<-cbind(zestaw2.df, zestaw2$Hobbyist)
library(plyr)

> zestaw2.dfPrim$`zestaw2$Hobbyist`<- mapvalues(zestaw2.dfPrim$`zestaw2$Hobbyist`, from = c("Yes","No"), to = c("1", "0"))  #<- Z2_SS3

#ad2 - wybór zmiennej jakościowej i przekształcenie za pomocą metody one-hot-encoding
# do One-Hot-Encoding wybieram kolumnę MgrIdiot

>zestaw2.dfPrim<-cbind(zestaw2.dfPrim, zestaw2$MgrIdiot)

library(caret)

dmy<-mapvalues(zestaw2$MgrIdiot, from = c(NA,"I don't have a manager"), to = c("0","I do not have a manager"))

#rzutuje brak odpowiedzi na 0 i usuwam apostrof

>survey <-data.frame(opinion=c('0','I do not have a manager','Not at all confident', 'Somewhat confident', 'Very confident'))

>survey <-data.frame(opinion=c('0','I do not have a manager','Not at all confident', 'Somewhat confident', 'Very confident'), rank=c(0,1,2,3,4))

>dmy2 <- dummyVars("rank ~ opinion", data = survey)

>trsf <- data.frame(predict(dmy2, newdata = survey))

> trsf
  opinion0 opinionI.do.not.have.a.manager opinionNot.at.all.confident opinionSomewhat.confident opinionVery.confident
1        1                              0                           0                         0                     0
2        0                              1                           0                         0                     0
3        0                              0                           1                         0                     0
4        0                              0                           0                         1                     0
5        0                              0                           0                         0                     1

#chyba załapałem o co chodzi, czas przenieść to na całość danych

>zestaw2.dfPrim$`zestaw2$MgrIdiot`<-mapvalues(zestaw2$MgrIdiot, from = c(NA,"I don't have a manager"), to = c("0","I do not have a manager"))

>zestaw2.F<-cbind(zestaw2.dfPrim, zestaw2$MgrIdiot)

>colnames (zestaw2.F) <- c ("YearsCode", "Age1stCode", "CompTotal", "ConvertedComp", "WorkWeekHrs","Hobbyist","MgrIdiot","MgrRank")

>zestaw2.F$MgrRank <- mapvalues(zestaw2.F$MgrRank, from = c(NA,"I don't have a manager","Not at all confident", "Somewhat confident", "Very confident"), to = c("0","1","2","3","4"))

> dmy4 <- dummyVars("MgrRank ~ MgrIdiot", data = zestaw2.F)
> trsf2 <- data.frame(predict(dmy4, newdata = zestaw2.F))

> zestaw2.F<-cbind(zestaw2.F, trsf2$MgrIdiot0)
> zestaw2.F<-cbind(zestaw2.F, trsf2$MgrIdiotI.do.not.have.a.manager)
> zestaw2.F<-cbind(zestaw2.F, trsf2$MgrIdiotNot.at.all.confident)
> zestaw2.F<-cbind(zestaw2.F, trsf2$MgrIdiotSomewhat.confident)
> zestaw2.F<-cbind(zestaw2.F, trsf2$MgrIdiotVery.confident)

>colnames (zestaw2.F) <- c ("YearsCode", "Age1stCode", "CompTotal", "ConvertedComp", "WorkWeekHrs","Hobbyist","MgrIdiot","MgrRank","Ans0","Ans1","Ans2","Ans3","Ans4")

#wiem, że powinienem zrobić to w 1 kroku przez złączenie tabel, ale przy ponad 50 tys. pozycjach funkcja merge trochę zajmuje czasu mojemu i7.... trochę za dużo, więc pododawałem kolumny po kolei, #otrzymana tabela stanowi Z2_SS4
# punkt nr 2 uważam za zakończony,

#Pkt 3 - Eliminacja wartości skrajnych

#najpierw przekształcę dotychczasowe charmy na inty, aby określić przedziały
>zestaw2.F$YearsCode<-as.numeric(zestaw2.F$YearsCode)
>zestaw2.F$Age1stCode<-as.numeric(zestaw2.F$Age1stCode)
>zestaw2.F$Hobbyist<-as.numeric(zestaw2.F$Hobbyist)
>zestaw2.F$MgrRank<-as.numeric(zestaw2.F$MgrRank)

#jakimś cudem wkradły mi się NA, więc wyczyszczę je
>library(janitor)
>library(dplyr)
>zestaw2.F<-zestaw2.F %>% na.omit #chyba najszybszy sposób na pozbycie się NA

> summary(zestaw2.F)
>YearsCode       Age1stCode      CompTotal         ConvertedComp      WorkWeekHrs         Hobbyist        MgrIdiot            MgrRank           Ans0       
 Min.   : 0.00   Min.   : 4.00   Min.   :0.000e+00   Min.   :      0   Min.   :   1.00   Min.   :0.0000   Length:55114       Min.   :0.000   Min.   :0.0000  
 1st Qu.: 6.00   1st Qu.:12.00   1st Qu.:2.000e+04   1st Qu.:  26124   1st Qu.:  40.00   1st Qu.:1.0000   Class :character   1st Qu.:2.000   1st Qu.:0.0000  
 Median :10.00   Median :15.00   Median :6.200e+04   Median :  57287   Median :  40.00   Median :1.0000   Mode  :character   Median :3.000   Median :0.0000  
 Mean   :12.86   Mean   :15.21   Mean   :3.629e+11   Mean   : 127149   Mean   :  41.97   Mean   :0.7924                      Mean   :2.723   Mean   :0.1441  
 3rd Qu.:17.00   3rd Qu.:18.00   3rd Qu.:1.200e+05   3rd Qu.: 100000   3rd Qu.:  43.00   3rd Qu.:1.0000                      3rd Qu.:4.000   3rd Qu.:0.0000  
 Max.   :50.00   Max.   :83.00   Max.   :1.000e+16   Max.   :2000000   Max.   :4850.00   Max.   :1.0000                      Max.   :4.000   Max.   :1.0000  
      Ans1              Ans2             Ans3             Ans4      
 Min.   :0.00000   Min.   :0.0000   Min.   :0.0000   Min.   :0.000  
 1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.000  
 Median :0.00000   Median :0.0000   Median :0.0000   Median :0.000  
 Mean   :0.02515   Mean   :0.1355   Mean   :0.3542   Mean   :0.341  
 3rd Qu.:0.00000   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:1.000  
 Max.   :1.00000   Max.   :1.0000   Max.   :1.0000   Max.   :1.000  

#żeby najlepiej zobrazować wartości odstające można np. posłużyć się wykresem pudełkowym, ja w celu zachowania oryginalnych danych z tymi wartościami stworzę drugą ramkę danych
#Patrz Z2_SS5

#kwantyle
>zestaw2.F <- filter(zestaw2.F, zestaw2.F$YearsCode <= 17)
>zestaw2.F <- filter(zestaw2.F, zestaw2.F$YearsCode >= 6)

#odchylenie standardowe
>sd(zestaw2.F$WorkWeekHrs)
[1] 39.05394
>zestaw2.F <- filter(zestaw2.F, zestaw2.F$WorkWeekHrs <= mean(zestaw2.F$WorkWeekHrs)+sd(zestaw2.F$WorkWeekHrs))
>zestaw2.F <- filter(zestaw2.F, zestaw2.F$WorkWeekHrs >= mean(zestaw2.F$WorkWeekHrs)-sd(zestaw2.F$WorkWeekHrs))

#w sumie to przed tymi operacjami jak usuwam wiersze powinienem zrobić sobię kopię zapasową oryginalnych danych, żeby móc do nich wrócić, nie robie tego tutaj ze względu na postawione jasno polecenia ćwiczenia,

#punkt 3 uważam za zakończony, zakres wierszy zmniejszył się do 28087 (Z2_SS6)

#Pkt4

#a - z wykorzystaniem jednej zmiennej (x1 lub x2),
>data2.plot<-data.frame(zestaw2.F$ConvertedComp,zestaw2.F$CompTotal)
>data2.lm<-lm(CompTotal~ConvertedComp,data=zestaw2.F),
>plot(data2.plot)
>abline(data2.lm, col='red', lwd=2, pch=20)

#jak widać mamy bardzo brzydki wykres, który prawie nic nie mówi - Z2_SS7, może 2 zmienne dadzą radę

#b - z wykorzystaniem obydwu zmiennych (x1 i x2), 

>data2Prim.lm<-lm(CompTotal~(ConvertedComp+Age1stCode),data=zestaw2.F)
> data2Prim.plot<-data.frame(zestaw2.F$CompTotal, zestaw2.F$ConvertedComp, zestaw2.F$Age1stCode)
> plot(data2Prim.plot)

#Trochę lepiej to wygląda, jeśli zrobimy sobie wykres dla regresji - Z2_SS8 i Z2_SS9

#c - z wykorzystaniem  zmiennych x1 i x2 oraz kolumn stworzonych w punkcie 2.

>data2Prim2.lm<-lm(CompTotal~(ConvertedComp+Age1stCode+Hobbyist+Ans0+Ans1+Ans2+Ans3+Ans4),data=zestaw2.F)
>plot(data2Prim2.lm)

#- Jeden  wykesów dla modelu - Z2_SS10

#Predykcja
>newdata = data.frame (Age1stCode <- 25, ConvertedComp <- 1234567, Hobbyist <- 1, Ans0 <-0 , Ans1 <-0 , Ans2 <-0, Ans3 <-0 , Ans4 <-1)

>predict(data2.lm, newdata)
      1 
3012552 

>predict(data2Prim.lm, newdata)
      1 
4337838 

>predict(data2Prim2.lm, newdata)
      1 
3999140 

#Jak widać wartości przewidywane z 2 i 3 modelu bardziej zbliżone niż w stosunku do pierwszego opartego tylko na 1 zmiennej

#Pozostało policzyć MSE

> summary(data2.lm)

Call:
lm(formula = CompTotal ~ ConvertedComp, data = zestaw2.F)

Residuals:
       Min         1Q     Median         3Q        Max 
  -4137795   -1193671   -1170340   -1143912 5697344537 

Coefficients:
               Estimate Std. Error t value Pr(>|t|)    
(Intercept)   1.133e+06  2.467e+05   4.593 4.39e-06 ***
ConvertedComp 1.522e+00  7.923e-01   1.921   0.0547 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 37830000 on 28085 degrees of freedom
Multiple R-squared:  0.0001314,	Adjusted R-squared:  9.584e-05 
F-statistic: 3.692 on 1 and 28085 DF,  p-value: 0.05468

>data2_sum<-summary(data2.lm)
> mean(data2_sum$residuals^2)
[1] 1.4312e+15   # MSE1


summary(data2Prim.lm)

Call:
lm(formula = CompTotal ~ (ConvertedComp + Age1stCode), data = zestaw2.F)

Residuals:
       Min         1Q     Median         3Q        Max 
  -6307620   -1501501   -1121512    -744668 5696425083 

Coefficients:
                Estimate Std. Error t value Pr(>|t|)  
(Intercept)   -8.984e+05  9.071e+05  -0.990   0.3220  
ConvertedComp  1.557e+00  7.923e-01   1.965   0.0494 *
Age1stCode     1.326e+05  5.696e+04   2.327   0.0200 *
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 37830000 on 28084 degrees of freedom
Multiple R-squared:  0.0003242,	Adjusted R-squared:  0.000253 
F-statistic: 4.554 on 2 and 28084 DF,  p-value: 0.01053

> data2Prim_sum<-summary(data2Prim.lm)
mean(data2Prim_sum$residuals^2)
[1] 1.430924e+15 #MSE2

summary(data2Prim2.lm)

Call:
lm(formula = CompTotal ~ (ConvertedComp + Age1stCode + Hobbyist + 
    Ans0 + Ans1 + Ans2 + Ans3 + Ans4), data = zestaw2.F)

Residuals:
       Min         1Q     Median         3Q        Max 
  -7033346   -1602339   -1083030    -607282 5696087729 

Coefficients: (1 not defined because of singularities)
                Estimate Std. Error t value Pr(>|t|)  
(Intercept)   -1.448e+06  1.099e+06  -1.318   0.1876  
ConvertedComp  1.570e+00  7.931e-01   1.980   0.0477 *
Age1stCode     1.343e+05  5.732e+04   2.343   0.0192 *
Hobbyist       1.519e+05  5.618e+05   0.270   0.7869  
Ans0           1.250e+06  7.861e+05   1.590   0.1118  
Ans1          -2.955e+05  1.526e+06  -0.194   0.8464  
Ans2           1.192e+05  7.109e+05   0.168   0.8669  
Ans3           6.843e+05  5.299e+05   1.291   0.1966  
Ans4                  NA         NA      NA       NA  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 37830000 on 28079 degrees of freedom
Multiple R-squared:  0.0004576,	Adjusted R-squared:  0.0002084 
F-statistic: 1.837 on 7 and 28079 DF,  p-value: 0.07575

data2Prim2_sum<-summary(data2Prim2.lm)
> mean(data2Prim2_sum$residuals^2)
[1] 1.430734e+15  #MSE3


#Pkt 5
#chyba zostawię tak jak jest, mam nadzieję, że napisane w miarę przejrzyście, z PEP8 nie porównam bo to nie python, MSE obliczają się automatycznie, co do wykresów można użyć biblioteki ggplot i rozbudowywać skrypt o funkcje prezentacji danych (kolory, siatka układu współrzędnych, tutuły etc.) udanej lektury!

