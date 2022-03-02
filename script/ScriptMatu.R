#library
library(readxl)
#install.packages("dplyr")
library(dplyr)
#install.packages("tidyr")
library(tidyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("hablar")
library(hablar)
#install.packages("stringr")
library(stringr)
#install.packages("forcats")
library("forcats")
#install.packages("plotrix")
library("plotrix")


DataMatu <- readRDS("C:/Users/vmartin/Desktop/Stage/R/Maturity/RprojMaturity/Data/ca.rds")

#------------------TRI DES DONNEES ET TRAVAIL EXPLORATOIRE----------------------

#Filter l'espece + Area 
DataMatu_MU <- DataMatu %>% 
  filter(spp == "Mullus surmuletus") 

#Barplot sexe
ggplot(DataMatu_MU, mapping = aes(x = sex)) + geom_bar()

#Tableau age
table(DataMatu_MU$age)

#Barplot echelles de maturite
ggplot(DataMatu_MU, mapping = aes(x = matScale)) + geom_bar()

#Barplot stades de maturite
ggplot(DataMatu_MU, mapping = aes(x = matStage)) + geom_bar()

#Barplot  area
ggplot(DataMatu_MU, mapping = aes(x = area)) + geom_bar()
table(DataMatu_MU$year,DataMatu_MU$area)


#Quelques données aberrantes dans les valeurs de longeur !!!! Au delà de 500 mm 
#Concentre sur l'année 2010, zone 8.a (23/159)
hist(DataMatu_MU$lenCls)
sd(DataMatu_MU$lenCls)
mean(DataMatu_MU$lenCls)
quantile(DataMatu_MU$lenCls,0.999) #=> 378
Probleme <- DataMatu_MU %>%
  filter(lenCls > 500) 
#Il vaut mieux filtrer au dela de 500 mm

DataMatu_MUR <- DataMatu_MU %>% 
 # filter(str_detect(area, "^27.")) %>%  
  filter(lenCls <= 500)%>%
  filter(lenCls !="-1")%>%
  filter(lenCls !="")%>%
  filter(matStage !="-1")%>%   
  filter(matStage != "0")%>%    
  filter(matStage != "5")%>%
  filter(matStage !="")%>% 
  filter(age !="-1")%>%  
  filter(sex != "-1")%>%
  filter(sex != "NON SEXE")%>%
  mutate(newarea = fct_collapse (area, "8a-b" =  c("27.8.a", "27.8.b","27.8.a,27.8.b"), 
                                 "7d-e" =  c("27.7.d", "27.7.d,27.7.e", "27.7.e"), 
                                 "7f-h et j " = c("27.7.g,27.7.h,27.7.j", "27.7.h", "27.7.f", 
                                                      "27.7.g", "27.7.j"), 
                                 "8c-d" =  c("27.8.c", "27.8.d"), 
                                 "4b-c" =  c("27.4.b", "27.4.c")))

ggplot(DataMatu_MUR, mapping = aes(x = newarea)) + geom_bar()
#Pas mal d'info pour 7.d-e, 8.a-b et 4.c-b #on va se concentrer dessus 

#Quelques tableaux 
table(DataMatu_MUR$matStage,DataMatu_MUR$year)
table(DataMatu_MUR$matScale,DataMatu_MUR$year)
# => Plus de precisions dans les echelles  de maturite (1-7)a partir de 2015

table(DataMatu_MUR$year,DataMatu_MUR$newarea)
table(DataMatu_MUR$year,DataMatu_MUR$lenCls)
View(table(DataMatu_MUR$newarea))


#---EVOLUTION DES STADES DE MATURITE DANS LE TEMPS ET EN FONCTION DES ZONES-----

#-------------------------------Femelle-----------------------------------------
#8.a et 8.b
ggplot(DataMatu_MUR %>% 
         filter(sex== "F") %>%
         filter(newarea =="8a-b"), mapping = aes(x = matStage)) +
  geom_bar()+
  facet_wrap(~year)

#7.d et e
ggplot(DataMatu_MUR %>% 
         filter(sex== "F") %>%
         filter(newarea =="7d-e"), mapping = aes(x = matStage)) +
  geom_bar()+
  facet_wrap(~year)

#4.b et c
ggplot(DataMatu_MUR %>% 
         filter(sex== "F") %>%
         filter(newarea =="4b-c"), mapping = aes(x = matStage)) +
  geom_bar()+
  facet_wrap(~year)

#------------------------------------Male---------------------------------------
#8.a et 8.b
ggplot(DataMatu_MUR %>% 
         filter(sex== "M") %>%
         filter(newarea =="8a-b"), mapping = aes(x = matStage)) +
  geom_bar()+
  facet_wrap(~year)

#7.d et e
ggplot(DataMatu_MUR %>% 
         filter(sex== "M") %>%
         filter(newarea =="7d-e"), mapping = aes(x = matStage)) +
  geom_bar()+
  facet_wrap(~year)

#4.c et b
ggplot(DataMatu_MUR %>% 
         filter(sex== "M") %>%
         filter(area =="4b-c"), mapping = aes(x = matStage)) +
  geom_bar()+
  facet_wrap(~year)


#-----------------En simplifiant les stades de maturite-------------------------

DataMatu_MUR2 <- DataMatu_MUR %>% 
  mutate(mat=ifelse(sex == "I","Immature",
                    ifelse (matStage == "1", "Immature",
                            ifelse (matStage == "2", "Immature",
                                    ifelse (matStage == "2a", "Immature", "Mature")))))
#Tableaux stade de maturite
table(DataMatu_MUR2$mat)
table(DataMatu_MUR2$mat,DataMatu_MUR2$year)


pb3 <-select(DataMatu_MUR2, sex, mat, lenCls, matStage)
pb4 <- pb3 %>%
  filter(sex=="I")
#-------------------------------Femelle-----------------------------------------
#8.a et 8.b
ggplot(DataMatu_MUR2 %>% 
         filter(sex == "F") %>%
         filter(newarea == "8a-b"), mapping = aes(x = mat)) +
  geom_bar()+
  facet_wrap(~year)

#7.d et e
ggplot(DataMatu_MUR2 %>% 
         filter(sex== "F") %>%
         filter(newarea == "7d-e"), mapping = aes(x = mat)) +
  geom_bar()+
  facet_wrap(~year)

#4.b et c
ggplot(DataMatu_MUR2 %>% 
         filter(sex== "F") %>%
         filter(newarea =="4b-c"), mapping = aes(x = mat)) +
  geom_bar()+
  facet_wrap(~year)

#---------------------------------Male------------------------------------------

#8.a et 8.b
ggplot(DataMatu_MUR2 %>% 
         filter(sex == "M") %>%
         filter(newarea == "8a-b"), mapping = aes(x = mat)) +
  geom_bar()+
  facet_wrap(~year)

#7.d et e
ggplot(DataMatu_MUR2 %>% 
         filter(sex== "M") %>%
         filter(newarea == "7d-e"), mapping = aes(x = mat)) +
  geom_bar()+
  facet_wrap(~year)

#4.b et c
ggplot(DataMatu_MUR2 %>% 
         filter(sex== "M") %>%
         filter(newarea =="4b-c"), mapping = aes(x = mat)) +
  geom_bar()+
  facet_wrap(~year)

#-----------EVOLUTION LONGUEUR EN FONCTION DU TEMPS MAT/IMMATURE---------------- 
#--------------------------------Femelle----------------------------------------

#Boxplot 
ggplot(DataMatu_MUR2 %>% 
         filter(sex== "F"), aes(x = factor(year), y = lenCls)) +
  geom_boxplot() + 
  facet_wrap(~mat)

#Toutes zones geographiques separees
ggplot(DataMatu_MUR2%>%
         filter(sex== "F") %>%
         filter(newarea %in% c("8a-b" , "7d-e" , "4b-c"))
       ,aes(x=lenCls,y=newarea,color=mat))+ #Couleur selon les stades de maturite
  geom_point()+
  facet_wrap(~year)

ggplot(DataMatu_MUR2%>%
         filter(sex== "F") %>%
         filter(newarea %in% c("8a-b" , "7d-e" , "4b-c"))
       ,aes(x=lenCls,y=mat,color=newarea))+     #Couleur selon les zones 
  geom_point()+
  facet_wrap(~year)

#8.a et 8.b
ggplot(DataMatu_MUR2%>%
         filter(sex== "F") %>%
         filter(newarea =="8a-b")
       ,aes(x=lenCls,y=mat,color=mat))+
  geom_point()+
  facet_wrap(~year)


#7.d et e
ggplot(DataMatu_MUR2 %>% 
         filter(sex== "F") %>%
         filter(newarea == "7d-e")
       ,aes(x=lenCls,y=mat,color=mat))+
  geom_point()+
  facet_wrap(~year)

#4.b et c 
ggplot(DataMatu_MUR2%>%
         filter(sex== "F") %>%
         filter(newarea =="4b-c")
       ,aes(x=lenCls,y=mat,color=mat))+
  geom_point()+
  facet_wrap(~year)

#---------------------------------Male------------------------------------------

#Bopxplot
ggplot(DataMatu_MUR2 %>% 
         filter(sex== "M"), aes(x = factor(year), y = lenCls)) +
  geom_boxplot() + 
  facet_wrap(~mat)

#Toutes zones geographiques séparees
ggplot(DataMatu_MUR2%>%
         filter(sex== "M") %>%
         filter(newarea %in% c("8a-b" , "7d-e" , "4b-c"))
       ,aes(x=lenCls,y=newarea,color=mat))+ #Couleur selon les stades de maturite
  geom_point()+
  facet_wrap(~year)

ggplot(DataMatu_MUR2%>%
         filter(sex== "M") %>%
         filter(newarea %in% c("8a-b" , "7d-e" , "4b-c"))
       ,aes(x=lenCls,y=mat,color=newarea))+ #Couleur selon les zones
  geom_point()+
  facet_wrap(~year)

#8.a et 8.b
ggplot(DataMatu_MUR2%>%
         filter(sex== "M") %>%
         filter(newarea =="8a-b")
       ,aes(x=lenCls,y=mat,color=mat))+
  geom_point()+
  facet_wrap(~year)

#7.d et e
ggplot(DataMatu_MUR2 %>% 
         filter(sex== "M") %>%
         filter(newarea == "7d-e")
       ,aes(x=lenCls,y=mat,color=mat))+
  geom_point()+
  facet_wrap(~year)

#4.b et c
ggplot(DataMatu_MUR2%>%
         filter(sex== "M") %>%
         filter(newarea =="4b-c")
       ,aes(x=lenCls,y=mat,color=mat))+
  geom_point()+
  facet_wrap(~year)


#--------------------NUAGE DE POINTS/ SERIE TEMPORELLE ------------------------- 

#----------Graphiques: serie temporelle en fonction des diverses zones----------

#-----------------------------Femelle-------------------------------------------

#----------------Calcul des moyennes et l'erreur standard----------------------- 
#------en fonction de la maturite et de l'annee (mais pas de la zone)-----------

DataMatu_MUR_F_ST <- DataMatu_MUR2 %>%
  filter(sex == "F") %>%
  group_by(mat, year, .add = TRUE) %>% 
  mutate(meanLen = mean (lenCls)) %>% 
  mutate(stdErrorLen = std.error(lenCls))

table(DataMatu_MUR_F_ST$stdErrorLen)
table(DataMatu_MUR_F_ST$stdErrorLen, DataMatu_MUR_F_ST$year)
table(DataMatu_MUR_F_ST$meanLen)
table(DataMatu_MUR_F_ST$meanLen, DataMatu_MUR_F_ST$year)

#Dans le monde des statistiques, l’erreur-type de moyenne est un terme tres utile 
#et important. Il nous indique comment l’echantillon s’ecarte de la moyenne reelle,
#contrairement a l’ecart-type, qui est une mesure du degre de dispersion des donnees.

#Toutes zones geographiques confondues
ggplot(data = DataMatu_MUR_F_ST, 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() + 
  geom_line() +
  geom_smooth() +
  geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5)+
  labs(title = "Evolution de la moyenne de la longeur totale pour les \n individus femelles matures et immatures entre 2006 et 2020",
       subtitle = "Toutes zones geographiques de peche proposees confondues",
       caption = "Barres d'erreur: erreur standard",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#---------Calcul des moyennes, medianes et erreurs standard des longueurs-------
#-----------en fonction de l'aire, de la maturite et de l'annee-----------------
DataMatu_MUR_F_ST_2 <- DataMatu_MUR2 %>%
  filter(sex == "F") %>%
  group_by(newarea, mat, year, .add = TRUE) %>% 
  mutate(meanLen = mean (lenCls))%>% 
  mutate(medianLen = median (lenCls))%>% 
  mutate(stdErrorLen = std.error(lenCls))

table(DataMatu_MUR_F_ST_2$meanLen)
table(DataMatu_MUR_F_ST_2$meanLen, DataMatu_MUR_F_ST_2$year)

table(DataMatu_MUR_F_ST_2$medianLen)
table(DataMatu_MUR_F_ST_2$medianLen, DataMatu_MUR_F_ST_2$year)

table(DataMatu_MUR_F_ST_2$stdErrorLen)
table(DataMatu_MUR_F_ST_2$stdErrorLen, DataMatu_MUR_F_ST_2$year)


#------------------Avec la moyenne avec ou non l'erreur standard----------------

#Toutes zones geographique separees
ggplot(data = DataMatu_MUR_F_ST_2, 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() + 
  facet_wrap(~newarea)+
  geom_line() +  
  geom_smooth()

#Toutes zones geographique separees (+ erreur standard) 
ggplot(data = DataMatu_MUR_F_ST_2, 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() + 
  facet_wrap(~newarea)+
  geom_line() +
  geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5)+
  labs(title = "Evolution de la moyenne de la longeur totale pour les \n individus femelles matures et immatures entre 2006 et 2020",
       subtitle = "Toutes zones geographiques de peche proposees",
       caption = "Barres d'erreur: erreur standard",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#8.a et 8.b
ggplot(data = DataMatu_MUR_F_ST_2 %>%
         filter(newarea == "8a-b"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()

#8.a et 8.b (+ erreur standard)
ggplot(data = DataMatu_MUR_F_ST_2 %>%
         filter(newarea == "8a-b"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()+
  geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5) +
  labs(title = "Evolution de la moyenne de la longeur totale pour les \n individus femelles matures et immatures entre 2006 et 2020",
       subtitle = "Zones geographiques de peche : 8.a et 8.b",
       caption = "Barres d'erreur: erreur standard",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#7.d et e
ggplot(data = DataMatu_MUR_F_ST_2 %>%
         filter(newarea == "7d-e"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()

#7.d et e (+ erreur standard)
ggplot(data = DataMatu_MUR_F_ST_2 %>%
         filter(newarea == "7d-e"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()+
  geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5)+
  labs(title = "Evolution de la moyenne de la longeur totale pour les \n individus femelles matures et immatures entre 2006 et 2020",
       subtitle = "Zones geographiques de peche : 7.d et e",
       caption = "Barres d'erreur: erreur standard",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu") 

#4.b et c
ggplot(data = DataMatu_MUR_F_ST_2 %>%
         filter(newarea =="4b-c"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()

#4.b et c (+ erreur standard)
ggplot(data = DataMatu_MUR_F_ST_2 %>%
         filter(newarea =="4b-c"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth() +
  geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5) +
  labs(title = "Evolution de la moyenne de la longeur totale pour les \n individus femelles matures et immatures entre 2006 et 2020",
       subtitle = "Zone geographique de peche : 4.c et b",
       caption = "Barres d'erreur: erreur standard",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu") 

#-----------------------Avec la mediane ----------------------------------------
#Toutes zones geographique separees
ggplot(data = DataMatu_MUR_F_ST_2, 
       mapping = aes(x = year, y = medianLen, color=mat)) + 
  geom_point() + 
  facet_wrap(~newarea)+
  geom_line()


#8.a et 8.b
ggplot(data = DataMatu_MUR_F_ST_2 %>%
         filter(newarea == "8a-b"), 
       mapping = aes(x = year, y = medianLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()

#7.d et e
ggplot(data = DataMatu_MUR_F_ST_2 %>%
         filter(newarea == "7d-e"), 
       mapping = aes(x = year, y = medianLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()

#4.b et c
ggplot(data = DataMatu_MUR_F_ST_2 %>%
         filter(newarea =="4b-c"), 
       mapping = aes(x = year, y = medianLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()


#-------------------------------Male--------------------------------------------

#----------------Calcul des moyennes et l'erreur standard----------------------- 
#------en fonction de la maturite et de l'annee (mais pas de la zone)-----------

DataMatu_MUR_M_ST <- DataMatu_MUR2 %>%
  filter(sex == "M") %>%
  group_by(mat, year, .add = TRUE) %>% 
  mutate(meanLen = mean (lenCls)) %>% 
  mutate(stdErrorLen = std.error(lenCls))

table(DataMatu_MUR_M_ST$stdErrorLen)
table(DataMatu_MUR_M_ST$stdErrorLen, DataMatu_MUR_M_ST$year)

table(DataMatu_MUR_M_ST$meanLen)
table(DataMatu_MUR_M_ST$meanLen, DataMatu_MUR_M_ST$year)

#Toutes zones geographiques confondues
ggplot(data = DataMatu_MUR_M_ST, 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() + 
  geom_line() +
  geom_smooth() +
  geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5)+
  labs(title = "Evolution de la moyenne de la longeur totale pour les \n individus males matures et immatures entre 2006 et 2020",
       subtitle = "Toutes zones geographiques de peche proposees confondues",
       caption = "Barres d'erreur: erreur standard",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#---------Calcul des moyennes, mediane et erreur standard des longueurs---------
#-----------en fonction de l'aire, de la maturite et de l'annee-----------------

DataMatu_MUR_M_ST <- DataMatu_MUR2 %>%
  filter(sex == "M") %>%
  group_by(newarea, mat, year, .add = TRUE) %>% 
  mutate(meanLen = mean (lenCls))%>% 
  mutate(medianLen = median (lenCls))

table(DataMatu_MUR_M_ST$meanLen)
table(DataMatu_MUR_M_ST$meanLen, DataMatu_MUR_M_ST$year)

table(DataMatu_MUR_M_ST$medianLen)
table(DataMatu_MUR_M_ST$medianLen, DataMatu_MUR_M_ST$year)

#-------------------Avec la moyenne avec ou non l'erreur standard---------------

#Toutes zones geographique separees
ggplot(data = DataMatu_MUR_M_ST, 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() + 
  facet_wrap(~newarea)+
  geom_line() +  
  geom_smooth()

#Toutes zones geographique separees (+erreur standard)
ggplot(data = DataMatu_MUR_M_ST, 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() + 
  facet_wrap(~newarea)+
  geom_line() +
  geom_smooth() +
  geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5)+
  labs(title = "Evolution de la moyenne de la longeur totale pour les \n individus males matures et immatures entre 2006 et 2020",
       subtitle = "Toutes zones geographiques de peche proposees",
       caption = "Barres d'erreur: erreur standard",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#8.a et 8.b
ggplot(data = DataMatu_MUR_M_ST %>%
         filter(newarea == "8a-b"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()

#8.a et 8.b (+erreur standard)
ggplot(data = DataMatu_MUR_M_ST %>%
         filter(newarea == "8a-b"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()+
  geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5) +
  labs(title = "Evolution de la moyenne de la longeur totale pour les \n individus males matures et immatures entre 2006 et 2020",
       subtitle = "Zones geographiques de peche : 8.a et 8.b",
       caption = "Barres d'erreur: erreur standard",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#7.d et e
ggplot(data = DataMatu_MUR_M_ST %>%
         filter(newarea == "7d-e"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()

#7.d et e (+erreur standard)
ggplot(data = DataMatu_MUR_M_ST %>%
         filter(newarea == "7d-e"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()+
  geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5)+
  labs(title = "Evolution de la moyenne de la longeur totale pour les \n individus males matures et immatures entre 2006 et 2020",
       subtitle = "Zones geographiques de peche : 7.d et e",
       caption = "Barres d'erreur: erreur standard",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu") 

#4.b et c
ggplot(data = DataMatu_MUR_M_ST %>%
         filter(newarea =="4b-c"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()


#4.c et b (+erreur standard)
ggplot(data = DataMatu_MUR_M_ST %>%
         filter(newarea =="4b-c"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth() +
  geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5) +
  labs(title = "Evolution de la moyenne de la longeur totale pour les \n individus males matures et immatures entre 2006 et 2020",
       subtitle = "Zone geographique de peche : 4.c et b",
       caption = "Barres d'erreur: erreur standard",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu") 

#--------------------------Avec la mediane--------------------------------------

#Toutes zones geographique separees
ggplot(data = DataMatu_MUR_M_ST, 
       mapping = aes(x = year, y = medianLen, color=mat)) + 
  geom_point() + 
  facet_wrap(~newarea)+
  geom_line()

#8.a et 8.b
ggplot(data = DataMatu_MUR_M_ST %>%
         filter(newarea == "8a-b"), 
       mapping = aes(x = year, y = medianLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()

#7.d et e 
ggplot(data = DataMatu_MUR_M_ST %>%
         filter(newarea == "7d-e"), 
       mapping = aes(x = year, y = medianLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()

#4.c et b
ggplot(data = DataMatu_MUR_M_ST %>%
         filter(newarea =="4b-c"), 
       mapping = aes(x = year, y = medianLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()



#---------------------------CALCUL DES L50--------------------------------------

#install.packages("sizeMat")
library(sizeMat)
#install.packages("questionr")
library("questionr")


#------------Tri des donnees pour rendre le jeu de donnees plus digerable------- 
Data_MUR_L50 <- DataMatu_MUR2 %>%
  mutate(mat=ifelse(mat == "Immature", "0","1"))%>%  
  select(lenCls, mat, sex, year, newarea)%>% 
  rename.variable("lenCls", "length")%>%
  rename.variable("mat", "mature")%>% 
  drop_na()

table(Data_MUR_L50$newarea)
table(Data_MUR_L50$year)

#----------------------------------Femelle--------------------------------------
#-------Graphique de la proportion de mature/immature et valeur L50------------- 
#------------------------en fonction des zones---------------------------------- 

#Toutes zones confondues 
L50_F<- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  mutate(mature=as.factor(mature)) 
rL50_F<-fl50(L50_F,niter=100,graph=T)
a <- median(rL50_F$L50)

#zone 8a-b 
L50_F_8ab <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_8ab<-fl50(L50_F_8ab,niter=100,graph=T)
b <- median(rL50_F_8ab$L50)

#zone 7d-e 
L50_F_7de <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(newarea=="7d-e")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_7de<-fl50(L50_F_7de,niter=100,graph=T)
c <- median(rL50_F_7de$L50)

#zone 4b-c 
L50_F_4bc <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(newarea=="4b-c")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_4bc<-fl50(L50_F_4bc,niter=100,graph=T)
d <- median(rL50_F_4bc$L50)

#-------Graphique de la proportion de mature/immature et valeur L50------------- 
#---------------------pour chaque année 2006-2020-------------------------------

#2006
L50_F_2006 <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2006")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2006<-fl50(L50_F_2006,niter=100,graph=T)
rL50_F_2006$L50
e <- median(rL50_F_2006$L50)

#2007              
L50_F_2007 <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2007")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2007<-fl50(L50_F_2007,niter=100,graph=T)
rL50_F_2007$L50
f <- median(rL50_F_2007$L50)   #ne fonctionne pas, pas assez d'individus mature 

#2008
L50_F_2008 <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2008")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2008<-fl50(L50_F_2008,niter=100,graph=T)
rL50_F_2008$L50
g <- median(rL50_F_2008$L50)

#2009                             #ne fonctionne pas très bien, valeur eleve 382
L50_F_2009 <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2009")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2009<-fl50(L50_F_2009,niter=100,graph=T)
rL50_F_2009$L50
h <- median(rL50_F_2009$L50)

#2010                        
L50_F_2010 <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2010")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2010<-fl50(L50_F_2010,niter=100,graph=T)
rL50_F_2010$L50
i <- median(rL50_F_2010$L50)

#2011
L50_F_2011 <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2011")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2011<-fl50(L50_F_2011,niter=100,graph=T)
rL50_F_2011$L50
j <- median(rL50_F_2011$L50)

#2012
L50_F_2012 <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2012")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2012<-fl50(L50_F_2012,niter=100,graph=T)
rL50_F_2012$L50
k <- median(rL50_F_2012$L50)

#2013
L50_F_2013 <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2013")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2013<-fl50(L50_F_2013,niter=100,graph=T)
rL50_F_2013$L50
l <- median(rL50_F_2013$L50)

#2014                             #Que des individus immatures  
L50_F_2014 <- Data_MUR_L50%>%
  filter(sex=="F"  | sex =="I")%>%
  filter(year=="2014")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2014<-fl50(L50_F_2014,niter=100,graph=T)
rL50_F_2014$L50
m <- median(rL50_F_2014$L50)

#pb2 <- Data_MUR_L50%>%
#filter(sex=="F")%>%
#filter(year=="2014")

#2015                         #Pas assez d'individus matures 
L50_F_2015 <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2015")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2015<-fl50(L50_F_2015,niter=100,graph=T)
rL50_F_2015$L50
n <- median(rL50_F_2015$L50)

#2016
L50_F_2016 <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2016")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2016<-fl50(L50_F_2016,niter=100,graph=T)
rL50_F_2016$L50
o <- median(rL50_F_2016$L50)

#2017
L50_F_2017 <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2017")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2017<-fl50(L50_F_2017,niter=100,graph=T)
rL50_F_2017$L50
p <- median(rL50_F_2017$L50)

#2018
L50_F_2018 <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2018")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2018<-fl50(L50_F_2018,niter=100,graph=T)
rL50_F_2018$L50
q <- median(rL50_F_2018$L50)

#2019
L50_F_2019 <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2019")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2019<-fl50(L50_F_2019,niter=100,graph=T)
rL50_F_2019$L50
r <- median(rL50_F_2019$L50)

#2020          ##ne fonctionne pas très bien, valeur eleve 495
L50_F_2020 <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2020")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2020<-fl50(L50_F_2020,niter=100,graph=T)
rL50_F_2020$L50
s <- median(rL50_F_2020$L50)

#------------Tableau et graphiques de la L50 en fonction des zones--------------- 
area <- c("Toutes zones confondues", "zone 8a-b", "zone 7d-e", "zone 4b-c")
L50area_F <- c(a, b, c, d)
tabL50area_F <- data.frame(area, L50area_F)
tabL50area_F 

ggplot(tabL50area_F ,aes(x=area,y=L50area_F))+
  geom_point()

#------------Tableau et graphiques de la L50 en fonction du temps---------------- 
year <- (2006:2020)
L50year_F <- c(e, "", g, h, i, j, k, l, "", "", o, p, q, r, s)
tabL50year_F <- data.frame(year, L50year_F )
tabL50year_F

plot(tabL50year_F)

ggplot(tabL50year_F,aes(x=year,y=L50year_F))+
  geom_point()

#------------------------------------Male---------------------------------------

#-------Graphique de la proportion de mature/immature et valeur L50------------- 
#------------------------en fonction des zones---------------------------------- 

#Toutes zones confondues 
L50_M<- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  mutate(mature=as.factor(mature)) 
rL50_M<-fl50(L50_M,niter=100,graph=T)
a1 <- median(rL50_M$L50)

#zone 8a-b 
L50_M_8ab <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_8ab<-fl50(L50_M_8ab,niter=100,graph=T)
b1 <- median(rL50_M_8ab$L50)

#zone 7d-e 
L50_M_7de <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(newarea=="7d-e")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_7de<-fl50(L50_M_7de,niter=100,graph=T)
c1 <- median(rL50_M_7de$L50)

#zone 4b-c 
L50_M_4bc <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(newarea=="4b-c")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_4bc<-fl50(L50_M_4bc,niter=100,graph=T)
d1 <- median(rL50_M_4bc$L50)

#-------Graphique de la proportion de mature/immature et valeur L50------------- 
#---------------------pour chaque année 2006-2020-------------------------------

#2006              #Pas tres fonctionel
L50_M_2006 <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2006")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2006<-fl50(L50_M_2006,niter=100,graph=T)
rL50_M_2006$L50
e1 <- median(rL50_M_2006$L50)

#2007 
L50_M_2007 <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2007")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2007<-fl50(L50_M_2007,niter=100,graph=T)
rL50_M_2007$L50
f1 <- median(rL50_M_2007$L50)

#2008
L50_M_2008 <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2008")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2008<-fl50(L50_M_2008,niter=100,graph=T)
rL50_M_2008$L50
g1 <- median(rL50_M_2008$L50)

#2009                
L50_M_2009 <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2009")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2009<-fl50(L50_M_2009,niter=100,graph=T)
rL50_M_2009$L50
h1 <- median(rL50_M_2009$L50)

#2010                       
L50_M_2010 <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2010")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2010<-fl50(L50_M_2010,niter=100,graph=T)
rL50_M_2010$L50
i1 <- median(rL50_M_2010$L50)

#2011
L50_M_2011 <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2011")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2011<-fl50(L50_M_2011,niter=100,graph=T)
rL50_M_2011$L50
j1 <- median(rL50_M_2011$L50)

#2012
L50_M_2012 <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2012")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2012<-fl50(L50_M_2012,niter=100,graph=T)
rL50_M_2012$L50
k1 <- median(rL50_M_2012$L50)

#2013
L50_M_2013 <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2013")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2013<-fl50(L50_M_2013,niter=100,graph=T)
rL50_M_2013$L50
l1 <- median(rL50_M_2013$L50)

#2014               
L50_M_2014 <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2014")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2014<-fl50(L50_M_2014,niter=100,graph=T)
rL50_M_2014$L50
m1 <- median(rL50_M_2014$L50)

#2015            
L50_M_2015 <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2015")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2015<-fl50(L50_M_2015,niter=100,graph=T)
rL50_M_2015$L50
n1 <- median(rL50_M_2015$L50)

#2016            #Pas très fonctionnel 
L50_M_2016 <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2016")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2016<-fl50(L50_M_2016,niter=100,graph=T)
rL50_M_2016$L50
o1 <- median(rL50_M_2016$L50)

#2017                 #Pas très fonctionnel 
L50_M_2017 <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2017")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2017<-fl50(L50_M_2017,niter=100,graph=T)
rL50_M_2017$L50
p1 <- median(rL50_M_2017$L50)

#2018             #Pas très fonctionnel
L50_M_2018 <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2018")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2018<-fl50(L50_M_2018,niter=100,graph=T)
rL50_M_2018$L50
q1 <- median(rL50_M_2018$L50)

#2019            #Pas très fonctionnel
L50_M_2019 <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2019")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2019<-fl50(L50_M_2019,niter=100,graph=T)
rL50_M_2019$L50
r1 <- median(rL50_M_2019$L50)

#2020                   #Eleve 603
L50_M_2020 <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2020")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2020<-fl50(L50_M_2020,niter=100,graph=T)
rL50_M_2020$L50
s1 <- median(rL50_M_2020$L50)

#------------Tableau et graphiques de la L50 en fonction des zones--------------- 
area <- c("Toutes zones confondues", "zone 8a-b", "zone 7d-e", "zone 4b-c")
L50area_M <- c(a1, b1, c1, d1)
tabL50area_M <- data.frame(area, L50area_M)
tabL50area_M 

ggplot(tabL50area_M ,aes(x=area,y=L50area_M))+
  geom_point()

#------------Tableau et graphiques de la L50 en fonction du temps---------------- 
year <- (2006:2020)
L50tyear_M <- c("", f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, "")
tabL50year_M <- data.frame(year, L50tyear_M)
tabL50year_M

plot(tabL50year_M)
ggplot(tabL50year_M,aes(x=year,y=L50tyear_M))+
  geom_point()



#redflag dans le tableau, on fera un lissage dans le graphique 
#ajouter le nb de mature/immature pour chaque individu 
#mettre un critère ? 



#---------------------------CALCUL DES a50--------------------------------------

#install.packages("sizeMat")
library(sizeMat)
#install.packages("questionr")
library("questionr")


#------------Tri des donnees pour rendre le jeu de donnees plus digerable------- 
Data_MUR_a50 <- DataMatu_MUR2 %>%
  mutate(mat=ifelse(mat == "Immature", "0","1"))%>%  
  select(lenCls, mat, sex, year, newarea, age)%>% 
  rename.variable("lenCls", "length")%>%
  rename.variable("mat", "mature")%>% 
  drop_na()

table(Data_MUR_a50$newarea)
table(Data_MUR_a50$year)
table(Data_MUR_a50$year, Data_MUR_a50$age)

#----------------------------------Femelle--------------------------------------
#-------Graphique de la proportion de mature/immature et valeur a50------------- 
#------------------------en fonction des zones---------------------------------- 

#Toutes zones confondues 
a50_F<- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  mutate(mature=as.factor(mature)) 
ra50_F<-fa50(a50_F,niter=100,graph=T)
a2 <- median(ra50_F$a50)

#zone 8a-b 
a50_F_8ab <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_8ab<-fa50(a50_F_8ab,niter=100,graph=T)
b2 <- median(ra50_F_8ab$a50)

#zone 7d-e 
a50_F_7de <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(newarea=="7d-e")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_7de<-fa50(a50_F_7de,niter=100,graph=T)
c2 <- median(ra50_F_7de$a50)

#zone 4b-c 
a50_F_4bc <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(newarea=="4b-c")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_4bc<-fa50(a50_F_4bc,niter=100,graph=T)
d2 <- median(ra50_F_4bc$a50)

#-------Graphique de la proportion de mature/immature et valeur a50------------- 
#---------------------pour chaque année 2006-2020-------------------------------

#2006
a50_F_2006 <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2006")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2006<-fa50(a50_F_2006,niter=100,graph=T)
ra50_F_2006$a50
e2 <- median(ra50_F_2006$a50)

#2007              
a50_F_2007 <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2007")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2007<-fa50(a50_F_2007,niter=100,graph=T)
ra50_F_2007$a50
f2 <- median(ra50_F_2007$a50)   #ne fonctionne pas, pas assez d'individus mature 

#2008
a50_F_2008 <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2008")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2008<-fa50(a50_F_2008,niter=100,graph=T)
ra50_F_2008$a50
g2 <- median(ra50_F_2008$a50)

#2009                             #ne fonctionne pas très bien, valeur eleve 4.3
a50_F_2009 <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2009")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2009<-fa50(a50_F_2009,niter=100,graph=T)
ra50_F_2009$a50
h2 <- median(ra50_F_2009$a50)

#2010                        
a50_F_2010 <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2010")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2010<-fa50(a50_F_2010,niter=100,graph=T)
ra50_F_2010$a50
i2 <- median(ra50_F_2010$a50)

#2011
a50_F_2011 <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2011")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2011<-fa50(a50_F_2011,niter=100,graph=T)
ra50_F_2011$a50
j2 <- median(ra50_F_2011$a50)

#2012
a50_F_2012 <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2012")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2012<-fa50(a50_F_2012,niter=100,graph=T)
ra50_F_2012$a50
k2 <- median(ra50_F_2012$a50)

#2013
a50_F_2013 <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2013")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2013<-fa50(a50_F_2013,niter=100,graph=T)
ra50_F_2013$a50
l2 <- median(ra50_F_2013$a50)

#2014                             #Que des individus immatures  
a50_F_2014 <- Data_MUR_a50%>%
  filter(sex=="F"  | sex =="I")%>%
  filter(year=="2014")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2014<-fa50(a50_F_2014,niter=100,graph=T)
ra50_F_2014$a50
m2 <- median(ra50_F_2014$a50)

#pb2 <- Data_MUR_a50%>%
#filter(sex=="F")%>%
#filter(year=="2014")

#2015                         #Pas assez d'individus matures 
a50_F_2015 <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2015")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2015<-fa50(a50_F_2015,niter=100,graph=T)
ra50_F_2015$a50
n2 <- median(ra50_F_2015$a50)

#2016
a50_F_2016 <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2016")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2016<-fa50(a50_F_2016,niter=100,graph=T)
ra50_F_2016$a50
o2 <- median(ra50_F_2016$a50)

#2017
a50_F_2017 <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2017")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2017<-fa50(a50_F_2017,niter=100,graph=T)
ra50_F_2017$a50
p2 <- median(ra50_F_2017$a50)

#2018
a50_F_2018 <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2018")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2018<-fa50(a50_F_2018,niter=100,graph=T)
ra50_F_2018$a50
q2 <- median(ra50_F_2018$a50)

#2019
a50_F_2019 <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2019")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2019<-fa50(a50_F_2019,niter=100,graph=T)
ra50_F_2019$a50
r2 <- median(ra50_F_2019$a50)

#2020          ##ne fonctionne pas très bien, valeur eleve 495
a50_F_2020 <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2020")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2020<-fa50(a50_F_2020,niter=100,graph=T)
ra50_F_2020$a50
s2 <- median(ra50_F_2020$a50)

#------------Tableau et graphiques de la a50 en fonction des zones--------------- 
area <- c("Toutes zones confondues", "zone 8a-b", "zone 7d-e", "zone 4b-c")
a50area_F <- c(a2, b2, c2, d2)
taba50area_F <- data.frame(area, a50area_F)
taba50area_F 

ggplot(taba50area_F ,aes(x=area,y=a50area_F))+
  geom_point()

#------------Tableau et graphiques de la a50 en fonction du temps---------------- 
year <- (2006:2020)
a50year_F <- c(e2, "", g2, h2, i2, j2, k2, l2, "", "", o2, p2, q2, r2, s2)
taba50year_F <- data.frame(year, a50year_F )
taba50year_F

plot(taba50year_F)

ggplot(taba50year_F,aes(x=annee,y=a50year_F))+
  geom_point()


#-----------------------------------Male----------------------------------------

#-------Graphique de la proportion de mature/immature et valeur a50------------- 
#------------------------en fonction des zones---------------------------------- 

#Toutes zones confondues 
a50_M<- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  mutate(mature=as.factor(mature)) 
ra50_M<-fa50(a50_M,niter=100,graph=T)
a3 <- median(ra50_M$a50)

#zone 8a-b 
a50_M_8ab <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_8ab<-fa50(a50_M_8ab,niter=100,graph=T)
b3 <- median(ra50_M_8ab$a50)

#zone 7d-e 
a50_M_7de <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(newarea=="7d-e")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_7de<-fa50(a50_M_7de,niter=100,graph=T)
c3 <- median(ra50_M_7de$a50)

#zone 4b-c 
a50_M_4bc <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(newarea=="4b-c")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_4bc<-fa50(a50_M_4bc,niter=100,graph=T)
d3 <- median(ra50_M_4bc$a50)

#-------Graphique de la proportion de mature/immature et valeur a50------------- 
#---------------------pour chaque année 2006-2020-------------------------------

#2006              #Pas fonctionel = pas d'immature
a50_M_2006 <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2006")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2006<-fa50(a50_M_2006,niter=100,graph=T)
ra50_M_2006$a50
e3 <- median(ra50_M_2006$a50)

#2007              #ne fonctionne pas très bien = pas assez d'individus mature
a50_M_2007 <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2007")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2007<-fa50(a50_M_2007,niter=100,graph=T)
ra50_M_2007$a50
f3 <- median(ra50_M_2007$a50)    

#2008                      #Pas fonctionel = peu d'immature
a50_M_2008 <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2008")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2008<-fa50(a50_M_2008,niter=100,graph=T)
ra50_M_2008$a50
g3 <- median(ra50_M_2008$a50)

#2009                             
a50_M_2009 <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2009")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2009<-fa50(a50_M_2009,niter=100,graph=T)
ra50_M_2009$a50
h3 <- median(ra50_M_2009$a50)

#2010                        
a50_M_2010 <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2010")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2010<-fa50(a50_M_2010,niter=100,graph=T)
ra50_M_2010$a50
i3 <- median(ra50_M_2010$a50)

#2011                     #Pas fonctionel = peu d'immature
a50_M_2011 <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2011")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2011<-fa50(a50_M_2011,niter=100,graph=T)
ra50_M_2011$a50
j3 <- median(ra50_M_2011$a50)

#2012                     #Pas fonctionel = peu d'immature
a50_M_2012 <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2012")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2012<-fa50(a50_M_2012,niter=100,graph=T)
ra50_M_2012$a50
k3 <- median(ra50_M_2012$a50)

#2013
a50_M_2013 <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2013")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2013<-fa50(a50_M_2013,niter=100,graph=T)
ra50_M_2013$a50
l3 <- median(ra50_M_2013$a50)

#2014                               
a50_M_2014 <- Data_MUR_a50%>%
  filter(sex=="M"  | sex =="I")%>%
  filter(year=="2014")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2014<-fa50(a50_M_2014,niter=100,graph=T)
ra50_M_2014$a50
m3 <- median(ra50_M_2014$a50)

#pb2 <- Data_MUR_a50%>%
#filter(sex=="M")%>%
#filter(year=="2014")

#2015                         #Pas très fonctionel = peu d'immature
a50_M_2015 <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2015")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2015<-fa50(a50_M_2015,niter=100,graph=T)
ra50_M_2015$a50
n3 <- median(ra50_M_2015$a50)

#2016                          #Pas fonctionel = peu d'immature
a50_M_2016 <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2016")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2016<-fa50(a50_M_2016,niter=100,graph=T)
ra50_M_2016$a50
o3 <- median(ra50_M_2016$a50)

#2017
a50_M_2017 <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2017")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2017<-fa50(a50_M_2017,niter=100,graph=T)
ra50_M_2017$a50
p3 <- median(ra50_M_2017$a50)

#2018                     #Pas fonctionel
a50_M_2018 <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2018")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2018<-fa50(a50_M_2018,niter=100,graph=T)
ra50_M_2018$a50
q3 <- median(ra50_M_2018$a50)

#2019
a50_M_2019 <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2019")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2019<-fa50(a50_M_2019,niter=100,graph=T)
ra50_M_2019$a50
r3 <- median(ra50_M_2019$a50)

#2020          ##ne fonctionne pas, valeur eleve 19.5
a50_M_2020 <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2020")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2020<-fa50(a50_M_2020,niter=100,graph=T)
ra50_M_2020$a50
s3 <- median(ra50_M_2020$a50)

#------------Tableau et graphiques de l'a50 en fonction des zones--------------- 
area <- c("Toutes zones confondues", "zone 8a-b", "zone 7d-e", "zone 4b-c")
a50area_M <- c(a3, b3, c3, d3)
taba50area_M <- data.frame(area, a50area_M)
taba50area_M 

ggplot(taba50area_M ,aes(x=area,y=a50area_M))+
  geom_point()

#------------Tableau et graphiques de l'a50 en fonction du temps---------------- 
year <- (2006:2020)
a50year_M <- c("", f3, "", h3, i3, "", k3, l3, m3, n3, o3, p3, q3, r3, "")
taba50year_M <- data.frame(year, a50year_M )
taba50year_M

plot(taba50year_M)

ggplot(taba50year_M,aes(x=annee,y=a50year_M))+
  geom_point()

































































#-------------------------------------------------------------------------------
#---------------------------Normes de reaction----------------------------------
#-------------------------------------------------------------------------------

#-----------------Toutes zones geographiques confondues-------------------------

#Femelles 

DataMatu_MUR_F_NdeR <- DataMatu_MUR2 %>%
  filter(sex == "F") %>%
  group_by(mat, year, .add = TRUE) %>% 
  mutate(meanLen = mean (lenCls)) %>% 
  mutate(stdErrorLen = std.error(lenCls))%>% 
  mutate(meanAge = mean (age))

table(DataMatu_MUR_F_NdeR$meanAge)
table(DataMatu_MUR_F_NdeR$meanAge, DataMatu_MUR_F_NdeR$year)
table(DataMatu_MUR_F_NdeR$meanLen)
table(DataMatu_MUR_F_NdeR$meanLen, DataMatu_MUR_F_NdeR$year)

ggplot(data = DataMatu_MUR_F_NdeR, 
       mapping = aes(x = meanAge, y = meanLen, color=mat)) + 
  geom_point() + 
  geom_line() +
  geom_smooth() +
  #geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5)+
  labs(title = "Evolution de la moyenne de la longeur totale en fonction l'age pour les \n individus 
       femelles matures et immatures entre 2006 et 2020",
       subtitle = "Toutes zones geographiques de peche proposees confondues",
       caption = "Barres d'erreur: erreur standard",
       x = "Age (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#------------------Toutes zones geographiques-----------------------------------
DataMatu_MUR_F_NdeR <- DataMatu_MUR2 %>%
  filter(sex == "F") %>%
  group_by(mat, year, newarea, .add = TRUE) %>% 
  mutate(meanLen = mean (lenCls)) %>% 
  mutate(stdErrorLen = std.error(lenCls))%>% 
  mutate(meanAge = mean (age))

table(DataMatu_MUR_F_NdeR$meanAge)
table(DataMatu_MUR_F_NdeR$meanAge, DataMatu_MUR_F_NdeR$year)
table(DataMatu_MUR_F_NdeR$meanLen)
table(DataMatu_MUR_F_NdeR$meanLen, DataMatu_MUR_F_NdeR$year)

ggplot(data = DataMatu_MUR_F_NdeR, 
       mapping = aes(x = meanAge, y = meanLen, color=mat)) + 
  geom_point() + 
  geom_line() +
  geom_smooth() +
  facet_wrap(~newarea) +
  #geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5)+
  labs(title = "Evolution de la moyenne de la longeur totale en fonction l'age pour les \n individus 
       femelles matures et immatures entre 2006 et 2020",
       subtitle = "Toutes zones geographiques de peche proposees",
       caption = "Barres d'erreur: erreur standard",
       x = "Age (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#----------------------Zone geographique 8a-b-----------------------------------
ggplot(data = DataMatu_MUR_F_NdeR %>%
         filter(newarea == "8a-b"), 
       mapping = aes(x = meanAge, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()+
  #geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5) +
  labs(title = "Evolution de la moyenne de la longeur totale en fonction l'age pour les \n individus 
       femelles matures et immatures entre 2006 et 2020",
       subtitle = "Zones geographiques de peche : 8.a et 8.b",
       caption = "Barres d'erreur: erreur standard",
       x = "Age (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")


#----------------------Zone geographique 4b-c-----------------------------------
ggplot(data = DataMatu_MUR_F_NdeR %>%
         filter(newarea == "4b-c"), 
       mapping = aes(x = meanAge, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()+
  #geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5) +
  labs(title = "Evolution de la moyenne de la longeur totale en fonction l'age pour les \n individus 
       femelles matures et immatures entre 2006 et 2020",
       subtitle = "Zones geographiques de peche : 4.b et 4.c",
       caption = "Barres d'erreur: erreur standard",
       x = "Age (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#----------------------Zone geographique 7d-e-----------------------------------
ggplot(data = DataMatu_MUR_F_NdeR %>%
         filter(newarea == "7d-e"), 
       mapping = aes(x = meanAge, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()+
  #geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5) +
  labs(title = "Evolution de la moyenne de la longeur totale en fonction l'age pour les \n individus 
       femelles matures et immatures entre 2006 et 2020",
       subtitle = "Zones geographiques de peche : 7.d et 7.e",
       caption = "Barres d'erreur: erreur standard",
       x = "Age (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")


#-----------------------------------Males---------------------------------------

DataMatu_MUR_M_NdeR <- DataMatu_MUR2 %>%
  filter(sex == "M") %>%
  group_by(mat, year, .add = TRUE) %>% 
  mutate(meanLen = mean (lenCls)) %>% 
  mutate(stdErrorLen = std.error(lenCls))%>% 
  mutate(meanAge = mean (age))


table(DataMatu_MUR_M_NdeR$meanAge)
table(DataMatu_MUR_M_NdeR$meanAge, DataMatu_MUR_M_ST$year)
table(DataMatu_MUR_M_NdeR$meanLen)
table(DataMatu_MUR_M_NdeR$meanLen, DataMatu_MUR_M_ST$year)

ggplot(data = DataMatu_MUR_M_NdeR, 
       mapping = aes(x = meanAge, y = meanLen, color=mat)) + 
  geom_point() + 
  geom_line() +
  geom_smooth() +
  #geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5)+
  labs(title = "Evolution de la moyenne de la longeur totale pour les 
       individus males matures et immatures entre 2006 et 2020",
       subtitle = "Toutes zones geographiques de peche proposees confondues",
       caption = "Barres d'erreur: erreur standard",
       x = "Age (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#------------------Toutes zones geographiques-----------------------------------
DataMatu_MUR_M_NdeR <- DataMatu_MUR2 %>%
  filter(sex == "M") %>%
  group_by(mat, year, newarea, .add = TRUE) %>% 
  mutate(meanLen = mean (lenCls)) %>% 
  mutate(stdErrorLen = std.error(lenCls))%>% 
  mutate(meanAge = mean (age))


table(DataMatu_MUR_M_NdeR$meanAge)
table(DataMatu_MUR_M_NdeR$meanAge, DataMatu_MUR_M_NdeR$year)
table(DataMatu_MUR_M_NdeR$meanLen)
table(DataMatu_MUR_M_NdeR$meanLen, DataMatu_MUR_M_NdeR$year)

ggplot(data = DataMatu_MUR_M_NdeR, 
       mapping = aes(x = meanAge, y = meanLen, color=mat)) + 
  geom_point() + 
  geom_line() +
  geom_smooth() +
  facet_wrap(~newarea) +
  #geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5)+
  labs(title = "Evolution de la moyenne de la longeur totale en fonction l'age pour les 
  individus males matures et immatures entre 2006 et 2020",
       subtitle = "Toutes zones geographiques de peche proposees",
       caption = "Barres d'erreur: erreur standard",
       x = "Age (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#----------------------Zone geographique 8a-b-----------------------------------


ggplot(data = DataMatu_MUR_M_NdeR %>%
         filter(newarea == "8a-b"), 
       mapping = aes(x = meanAge, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()+
  #geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5) +
  labs(title = "Evolution de la moyenne de la longeur totale en fonction l'age pour les \n individus 
       males matures et immatures entre 2006 et 2020",
       subtitle = "Zones geographiques de peche : 8.a et 8.b",
       caption = "Barres d'erreur: erreur standard",
       x = "Age (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#----------------------Zone geographique 4b-c-----------------------------------


ggplot(data = DataMatu_MUR_M_NdeR %>%
         filter(newarea == "4b-c"), 
       mapping = aes(x = meanAge, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()+
  #geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5) +
  labs(title = "Evolution de la moyenne de la longeur totale en fonction l'age pour les \n individus 
       males matures et immatures entre 2006 et 2020",
       subtitle = "Zones geographiques de peche : 4.b et 4.c",
       caption = "Barres d'erreur: erreur standard",
       x = "Age (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#----------------------Zone geographique 7d-e-----------------------------------


ggplot(data = DataMatu_MUR_M_NdeR %>%
         filter(newarea == "7d-e"), 
       mapping = aes(x = meanAge, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()+
  #geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5) +
  labs(title = "Evolution de la moyenne de la longeur totale en fonction l'age pour les \n individus 
       males matures et immatures entre 2006 et 2020",
       subtitle = "Zones geographiques de peche : 7.d et 7.e",
       caption = "Barres d'erreur: erreur standard",
       x = "Age (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")



#-------------------------------Plot en 3D--------------------------------------

#-----Package gg3D
#install.packages("devtools")
#devtools::install_github("AckerDWM/gg3D")
library("ggplot2")
library("gg3D")

ggplot(data = DataMatu_MUR_F_NdeR, 
       mapping = aes(x = year, y = meanLen, z = meanAge , color=mat)) +
  theme_void() +
  axes_3D() +
  stat_3D() +
  labs_3D(labs=c("year", "mean of length", "mean of age"))

#theta=1 
#phi=50
#ggplot(iris, aes(x=Petal.Width, y=Sepal.Width, z=Petal.Length, color=Species)) +
#axes_3D(theta=theta, phi=phi) + stat_3D(theta=theta, phi=phi) +
#axis_labs_3D(theta=theta, phi=phi, size=3, hjust=c(1,1,1.2,1.2,1.2,1.2), vjust=c(-.5,-.5,-.2,-.2,1.2,1.2)) +
#labs_3D(theta=theta, phi=phi, hjust=c(1,0,0), vjust=c(1.5,1,-.2),labs=c("Petal width", "Sepal width", "Petal length")) +theme_void()



#-----Package rgl
#install.packages("rgl")
#devtools::install_github("AckerDWM/gg3D")
library("rgl")
plot3d(DataMatu_MUR_F_NdeR$year,
       DataMatu_MUR_F_NdeR$meanLen,
       DataMatu_MUR_F_NdeR$meanAge,
       xlab="year", ylab="mean of length" ,zlab="mean of age", 
       col = DataMatu_MUR_F_NdeR$mat) #ne fonctionne pas

#-----Package plot3D
#install.packages("plot3D")
library("plot3D")

x <- DataMatu_MUR_F_NdeR$year
y <- DataMatu_MUR_F_NdeR$meanLen
z <- DataMatu_MUR_F_NdeR$meanAge
a <- DataMatu_MUR_F_NdeR$mat

table(DataMatu_MUR_F_NdeR$mat)


a <- as.numeric(as.factor(a))


scatter3D(x, y, z,  theta = 15, d = 2, colvar = a, 
          col = c("blue", "red"), 
          xlab = "year",
          ylab ="Moyenne de la longueur", 
          zlab = "Moyenne de l'Age", 
          clab = c("Maturité"), 
          ticktype = "detailed")



