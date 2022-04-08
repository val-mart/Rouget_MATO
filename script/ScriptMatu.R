#install.packages("tidyr")
library(tidyr)
#install.packages("sizeMat")
library(sizeMat)
#install.packages("questionr")
library("questionr")
#install.packages("readxl")
library(readxl)
#install.packages("dplyr")
library(dplyr)
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
#install.packages(fmsb)
library(fmsb)

#Packages non utiles : graph 3D
#-----Package gg3D
#install.packages("devtools")
#devtools::install_github("AckerDWM/gg3D")
#library("ggplot2")
#library("gg3D")
#-----Package rgl
#install.packages("rgl")
#devtools::install_github("AckerDWM/gg3D")
#library("rgl")
#-----Package plot3D
#install.packages("plot3D")
#library("plot3D")
library("COSTcore")

DataMatu <- readRDS("C:/Users/vmartin/Desktop/Stage/R/Maturity/RprojMaturity/Data/ca.rds") #ensemble des données sans 2021
DataMatu2 <- readRDS("C:/Users/vmartin/Desktop/Stage/R/Maturity/RprojMaturity/Data/ca2.rds") #ensemble des données avec 2021

#------------------------------TRI DES DONNEES----------------------------------

#Filter l'espece 
DataMatu_MU <- DataMatu %>% 
  filter(spp == "Mullus surmuletus") 

DataMatu_MU <- DataMatu2 %>% 
  filter(spp == "Mullus surmuletus") 

#Quelques données aberrantes dans les valeurs de longeur !!!! Au delà de 500 mm 
#Concentre sur l'année 2010, zone 8.a (23/159)
hist(DataMatu_MU$lenCls)

sd(DataMatu_MU$lenCls, na.rm = TRUE)
mean(DataMatu_MU$lenCls, na.rm = TRUE)
quantile(DataMatu_MU$lenCls,0.995 , na.rm = TRUE) #=> 370
Probleme_taile <- DataMatu_MU %>%
  filter(lenCls > 500) 
table(Probleme_taile$lenCls)
table(Probleme_taile$lenCls,Probleme_taile$year)
table(Probleme_taile$lenCls,Probleme_taile$area)

#Il vaut mieux filtrer au dela de 500 mm
DataMatu_MUR <- DataMatu_MU %>% 
  filter(str_detect(area, "^27.")) %>%
  filter(lenCls <= 500)%>%
  filter(lenCls !="-1")%>%
  filter(lenCls !="")%>%
  filter(matStage != "F")%>%
  filter(matStage != "NA")%>%
  filter(matStage !="-1")%>%   
  #filter(matStage != "0")%>% # les individus non sexé    
  filter(matStage != "5")%>%
  filter(matStage !="")%>% 
  filter(age !="-1")%>%  
  filter(sex != "-1")%>%
  filter(sex != "NON SEXE")%>%
  mutate(newarea = fct_collapse (area, "8a-b" =  c("27.8.a", "27.8.b","27.8.a,27.8.b"), 
                                 "4c-7d" =  c("27.4.c", "27.7.d")))

#Simplification des stade de maturite 
DataMatu_MUR2 <- DataMatu_MUR %>% 
  mutate(mat=ifelse(sex %in%  "I","Immature",
                   ifelse (matStage %in% c("1", "2A", "A"), "Immature", "Mature")))

table(DataMatu_MUR2$matStage, DataMatu_MUR2$mat)
table(DataMatu_MUR2$sex, DataMatu_MUR2$matStage)


#Calcul du sex ratio : 
table(DataMatu_MUR2$sex, DataMatu_MUR2$year)
tab_sex_ratio <- data.frame(year=rep(NA,16), nbF =rep(NA,16), nbMF=rep(NA,16), sex_ratio=rep(NA,16))

for(currentyear in 2006:2021){
  i <- currentyear - 2005
  #currentyear <- 2009
  dataF <- DataMatu_MUR2%>%
    filter(sex=="F")%>%
    filter(year==currentyear)
  dataMF <- DataMatu_MUR2%>%
    filter(sex == "F" | sex == "M")%>%
    filter(year==currentyear)
  tab_sex_ratio$nbF[i] <- nrow(dataF)
  tab_sex_ratio$nbMF[i] <- nrow(dataMF)
  tab_sex_ratio$ sex_ratio[i] <- nrow(dataF)/nrow(dataMF)
  tab_sex_ratio$year[i] <- currentyear
}

tab_sex_ratio
sex_ratio_value_mean <- mean(tab_sex_ratio$ sex_ratio)
sex_ratio_value_mean

#-----------------------TRAVAIL EXPLORATOIRE (qui a permis le tri des données)-------------------------

#Barplot sexe
ggplot(DataMatu_MU, mapping = aes(x = sex)) + geom_bar()

#Tableau age
table(DataMatu_MU$age)

#Barplot echelles de maturite
ggplot(DataMatu_MU, mapping = aes(x = matScale)) + geom_bar()

#Barplot stades de maturite
ggplot(DataMatu_MU, mapping = aes(x = matStage)) + geom_bar()
ggplot(DataMatu_MUR, mapping = aes(x = matStage)) + geom_bar()


#Barplot  area
ggplot(DataMatu_MUR, mapping = aes(x = area)) + 
  geom_bar()+
  theme(axis.text.x = element_text(size=8, angle=75),
        axis.text.y = element_text(size=8, angle=75))
table(DataMatu_MU$year,DataMatu_MU$area)

ggplot(DataMatu_MUR, mapping = aes(x = newarea)) + geom_bar()
#Pas mal d'info pour 7.d-e, 8.a-b et 4.c-b #on va se concentrer dessus 

#Quelques tableaux 
table(DataMatu_MUR$matStage,DataMatu_MUR$year)
table(DataMatu_MUR$matScale,DataMatu_MUR$year)
# => Plus de precisions dans les echelles  de maturite (1-7)a partir de 2015

table(DataMatu_MUR$year,DataMatu_MUR$newarea)
table(DataMatu_MUR$year,DataMatu_MUR$lenCls)
#View(table(DataMatu_MUR$year,DataMatu_MUR$neware))

#---EVOLUTION DES STADES DE MATURITE DANS LE TEMPS ET EN FONCTION DES ZONES-----

#-------------------------------Femelle-----------------------------------------
#8a-b
ggplot(DataMatu_MUR %>% 
         filter(sex== "F") %>%
         filter(newarea =="8a-b"), mapping = aes(x = matStage)) +
  geom_bar()+
  facet_wrap(~year)

#4c-7d
ggplot(DataMatu_MUR %>% 
         filter(sex== "F") %>%
         filter(newarea =="4c-7d"), mapping = aes(x = matStage)) +
  geom_bar()+
  facet_wrap(~year)

#------------------------------------Male---------------------------------------
#8a-b
ggplot(DataMatu_MUR %>% 
         filter(sex== "M") %>%
         filter(newarea =="8a-b"), mapping = aes(x = matStage)) +
  geom_bar()+
  facet_wrap(~year)

#4c-7d
ggplot(DataMatu_MUR %>% 
         filter(sex== "M") %>%
         filter(newarea =="4c-7d"), mapping = aes(x = matStage)) +
  geom_bar()+
  facet_wrap(~year)

#-----------------En simplifiant les stades de maturite-------------------------

#Tableaux stade de maturite
table(DataMatu_MUR2$mat)
table(DataMatu_MUR2$mat,DataMatu_MUR2$newarea)

#-------------------------------Femelle-----------------------------------------
#8a-b
ggplot(DataMatu_MUR2 %>% 
         filter(sex == "F") %>%
         filter(newarea == "8a-b"), mapping = aes(x = mat)) +
  geom_bar()+
  facet_wrap(~year)

#4c-7d
ggplot(DataMatu_MUR2 %>% 
         filter(sex== "F") %>%
         filter(newarea == "4c-7d"), mapping = aes(x = mat)) +
  geom_bar()+
  facet_wrap(~year)

#---------------------------------Male------------------------------------------

#8a-b
ggplot(DataMatu_MUR2 %>% 
         filter(sex == "M") %>%
         filter(newarea == "8a-b"), mapping = aes(x = mat)) +
  geom_bar()+
  facet_wrap(~year)

#4c-7d
ggplot(DataMatu_MUR2 %>% 
         filter(sex== "M") %>%
         filter(newarea == "4c-7d"), mapping = aes(x = mat)) +
  geom_bar()+
  facet_wrap(~year)

#-----------EVOLUTION LONGUEUR EN FONCTION DU TEMPS MAT/IMMATURE---------------- 
#--------------------------------Femelle----------------------------------------

#Boxplot 
ggplot(DataMatu_MUR2 %>% 
         filter(sex== "F"), aes(x = factor(year), y = lenCls)) +
  geom_boxplot() + 
  facet_wrap(~mat)

#zones geographiques separees
ggplot(DataMatu_MUR2%>%
         filter(sex== "F") %>%
         filter(newarea %in% c("8a-b" , "4c-7d"))
       ,aes(x=lenCls,y=newarea,color=mat))+ #Couleur selon les stades de maturite
  geom_point()+
  facet_wrap(~year)

ggplot(DataMatu_MUR2%>%
         filter(sex== "F") %>%
         filter(newarea %in% c("8a-b" , "4c-7d"))
       ,aes(x=lenCls,y=mat,color=newarea))+     #Couleur selon les zones 
  geom_point()+
  facet_wrap(~year)

#8a-b
ggplot(DataMatu_MUR2%>%
         filter(sex== "F") %>%
         filter(newarea =="8a-b")
       ,aes(x=lenCls,y=mat,color=mat))+
  geom_point()+
  facet_wrap(~year)

#4c-7d
ggplot(DataMatu_MUR2 %>% 
         filter(sex== "F") %>%
         filter(newarea == "4c-7d")
       ,aes(x=lenCls,y=mat,color=mat))+
  geom_point()+
  facet_wrap(~year)

#---------------------------------Male------------------------------------------

#Bopxplot
ggplot(DataMatu_MUR2 %>% 
         filter(sex== "M"), aes(x = factor(year), y = lenCls)) +
  geom_boxplot() + 
  facet_wrap(~mat)

# zones geographiques séparees
ggplot(DataMatu_MUR2%>%
         filter(sex== "M") %>%
         filter(newarea %in% c("8a-b" , "4c-7d"))
       ,aes(x=lenCls,y=newarea,color=mat))+ #Couleur selon les stades de maturite
  geom_point()+
  facet_wrap(~year)

ggplot(DataMatu_MUR2%>%
         filter(sex== "M") %>%
         filter(newarea %in% c("8a-b" , "4c-7d"))
       ,aes(x=lenCls,y=mat,color=newarea))+ #Couleur selon les zones
  geom_point()+
  facet_wrap(~year)

#8a-b
ggplot(DataMatu_MUR2%>%
         filter(sex== "M") %>%
         filter(newarea =="8a-b")
       ,aes(x=lenCls,y=mat,color=mat))+
  geom_point()+
  facet_wrap(~year)

#4c-7d
ggplot(DataMatu_MUR2 %>% 
         filter(sex== "M") %>%
         filter(newarea == "4c-7d")
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

#zones geographiques confondues
ggplot(data = DataMatu_MUR_F_ST, 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() + 
  geom_line() +
  geom_smooth() +
  geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5)+
  labs(title = "Evolution de la moyenne de la longeur totale pour les \n individus femelles matures et immatures entre 2006 et 2021",
       subtitle = "Toutes zones geographiques de peche proposees confondues",
       caption = "Barres d'erreur: erreur standard",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#---------Calcul des moyennes, medianes et erreurs standard des longueurs-------
#-----------en fonction de l'aire, de la maturite et de l'annee-----------------
DataMatu_MUR_F_ST_2 <- DataMatu_MUR2 %>%
  filter(sex == "F") %>%
  filter(newarea == c("8a-b", "4c-7d"))%>%
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
  geom_smooth()+
  labs(title = "Evolution de la moyenne de la longeur totale pour les \n individus femelles matures et immatures entre 2006 et 2021",
       subtitle = " zones 8a-b et 4c-7d séparées ",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#Toutes zones geographique separees (+ erreur standard) 
ggplot(data = DataMatu_MUR_F_ST_2, 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() + 
  facet_wrap(~newarea)+
  geom_line() +
  geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5) +
  labs(title = "Evolution de la moyenne de la longeur totale pour les \n individus femelles matures et immatures entre 2006 et 2021",
       subtitle = " zones 8a-b et 4c-7d séparées ",
       caption = "Barres d'erreur: erreur standard",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#8a-b
ggplot(data = DataMatu_MUR_F_ST_2 %>%
         filter(newarea == "8a-b"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()+
  labs(title = "Evolution de la moyenne de la longeur totale pour les \n individus femelles matures et immatures entre 2006 et 2021",
       subtitle = " zones 8a-b",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#8a-b (+ erreur standard)
ggplot(data = DataMatu_MUR_F_ST_2 %>%
         filter(newarea == "8a-b"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()+
  geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5) +
  labs(title = "Evolution de la moyenne de la longeur totale pour les \n individus femelles matures et immatures entre 2006 et 2021",
       subtitle = "Zones geographiques de peche : 8a-b",
       caption = "Barres d'erreur: erreur standard",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#4c-7d
ggplot(data = DataMatu_MUR_F_ST_2 %>%
         filter(newarea == "4c-7d"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()+
  labs(title = "Evolution de la moyenne de la longeur totale pour les \n individus femelles matures et immatures entre 2006 et 2021",
       subtitle = " zones 4c-7d",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#4c-7d(+ erreur standard)
ggplot(data = DataMatu_MUR_F_ST_2 %>%
         filter(newarea == "4c-7d"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()+
  geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5)+
  labs(title = "Evolution de la moyenne de la longeur totale pour les \n individus femelles matures et immatures entre 2006 et 2021",
       subtitle = "Zones geographiques de peche : 4c-7d",
       caption = "Barres d'erreur: erreur standard",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu") 


#-----------------------Avec la mediane ----------------------------------------
#zones geographique separees
ggplot(data = DataMatu_MUR_F_ST_2, 
       mapping = aes(x = year, y = medianLen, color=mat)) + 
  geom_point() + 
  facet_wrap(~newarea)+
  geom_line()+
  labs(title = "Evolution de la mediane de la longeur totale pour les \n individus femelles matures et immatures entre 2006 et 2021",
       subtitle = " zones 8a-b et 4c-7d séparées",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#8a-b
ggplot(data = DataMatu_MUR_F_ST_2 %>%
         filter(newarea == "8a-b"), 
       mapping = aes(x = year, y = medianLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()+
  labs(title = "Evolution de la mediane de la longeur totale pour les \n individus femelles matures et immatures entre 2006 et 2021",
       subtitle = " zones 8a-b ",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#4c-7d
ggplot(data = DataMatu_MUR_F_ST_2 %>%
         filter(newarea == "4c-7d"), 
       mapping = aes(x = year, y = medianLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()+
  labs(title = "Evolution de la mediane de la longeur totale pour les \n individus femelles matures et immatures entre 2006 et 2021",
       subtitle = " zones 4c-7d",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

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

#Tzones geographiques confondues
ggplot(data = DataMatu_MUR_M_ST, 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() + 
  geom_line() +
  geom_smooth() +
  geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5)+
  labs(title = "Evolution de la moyenne de la longeur totale pour les \n individus males matures et immatures entre 2006 et 2021",
       subtitle = "Toutes zones geographiques de peche proposees confondues",
       caption = "Barres d'erreur: erreur standard",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#---------Calcul des moyennes, mediane et erreur standard des longueurs---------
#-----------en fonction de l'aire, de la maturite et de l'annee-----------------

DataMatu_MUR_M_ST_2 <- DataMatu_MUR2 %>%
  filter(sex == "M") %>%
  filter(newarea == c("8a-b", "4c-7d"))%>%
  group_by(newarea, mat, year, .add = TRUE) %>% 
  mutate(meanLen = mean (lenCls))%>% 
  mutate(medianLen = median (lenCls))%>% 
  mutate(stdErrorLen = std.error(lenCls))

table(DataMatu_MUR_M_ST_2$meanLen)
table(DataMatu_MUR_M_ST_2$meanLen, DataMatu_MUR_M_ST_2$year)

table(DataMatu_MUR_M_ST_2$medianLen)
table(DataMatu_MUR_M_ST_2$medianLen, DataMatu_MUR_M_ST_2$year)

#-------------------Avec la moyenne avec ou non l'erreur standard---------------

#zones geographique separees
ggplot(data = DataMatu_MUR_M_ST_2, 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() + 
  facet_wrap(~newarea)+
  geom_line() +  
  geom_smooth()

#zones geographique separees (+erreur standard)
ggplot(data = DataMatu_MUR_M_ST_2, 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() + 
  facet_wrap(~newarea)+
  geom_line() +
  geom_smooth() +
  geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5)+
  labs(title = "Evolution de la moyenne de la longeur totale pour les \n individus males matures et immatures entre 2006 et 2021",
       subtitle = "zones geographiques 8ab et 4c7d séparées",
       caption = "Barres d'erreur: erreur standard",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#8a-b
ggplot(data =DataMatu_MUR_M_ST_2 %>%
         filter(newarea == "8a-b"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()+
  labs(title = "Evolution de la moyenne de la longeur totale pour les \n individus males matures et immatures entre 2006 et 2021",
       subtitle = "zones geographiques 8ab",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#8a-b (+erreur standard)
ggplot(data = DataMatu_MUR_M_ST_2 %>%
         filter(newarea == "8a-b"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()+
  geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5) +
  labs(title = "Evolution de la moyenne de la longeur totale pour les \n individus males matures et immatures entre 2006 et 2021",
       subtitle = "Zones geographiques de peche : 8a-b",
       caption = "Barres d'erreur: erreur standard",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#4c-7d
ggplot(data = DataMatu_MUR_M_ST_2 %>%
         filter(newarea == "4c-7d"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()+
  labs(title = "Evolution de la moyenne de la longeur totale pour les \n individus males matures et immatures entre 2006 et 2021",
       subtitle = "zones geographiques 4c-7d",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#4c-7d (+erreur standard)
ggplot(data = DataMatu_MUR_M_ST_2 %>%
         filter(newarea == "4c-7d"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()+
  geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5)+
  labs(title = "Evolution de la moyenne de la longeur totale pour les \n individus males matures et immatures entre 2006 et 2021",
       subtitle = "Zones geographiques de peche : 4c-7d",
       caption = "Barres d'erreur: erreur standard",
       x = "Temps (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu") 

#--------------------------Avec la mediane--------------------------------------

#zones geographique separees
ggplot(data =  DataMatu_MUR_M_ST_2, 
       mapping = aes(x = year, y = medianLen, color=mat)) + 
  geom_point() + 
  facet_wrap(~newarea)+
  geom_line()

#8a-b
ggplot(data = DataMatu_MUR_M_ST_2 %>%
         filter(newarea == "8a-b"), 
       mapping = aes(x = year, y = medianLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()

#4c7d 
ggplot(data =  DataMatu_MUR_M_ST_2 %>%
         filter(newarea == "4c7d"), 
       mapping = aes(x = year, y = medianLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()

#---------------------------CALCUL DES L50--------------------------------------
#function to compute L50{{{
fl50<-function(data,niter=1000,graph=T){
  #print(unique(data$year))
  model_glm <- glm(mature ~ length, data = data, family = binomial(link = "logit"))
  smry_model <- summary(model_glm)
  n_coef   <- list()
  for(i in seq_len(niter)){
    new_data   <- data[sample(nrow(data), replace = TRUE), ]
    model_boot <- glm(mature ~ length, data = new_data, family = binomial(link = "logit"))
    glm_coef   <- coef(model_boot)
    n_coef     <- rbind(n_coef, glm_coef)
  }
  A    <- as.numeric(n_coef[,1])
  B    <- as.numeric(n_coef[,2])
  L50  <- -A/B
  create_length <- cbind(1, data$length)
  x_fq     <- as.matrix(create_length) %*% t(as.matrix(cbind(A,B)))
  pred_fq  <- 1 / (1 + exp(-x_fq))
  qtl      <- round(matrixStats::rowQuantiles(pred_fq, probs = c(0.025, 0.5, 0.975)), 3)
  fitted   <- qtl[, 2]
  lower    <- qtl[, 1]
  upper    <- qtl[, 3]
  #output
  x_input <- data$length
  y_input <- as.numeric(as.character(data$mature))
  m_p     <- tapply(y_input, x_input, mean)
  wide    <- quantile(L50, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
  # R-square Nagelkerke method
  model1 <- glm(y_input ~ x_input, family = binomial(link = "logit"))
  # R-square Nagelkerke method
  R2     <- nagelkerkeR2(model1)
  dat1<-data.frame(x=sort(unique(x_input)), y=m_p)#,year=rep(unique(data$year),each=length(m_p)))
  dat2<-data.frame(x=sort(x_input))#,year=rep(unique(data$year),each=length(fitted)),fitted=sort(fitted),lower=sort(lower),upper=sort(upper))
  #tidyr::pivot_longer(3:5)
  estimate <- list(model = smry_model, parameters_A = A, parameters_B = B, L50 = L50, 
                   lower = lower, fitted = fitted,  upper = upper,dat1=dat1,dat2=dat2,L50=L50,R2=R2)
  if(graph){
    plot(sort(unique(x_input)), m_p, xlab = "", ylab = "", pch = 19, col = "darkgrey")
    lines(sort(x_input), sort(fitted), col = "blue", lwd = 1)
    lines(sort(x_input), sort(lower), col = "blue", lwd = 1, lty = 2)
    lines(sort(x_input), sort(upper), col = "blue", lwd = 1, lty = 2)
    lines(c(wide[2], wide[2]), c(0, 0.5), col = "red", lwd = 1, lty = 2)
    lines(c(0, wide[2]), c(0.5, 0.5), col = "red", lwd = 1, lty = 2)
    points(wide[2], 0.5, pch = 19, col = "red", cex = 1.25)
    legend("topleft", c(as.expression(bquote(bold(L[50] == .(round(wide[2], 1))))), 
                        as.expression(bquote(bold(R^2 == .(round(R2, 2)))))), bty = "n")
  }
  return(estimate)
}

#------------Tri des donnees pour rendre le jeu de donnees plus digerable------- 
#Desactiver le package raster pour cette ligne sinon conflict pour la commande 
#select entre les deux packages
Data_MUR_L50 <- DataMatu_MUR2 %>%
  mutate(mat=ifelse(mat == "Immature", "0","1"))%>%  
  dplyr :: select(lenCls, mat, sex, year, newarea, age)%>% 
  rename.variable("lenCls", "length")%>%
  rename.variable("mat", "mature")%>%
  drop_na()

table(Data_MUR_L50$newarea)
table(Data_MUR_L50$year)
table(Data_MUR_L50$sex)
table(Data_MUR_L50$mat, Data_MUR_L50$sex)

#-------Graphique de la proportion de mature/immature et valeur L50------------- 
#---------------------pour chaque année 2006-2021-------------------------------

#------------------Boucle pour calculer le L50 et afficher le R²---------------- 

idyear<-2006:2021
#--------------------------Femelle avec Immature -------------------------------
#------------------------zone CIEM 8ab version boucle---------------------------
#result_L50_FI_8ab<-data.frame(year=rep(NA,16),L50=rep(NA,16), Ecart_Type=rep(NA,16),R2=rep(NA,16))
#for(i in 1:16){
  #i<-3
  currentyear<-idyear[i]
  datatmpyear <- Data_MUR_L50%>%
    filter(sex=="F" | sex =="I")%>%
    filter(year==currentyear)%>%
    filter(newarea=="8a-b")%>%
    mutate(mature=as.factor(mature)) 
  if(nrow(datatmpyear)>2){
    reztmp <-fl50(datatmpyear,niter=100,graph=T)
    #reztmp$L50
    result_L50_FI_8ab$L50[i] <- median(reztmp$L50)
    result_L50_FI_8ab$R2[i] <- reztmp$R2
    result_L50_FI_8ab$Ecart_Type[i]  <- sd(reztmp$L50)
  
  }
  result_L50_FI_8ab$year[i] <- currentyear 
#}
result_L50_FI_8ab

ggplot(result_L50_FI_8ab, 
       mapping = aes(x = year, y = L50)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = L50 - Ecart_Type, ymax = L50 + Ecart_Type), width = 0.5) + 
  ylim(0, 400) +
  labs(title = "Evolution de la L50 chez les femelles et immatures de Rouget barbet de roche entre 2006 et 2021",
       subtitle = "Zones geographiques de pêche : 8a et 8b ",
       caption = "Barres d'erreur: écart-type",
       x = "Temps (an)",
       y = "Longueur moyenne de l'individu à maturité (mm)")  
  #scale_x_continuous(limits=c(2006, 2021), breaks = seq(2006, 2021, 1))

#--------------------------Femelle avec Immature -------------------------------
#------------------------zones CIEM 4c-7d version boucle------------------------
#result_L50_FI_4c7d <-data.frame(year=rep(NA,16),L50=rep(NA,16), Ecart_Type=rep(NA,16), R2=rep(NA,16))
#for(i in 1:16){
  #i<-1
  currentyear<-idyear[i]
  datatmpyear <- Data_MUR_L50%>%
    filter(sex=="F"| sex =="I")%>%
    filter(year==currentyear)%>%
    filter(newarea=="4c-7d")%>%
    mutate(mature=as.factor(mature)) 
  if(nrow(datatmpyear)>2){
    reztmp <-fl50(datatmpyear,niter=100,graph=T)
    #reztmp$L50
    result_L50_FI_4c7d$L50[i] <- median(reztmp$L50)
    result_L50_FI_4c7d$R2[i]  <- reztmp$R2
    result_L50_FI_4c7d$Ecart_Type[i]  <- sd(reztmp$L50)
    
  }
  result_L50_FI_4c7d$year[i] <- currentyear 
#}

result_L50_FI_4c7d

ggplot(result_L50_FI_4c7d, 
         mapping = aes(x = year, y = L50)) +
         geom_point() +
         geom_line() +   
         geom_smooth() + 
         geom_errorbar(aes(ymin = L50 - Ecart_Type, ymax = L50 + Ecart_Type), width = 0.5) + 
  ylim(0, 400) +
  labs(title = "Evolution de la L50 chez les femelles + immatures de Rouget barbet de roche entre 2006 et 2021",
       subtitle = "Zones geographiques de pêche : 4c et 7d",
       caption = "Barres d'erreur: écart-type",
       x = "Temps (an)",
       y = "Longueur moyenne de l'individu à maturité (mm)") + 
  scale_x_continuous(limits=c(2006, 2021), breaks = seq(2006, 2021, 1))




#-------------------------Femelle sans Immature --------------------------------
#------------------------zone CIEM 8ab version boucle---------------------------
#result_L50_F_8ab<-data.frame(year=rep(NA,16),L50=rep(NA,16), Ecart_Type=rep(NA,16),R2=rep(NA,16))
#for(i in 1:16){
  #i<-3
  currentyear<-idyear[i]
  datatmpyear <- Data_MUR_L50%>%
    filter(sex=="F")%>%
    filter(year==currentyear)%>%
    filter(newarea=="8a-b")%>%
    mutate(mature=as.factor(mature)) 
  if(nrow(datatmpyear)>2){
    reztmp <-fl50(datatmpyear,niter=100,graph=T)
    #reztmp$L50
    result_L50_F_8ab$L50[i] <- median(reztmp$L50)
    result_L50_F_8ab$R2[i] <- reztmp$R2
    result_L50_F_8ab$Ecart_Type[i]  <- sd(reztmp$L50)
    
  }
  result_L50_F_8ab$year[i] <- currentyear 
#}

result_L50_F_8ab

ggplot(result_L50_F_8ab, 
       mapping = aes(x = year, y = L50)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = L50 - Ecart_Type, ymax = L50 + Ecart_Type), width = 0.5) + 
  ylim(0, 400) +
  labs(title = "Evolution de la L50 chez les femelles de Rouget barbet de roche entre 2006 et 2021",
       subtitle = "Zones geographiques de pêche : 8a ",
       caption = "Barres d'erreur: écart-type",
       x = "Temps (an)",
       y = "Longueur moyenne de l'individu à maturité (mm)")  
#scale_x_continuous(limits=c(2006, 2021), breaks = seq(2006, 2021, 1))

#-------------------------Femelle sans Immature --------------------------------
#------------------------zones CIEM 4c-7d version boucle------------------------

#result_L50_F_4c7d <-data.frame(year=rep(NA,16),L50=rep(NA,16), Ecart_Type=rep(NA,16), R2=rep(NA,16))
#for(i in 1:16){
  #i<-1
  currentyear<-idyear[i]
  datatmpyear <- Data_MUR_L50%>%
    filter(sex=="F")%>%
    filter(year==currentyear)%>%
    filter(newarea=="4c-7d")%>%
    mutate(mature=as.factor(mature)) 
  if(nrow(datatmpyear)>2){
    reztmp <-fl50(datatmpyear,niter=100,graph=T)
    #reztmp$L50
    result_L50_F_4c7d$L50[i] <- median(reztmp$L50)
    result_L50_F_4c7d$R2[i]  <- reztmp$R2
    result_L50_F_4c7d$Ecart_Type[i]  <- sd(reztmp$L50)
    
  }
  result_L50_F_4c7d$year[i] <- currentyear 
#}
result_L50_F_4c7d

ggplot(result_L50_F_4c7d, 
       mapping = aes(x = year, y = L50)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = L50 - Ecart_Type, ymax = L50 + Ecart_Type), width = 0.5) + 
  ylim(0, 400) +
  labs(title = "Evolution de la L50 chez les femelles de Rouget barbet de roche entre 2006 et 2021",
       subtitle = "Zones geographiques de pêche : 4c et 7d",
       caption = "Barres d'erreur: écart-type",
       x = "Temps (an)",
       y = "Longueur moyenne de l'individu à maturité (mm)") + 
  scale_x_continuous(limits=c(2006, 2021), breaks = seq(2006, 2021, 1))


#

comparaison_L50_F_and_FI_8ab <- cbind(result_L50_F_8ab, result_L50_FI_8ab) 
comparaison_L50_F_and_FI_8ab


comparaison_L50_F_and_FI_4c7d <- cbind(result_L50_F_4c7d, result_L50_FI_4c7d) 
comparaison_L50_F_and_FI_4c7d


idyear<-2006:2021
#----------------------------------Male-----------------------------------------

#----------------------------Male avec Immature --------------------------------
#------------------------zone CIEM 8ab version boucle---------------------------
#result_L50_MI_8ab<-data.frame(year=rep(NA,16),L50=rep(NA,16), Ecart_Type=rep(NA,16),R2=rep(NA,16))
#for(i in 1:16){
  #i<-3
  currentyear<-idyear[i]
  datatmpyear <- Data_MUR_L50%>%
    filter(sex=="M" | sex =="I")%>%
    filter(year==currentyear)%>%
    filter(newarea=="8a-b")%>%
    mutate(mature=as.factor(mature)) 
  if(nrow(datatmpyear)>2){
    reztmp <-fl50(datatmpyear,niter=100,graph=T)
    #reztmp$L50
    result_L50_MI_8ab$L50[i] <- median(reztmp$L50)
    result_L50_MI_8ab$R2[i] <- reztmp$R2
    result_L50_MI_8ab$Ecart_Type[i]  <- sd(reztmp$L50)
    
  }
  result_L50_MI_8ab$year[i] <- currentyear 
#}

result_L50_MI_8ab

ggplot(result_L50_MI_8ab, 
       mapping = aes(x = year, y = L50)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = L50 - Ecart_Type, ymax = L50 + Ecart_Type), width = 0.5) + 
  ylim(0, 400) +
  labs(title = "Evolution de la L50 chez les males +  immatures de Rouget barbet de roche entre 2006 et 2021",
       subtitle = "Zones geographiques de pêche : 4c et 7d",
       caption = "Barres d'erreur: écart-type",
       x = "Temps (an)",
       y = "Longueur moyenne de l'individu à maturité (mm)") + 
  scale_x_continuous(limits=c(2006, 2021), breaks = seq(2006, 2021, 1))

#-----------------------------Male avec Immature -------------------------------
#------------------------zones CIEM 4c-7d version boucle------------------------
#result_L50_MI_4c7d <-data.frame(year=rep(NA,16),L50=rep(NA,16), Ecart_Type=rep(NA,16), R2=rep(NA,16))
#for(i in 1:16){
  #i<-1
  currentyear<-idyear[i]
  datatmpyear <- Data_MUR_L50%>%
    filter(sex=="M" | sex =="I")%>%
    filter(year==currentyear)%>%
    filter(newarea=="4c-7d")%>%
    mutate(mature=as.factor(mature)) 
  if(nrow(datatmpyear)>2){
    reztmp <-fl50(datatmpyear,niter=100,graph=T)
    #reztmp$L50
    result_L50_MI_4c7d$L50[i] <- median(reztmp$L50)
    result_L50_MI_4c7d$R2[i]  <- reztmp$R2
    result_L50_MI_4c7d$Ecart_Type[i]  <- sd(reztmp$L50)
    
  }
  result_L50_MI_4c7d$year[i] <- currentyear 
#}
result_L50_MI_4c7d

ggplot(result_L50_MI_4c7d, 
       mapping = aes(x = year, y = L50)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = L50 - Ecart_Type, ymax = L50 + Ecart_Type), width = 0.5) + 
  ylim(0, 400) +
  labs(title = "Evolution de la L50 chez les males +  immatures de Rouget barbet de roche entre 2006 et 2021",
       subtitle = "Zones geographiques de pêche : 4c et 7d",
       caption = "Barres d'erreur: écart-type",
       x = "Temps (an)",
       y = "Longueur moyenne de l'individu à maturité (mm)") + 
  scale_x_continuous(limits=c(2006, 2021), breaks = seq(2006, 2021, 1))


#----------------------------Male sans Immature --------------------------------
#------------------------zone CIEM 8ab version boucle---------------------------

#result_L50_M_8ab<-data.frame(year=rep(NA,16),L50=rep(NA,16), Ecart_Type=rep(NA,16),R2=rep(NA,16))
#for(i in 1:16){
  #i<-3
  currentyear<-idyear[i]
  datatmpyear <- Data_MUR_L50%>%
    filter(sex=="M")%>%
    filter(year==currentyear)%>%
    filter(newarea=="8a-b")%>%
    mutate(mature=as.factor(mature)) 
  if(nrow(datatmpyear)>2){
    reztmp <-fl50(datatmpyear,niter=100,graph=T)
    #reztmp$L50
    result_L50_M_8ab$L50[i] <- median(reztmp$L50)
    result_L50_M_8ab$R2[i] <- reztmp$R2
    result_L50_M_8ab$Ecart_Type[i]  <- sd(reztmp$L50)
    
  }
  result_L50_M_8ab$year[i] <- currentyear 
#}

result_L50_M_8ab

ggplot(result_L50_M_8ab, 
       mapping = aes(x = year, y = L50)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = L50 - Ecart_Type, ymax = L50 + Ecart_Type), width = 0.5) + 
  ylim(0, 400) +
  labs(title = "Evolution de la L50 chez les males de Rouget barbet de roche entre 2006 et 2021",
       subtitle = "Zones geographiques de pêche : 4c et 7d",
       caption = "Barres d'erreur: écart-type",
       x = "Temps (an)",
       y = "Longueur moyenne de l'individu à maturité (mm)") + 
  scale_x_continuous(limits=c(2006, 2021), breaks = seq(2006, 2021, 1))

#--------------------------Male sans Immature ----------------------------------
#------------------------zones CIEM 4c-7d version boucle------------------------

#result_L50_M_4c7d <-data.frame(year=rep(NA,16),L50=rep(NA,16), Ecart_Type=rep(NA,16), R2=rep(NA,16))
#for(i in 1:16){
  #i<-1
  currentyear<-idyear[i]
  datatmpyear <- Data_MUR_L50%>%
    filter(sex=="M")%>%
    filter(year==currentyear)%>%
    filter(newarea=="4c-7d")%>%
    mutate(mature=as.factor(mature)) 
  if(nrow(datatmpyear)>2){
    reztmp <-fl50(datatmpyear,niter=100,graph=T)
    #reztmp$L50
    result_L50_M_4c7d$L50[i] <- median(reztmp$L50)
    result_L50_M_4c7d$R2[i]  <- reztmp$R2
    result_L50_M_4c7d$Ecart_Type[i]  <- sd(reztmp$L50)
    
  }
  result_L50_M_4c7d$year[i] <- currentyear 
#}
result_L50_M_4c7d

ggplot(result_L50_M_4c7d, 
       mapping = aes(x = year, y = L50)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = L50 - Ecart_Type, ymax = L50 + Ecart_Type), width = 0.5) + 
  ylim(0, 400) +
  labs(title = "Evolution de la L50 chez les males de Rouget barbet de roche entre 2006 et 2021",
       subtitle = "Zones geographiques de pêche : 4c et 7d",
       caption = "Barres d'erreur: écart-type",
       x = "Temps (an)",
       y = "Longueur moyenne de l'individu à maturité (mm)") + 
  scale_x_continuous(limits=c(2006, 2021), breaks = seq(2006, 2021, 1))


#

comparaison_L50_M_and_MI_8ab <- cbind(result_L50_M_8ab, result_L50_MI_8ab) 
comparaison_L50_M_and_MI_8ab


comparaison_L50_M_and_MI_4c7d <- cbind(result_L50_M_4c7d, result_L50_MI_4c7d) 
comparaison_L50_M_and_MI_4c7d


#




























#------------------Boucle pour calculer le a50 et afficher le R²---------------- 

#---------------------------CALCUL DES a50--------------------------------------


fa50<-function(data,niter=1000,graph=T){
  #print(unique(data$year))
  model_glm <- glm(mature ~ age, data = data, family = binomial(link = "logit"))
  smry_model <- summary(model_glm)
  n_coef   <- list()
  for(i in seq_len(niter)){
    new_data   <- data[sample(nrow(data), replace = TRUE), ]
    model_boot <- glm(mature ~ age, data = new_data, family = binomial(link = "logit"))
    glm_coef   <- coef(model_boot)
    n_coef     <- rbind(n_coef, glm_coef)
  }
  A    <- as.numeric(n_coef[,1])
  B    <- as.numeric(n_coef[,2])
  a50  <- -A/B
  create_x <- cbind(1, data$age)
  x_fq     <- as.matrix(create_x) %*% t(as.matrix(cbind(A,B)))
  pred_fq  <- 1 / (1 + exp(-x_fq))
  qtl      <- round(matrixStats::rowQuantiles(pred_fq, probs = c(0.025, 0.5, 0.975)), 3)
  fitted   <- qtl[, 2]
  lower    <- qtl[, 1]
  upper    <- qtl[, 3]
  #output
  x_input <- data$age
  y_input <- as.numeric(as.character(data$mature))
  m_p     <- tapply(y_input, x_input, mean)
  wide    <- quantile(a50, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)
  # R-square Nagelkerke method
  model1 <- glm(y_input ~ x_input, family = binomial(link = "logit"))
  # R-square Nagelkerke method
  R2     <- nagelkerkeR2(model1)
  dat1<-data.frame(x=sort(unique(x_input)), y=m_p)#,year=rep(unique(data$year),each=age(m_p)))
  dat2<-data.frame(x=sort(x_input))#,year=rep(unique(data$year),each=age(fitted)),fitted=sort(fitted),lower=sort(lower),upper=sort(upper))
  #tidyr::pivot_longer(3:5)
  estimate <- list(model = smry_model, parameters_A = A, parameters_B = B, a50 = a50, 
                   lower = lower, fitted = fitted,  upper = upper,dat1=dat1,dat2=dat2,a50=a50,R2=R2)
  if(graph){
    plot(sort(unique(x_input)), m_p, xlab = "", ylab = "", pch = 19, col = "darkgrey")
    lines(sort(x_input), sort(fitted), col = "blue", lwd = 1)
    lines(sort(x_input), sort(lower), col = "blue", lwd = 1, lty = 2)
    lines(sort(x_input), sort(upper), col = "blue", lwd = 1, lty = 2)
    lines(c(wide[2], wide[2]), c(0, 0.5), col = "red", lwd = 1, lty = 2)
    lines(c(0, wide[2]), c(0.5, 0.5), col = "red", lwd = 1, lty = 2)
    points(wide[2], 0.5, pch = 19, col = "red", cex = 1.25)
    legend("topleft", c(as.expression(bquote(bold(a[50] == .(round(wide[2], 1))))), 
                        as.expression(bquote(bold(R^2 == .(round(R2, 2)))))), bty = "n")
  }
  return(estimate)
}

#------------Tri des donnees pour rendre le jeu de donnees plus digerable------- 
Data_MUR_a50 <- DataMatu_MUR2 %>%
  mutate(mat=ifelse(mat == "Immature", "0","1"))%>%  
  dplyr :: select(lenCls, mat, sex, year, newarea, age)%>% 
  rename.variable("lenCls", "length")%>%
  rename.variable("mat", "mature")%>%
  mutate(newarea = fct_collapse (newarea, "4c-7d" =  c("4c", "7d")))%>%
  drop_na()
  

table(Data_MUR_a50$newarea)
table(Data_MUR_a50$year)
table(Data_MUR_a50$year, Data_MUR_a50$age)


#------------------Boucle pour calculer le a50 et afficher le R²---------------- 

idyear<-2006:2021
#--------------------------Femelle avec Immature -------------------------------
#------------------------zone CIEM 8ab version boucle---------------------------
#result_a50_FI_8ab<-data.frame(year=rep(NA,16),a50=rep(NA,16), Ecart_Type=rep(NA,16),R2=rep(NA,16))
#for(i in 1:16){
  #i<-3
  currentyear<-idyear[i]
  datatmpyear <- Data_MUR_a50%>%
    filter(sex=="F" | sex =="I")%>%
    filter(year==currentyear)%>%
    filter(newarea=="8a-b")%>%
    mutate(mature=as.factor(mature)) 
  if(nrow(datatmpyear)>2){
    reztmp <-fa50(datatmpyear,niter=100,graph=T)
    #reztmp$a50
    result_a50_FI_8ab$a50[i] <- median(reztmp$a50)
    result_a50_FI_8ab$R2[i] <- reztmp$R2
    result_a50_FI_8ab$Ecart_Type[i]  <- sd(reztmp$a50)
    
  }
  result_a50_FI_8ab$year[i] <- currentyear 
#}
result_a50_FI_8ab

ggplot(result_a50_FI_8ab, 
       mapping = aes(x = year, y = a50)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = a50 - Ecart_Type, ymax = a50 + Ecart_Type), width = 0.5) + 
  ylim(0, 5) +
  labs(title = "Evolution de la a50 chez les femelles +  immatures de Rouget barbet de roche entre 2006 et 2021",
       subtitle = "Zones geographiques de pêche : 4c et 7d",
       caption = "Barres d'erreur: écart-type",
       x = "Temps (an)",
       y = "Longueur moyenne de l'individu à maturité (mm)")  
#scale_x_continuous(limits=c(2006, 2021), breaks = seq(2006, 2021, 1))

#--------------------------Femelle avec Immature -------------------------------
#------------------------zones CIEM 4c-7d version boucle------------------------
#result_a50_FI_4c7d <-data.frame(year=rep(NA,16),a50=rep(NA,16), Ecart_Type=rep(NA,16), R2=rep(NA,16))
#for(i in 1:16){
  #i<-1
  currentyear<-idyear[i]
  datatmpyear <- Data_MUR_a50%>%
    filter(sex=="F"| sex =="I")%>%
    filter(year==currentyear)%>%
    filter(newarea=="4c-7d")%>%
    mutate(mature=as.factor(mature)) 
  if(nrow(datatmpyear)>2){
    reztmp <-fa50(datatmpyear,niter=100,graph=T)
    #reztmp$a50
    result_a50_FI_4c7d$a50[i] <- median(reztmp$a50)
    result_a50_FI_4c7d$R2[i]  <- reztmp$R2
    result_a50_FI_4c7d$Ecart_Type[i]  <- sd(reztmp$a50)
    
  }
  result_a50_FI_4c7d$year[i] <- currentyear 
#}

result_a50_FI_4c7d

ggplot(result_a50_FI_4c7d, 
       mapping = aes(x = year, y = a50)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = a50 - Ecart_Type, ymax = a50 + Ecart_Type), width = 0.5) + 
  ylim(0, 5) +
  labs(title = "Evolution de la a50 chez les femelles +  immatures de Rouget barbet de roche entre 2006 et 2021",
       subtitle = "Zones geographiques de pêche : 4c et 7d",
       caption = "Barres d'erreur: écart-type",
       x = "Temps (an)",
       y = "Longueur moyenne de l'individu à maturité (mm)") + 
  scale_x_continuous(limits=c(2006, 2021), breaks = seq(2006, 2021, 1))




#-------------------------Femelle sans Immature --------------------------------
#------------------------zone CIEM 8ab version boucle---------------------------
#result_a50_F_8ab<-data.frame(year=rep(NA,16),a50=rep(NA,16), Ecart_Type=rep(NA,16),R2=rep(NA,16))
#for(i in 1:16){
  #i<-3
  currentyear<-idyear[i]
  datatmpyear <- Data_MUR_a50%>%
    filter(sex=="F")%>%
    filter(year==currentyear)%>%
    filter(newarea=="8a-b")%>%
    mutate(mature=as.factor(mature)) 
  if(nrow(datatmpyear)>2){
    reztmp <-fa50(datatmpyear,niter=100,graph=T)
    #reztmp$a50
    result_a50_F_8ab$a50[i] <- median(reztmp$a50)
    result_a50_F_8ab$R2[i] <- reztmp$R2
    result_a50_F_8ab$Ecart_Type[i]  <- sd(reztmp$a50)
    
  }
  result_a50_F_8ab$year[i] <- currentyear 
#}

result_a50_F_8ab

ggplot(result_a50_F_8ab, 
       mapping = aes(x = year, y = a50)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = a50 - Ecart_Type, ymax = a50 + Ecart_Type), width = 0.5) + 
  ylim(0, 5) +
  labs(title = "Evolution de la a50 chez les femelles de Rouget barbet de roche entre 2006 et 2021",
       subtitle = "Zones geographiques de pêche : 4c et 7d",
       caption = "Barres d'erreur: écart-type",
       x = "Temps (an)",
       y = "Longueur moyenne de l'individu à maturité (mm)")  
#scale_x_continuous(limits=c(2006, 2021), breaks = seq(2006, 2021, 1))

#-------------------------Femelle sans Immature --------------------------------
#------------------------zones CIEM 4c-7d version boucle------------------------

#result_a50_F_4c7d <-data.frame(year=rep(NA,16),a50=rep(NA,16), Ecart_Type=rep(NA,16), R2=rep(NA,16))
#for(i in 1:16){
  #i<-1
  currentyear<-idyear[i]
  datatmpyear <- Data_MUR_a50%>%
    filter(sex=="F")%>%
    filter(year==currentyear)%>%
    filter(newarea=="4c-7d")%>%
    mutate(mature=as.factor(mature)) 
  if(nrow(datatmpyear)>2){
    reztmp <-fa50(datatmpyear,niter=100,graph=T)
    #reztmp$a50
    result_a50_F_4c7d$a50[i] <- median(reztmp$a50)
    result_a50_F_4c7d$R2[i]  <- reztmp$R2
    result_a50_F_4c7d$Ecart_Type[i]  <- sd(reztmp$a50)
    
  }
  result_a50_F_4c7d$year[i] <- currentyear 
#}
result_a50_F_4c7d

ggplot(result_a50_F_4c7d, 
       mapping = aes(x = year, y = a50)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = a50 - Ecart_Type, ymax = a50 + Ecart_Type), width = 0.5) + 
  ylim(0, 5) +
  labs(title = "Evolution de la a50 chez les femelles de Rouget barbet de roche entre 2006 et 2021",
       subtitle = "Zones geographiques de pêche : 4c et 7d",
       caption = "Barres d'erreur: écart-type",
       x = "Temps (an)",
       y = "Longueur moyenne de l'individu à maturité (mm)") + 
  scale_x_continuous(limits=c(2006, 2021), breaks = seq(2006, 2021, 1))

#

comparaison_a50_F_and_FI_8ab <- cbind(result_a50_F_8ab, result_a50_FI_8ab) 
comparaison_a50_F_and_FI_8ab


comparaison_a50_F_and_FI_4c7d <- cbind(result_a50_F_4c7d, result_a50_FI_4c7d) 
comparaison_a50_F_and_FI_4c7d


idyear<-2006:2021
#----------------------------------Male-----------------------------------------

#----------------------------Male avec Immature --------------------------------
#------------------------zone CIEM 8ab version boucle---------------------------
#result_a50_MI_8ab<-data.frame(year=rep(NA,16),a50=rep(NA,16), Ecart_Type=rep(NA,16),R2=rep(NA,16))
#for(i in 1:16){
  #i<-3
  currentyear<-idyear[i]
  datatmpyear <- Data_MUR_a50%>%
    filter(sex=="M" | sex =="I")%>%
    filter(year==currentyear)%>%
    filter(newarea=="8a-b")%>%
    mutate(mature=as.factor(mature)) 
  if(nrow(datatmpyear)>2){
    reztmp <-fa50(datatmpyear,niter=100,graph=F)
    #reztmp$a50
    result_a50_MI_8ab$a50[i] <- median(reztmp$a50)
    result_a50_MI_8ab$R2[i] <- reztmp$R2
    result_a50_MI_8ab$Ecart_Type[i]  <- sd(reztmp$a50)
  }
  result_a50_MI_8ab$year[i] <- currentyear 
#}
result_a50_MI_8ab

ggplot(result_a50_MI_8ab, 
       mapping = aes(x = year, y = a50)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = a50 - Ecart_Type, ymax = a50 + Ecart_Type), width = 0.5) + 
  ylim(0, 5) +
  labs(title = "Evolution de la a50 chez les males +  immatures de Rouget barbet de roche entre 2006 et 2021",
       subtitle = "Zones geographiques de pêche : 4c et 7d",
       caption = "Barres d'erreur: écart-type",
       x = "Temps (an)",
       y = "Longueur moyenne de l'individu à maturité (mm)") + 
  scale_x_continuous(limits=c(2006, 2021), breaks = seq(2006, 2021, 1))

#-----------------------------Male avec Immature -------------------------------
#------------------------zones CIEM 4c-7d version boucle------------------------

result_a50_MI_4c7d <-data.frame(year=rep(NA,16),a50=rep(NA,16), Ecart_Type=rep(NA,16), R2=rep(NA,16))
for(i in 1:16){
  #i<-1
  currentyear<-idyear[i]
  datatmpyear <- Data_MUR_a50%>%
    filter(sex=="M" | sex =="I")%>%
    filter(year==currentyear)%>%
    filter(newarea=="4c-7d")%>%
    mutate(mature=as.factor(mature)) 
  if(nrow(datatmpyear)>2){
    reztmp <-fa50(datatmpyear,niter=100,graph=T)
    #reztmp$a50
    result_a50_MI_4c7d$a50[i] <- median(reztmp$a50)
    result_a50_MI_4c7d$R2[i]  <- reztmp$R2
    result_a50_MI_4c7d$Ecart_Type[i]  <- sd(reztmp$a50)
    
  }
  result_a50_MI_4c7d$year[i] <- currentyear 
}
result_a50_MI_4c7d

ggplot(result_a50_MI_4c7d, 
       mapping = aes(x = year, y = a50)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = a50 - Ecart_Type, ymax = a50 + Ecart_Type), width = 0.5) + 
  ylim(0, 5) +
  labs(title = "Evolution de la a50 chez les males +  immatures de Rouget barbet de roche entre 2006 et 2021",
       subtitle = "Zones geographiques de pêche : 4c et 7d",
       caption = "Barres d'erreur: écart-type",
       x = "Temps (an)",
       y = "Longueur moyenne de l'individu à maturité (mm)") + 
  scale_x_continuous(limits=c(2006, 2021), breaks = seq(2006, 2021, 1))


#----------------------------Male sans Immature --------------------------------
#------------------------zone CIEM 8ab version boucle---------------------------

#result_a50_M_8ab<-data.frame(year=rep(NA,16),a50=rep(NA,16), Ecart_Type=rep(NA,16),R2=rep(NA,16))
#for(i in 1:16){
  #i<-3
  currentyear<-idyear[i]
  datatmpyear <- Data_MUR_a50%>%
    filter(sex=="M")%>%
    filter(year==currentyear)%>%
    filter(newarea=="8a-b")%>%
    mutate(mature=as.factor(mature)) 
  if(nrow(datatmpyear)>2){
    reztmp <-fa50(datatmpyear,niter=100,graph=F)
    #reztmp$a50
    result_a50_M_8ab$a50[i] <- median(reztmp$a50)
    result_a50_M_8ab$R2[i] <- reztmp$R2
    result_a50_M_8ab$Ecart_Type[i]  <- sd(reztmp$a50)
    
  }
  result_a50_M_8ab$year[i] <- currentyear 
#}

result_a50_M_8ab


ggplot(result_a50_M_8ab, 
       mapping = aes(x = year, y = a50)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = a50 - Ecart_Type, ymax = a50 + Ecart_Type), width = 0.5) + 
  ylim(0, 5) +
  labs(title = "Evolution de la a50 chez les males de Rouget barbet de roche entre 2006 et 2021",
       subtitle = "Zones geographiques de pêche : 4c et 7d",
       caption = "Barres d'erreur: écart-type",
       x = "Temps (an)",
       y = "Longueur moyenne de l'individu à maturité (mm)") + 
  scale_x_continuous(limits=c(2006, 2021), breaks = seq(2006, 2021, 1))

#--------------------------Male sans Immature ----------------------------------
#------------------------zones CIEM 4c-7d version boucle------------------------

#result_a50_M_4c7d <-data.frame(year=rep(NA,16),a50=rep(NA,16), Ecart_Type=rep(NA,16), R2=rep(NA,16))
#for(i in 1:16){
  #i<-1
  currentyear<-idyear[i]
  datatmpyear <- Data_MUR_a50%>%
    filter(sex=="M" | sex =="I")%>%
    filter(year==currentyear)%>%
    filter(newarea=="4c-7d")%>%
    mutate(mature=as.factor(mature)) 
  if(nrow(datatmpyear)>2){
    reztmp <-fa50(datatmpyear,niter=100,graph=T)
    #reztmp$a50
    result_a50_M_4c7d$a50[i] <- median(reztmp$a50)
    result_a50_M_4c7d$R2[i]  <- reztmp$R2
    result_a50_M_4c7d$Ecart_Type[i]  <- sd(reztmp$a50)
    
  }
  result_a50_M_4c7d$year[i] <- currentyear 
#}
result_a50_M_4c7d

ggplot(result_a50_M_4c7d, 
       mapping = aes(x = year, y = a50)) +
  geom_point() +
  geom_line() +   
  geom_smooth() + 
  geom_errorbar(aes(ymin = a50 - Ecart_Type, ymax = a50 + Ecart_Type), width = 0.5) + 
  ylim(0, 5) +
  labs(title = "Evolution de la a50 chez les males de Rouget barbet de roche entre 2006 et 2021",
       subtitle = "Zones geographiques de pêche : 4c et 7d",
       caption = "Barres d'erreur: écart-type",
       x = "Temps (an)",
       y = "Longueur moyenne de l'individu à maturité (mm)") + 
  scale_x_continuous(limits=c(2006, 2021), breaks = seq(2006, 2021, 1))

#
comparaison_a50_M_and_MI_8ab <- cbind(result_a50_M_8ab, result_a50_MI_8ab) 
comparaison_a50_M_and_MI_8ab

comparaison_a50_M_and_MI_4c7d <- cbind(result_a50_M_4c7d, result_a50_MI_4c7d) 
comparaison_a50_M_and_MI_4c7d


#











































































#-------------------------------------------------------------------------------
#--------------------------CALUCUL DE TOUT MANUELLEMENT-------------------------
#-------------------------------------------------------------------------------

#----------------------------------Femelle--------------------------------------
#-------Graphique de la proportion de mature/immature et valeur L50------------- 
#------------------------en fonction des zones---------------------------------- 

#Toutes zones confondues 
L50_F<- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  mutate(mature=as.factor(mature)) 
rL50_F<-fl50(L50_F,niter=100,graph=T)
a_l50_F <- median(rL50_F$L50)


#zone 8a-b - toutes années
L50_F_8ab <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_8ab<-fl50(L50_F_8ab,niter=100,graph=T)
b_l50_F <- median(rL50_F_8ab$L50)

#table(L50_F_8ab$year, L50_F_8ab$mature)

#zone 7d - toutes années
L50_F_7d <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_7d<-fl50(L50_F_7d,niter=100,graph=T)
c_l50_F <- median(rL50_F_7d$L50)

#table(L50_F_7d$year, L50_F_7d$mature)

#zone 4c - toutes années
L50_F_4c <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_4c<-fl50(L50_F_4c,niter=100,graph=T)
d_l50_F <- median(rL50_F_4c$L50)

#table(L50_F_4c$year, L50_F_4c$mature)

#------------Tableau et graphiques de la L50 en fonction des zones--------------- 
area <- c("Toutes zones confondues", "zone 8a-b", "zone 7d", "zone 4c")
L50area_F <- c(a_l50_F, b_l50_F, c_l50_F, d_l50_F)
tabL50area_F <- data.frame(area, L50area_F)
tabL50area_F 

ggplot(tabL50area_F ,aes(x=area,y=L50area_F))+
  geom_point()


#---------------------------zone CIEM 8ab---------------------------------------

#2006                                                        #ne fonctionne  pas 
L50_F_2006_8ab <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2006")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2006_8ab <-fl50(L50_F_2006_8ab,niter=100,graph=T)
rL50_F_2006_8ab$L50
e_l50_F_8ab <- median(rL50_F_2006$L50) 

#2007                                                         #ne fonctionne pas 
L50_F_2007_8ab <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2007")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2007_8ab<-fl50(L50_F_2007_8ab,niter=100,graph=T)
rL50_F_2007_8ab$L50
f_l50_F_8ab <- median(rL50_F_2007_8ab$L50)    

#2008                                               #ne fonctionne pas très bien 
L50_F_2008_8ab <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2008")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2008_8ab<-fl50(L50_F_2008_8ab,niter=100,graph=T)
rL50_F_2008_8ab$L50
g_l50_F_8ab <- median(rL50_F_2008_8ab$L50)

#2009                                               #ne fonctionne pas très bien  
L50_F_2009_8ab <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2009")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2009_8ab<-fl50(L50_F_2009_8ab,niter=100,graph=T)
rL50_F_2009_8ab$L50
h_l50_F_8ab <- median(rL50_F_2009_8ab$L50)

#2010                                               #ne fonctionne pas très bien      
L50_F_2010_8ab <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2010")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2010_8ab<-fl50(L50_F_2010_8ab,niter=100,graph=T)
rL50_F_2010_8ab$L50
i_l50_F_8ab <- median(rL50_F_2010_8ab$L50)

#2011                                               #ne fonctionne pas très bien 
L50_F_2011_8ab <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2011")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2011_8ab<-fl50(L50_F_2011_8ab,niter=100,graph=T)
rL50_F_2011_8ab$L500
j_l50_F_8ab <- median(rL50_F_2011_8ab$L50)

#2012                                                                        #OK
L50_F_2012_8ab <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2012")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2012_8ab<-fl50(L50_F_2012_8ab,niter=100,graph=T)
rL50_F_2012_8ab$L50
k_l50_F_8ab <- median(rL50_F_2012_8ab$L50)

#2013                                                                        #OK
L50_F_2013_8ab <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2013")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2013_8ab<-fl50(L50_F_2013_8ab,niter=100,graph=T)
rL50_F_2013_8ab$L50
l_l50_F_8ab <- median(rL50_F_2013_8ab$L50)
  
#2014                                                         #ne fonctionne pas         
L50_F_2014_8ab <- Data_MUR_L50%>%
  filter(sex=="F"  | sex =="I")%>%
  filter(year=="2014")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2014_8ab<-fl50(L50_F_2014_8ab,niter=100,graph=T)
rL50_F_2014_8ab$L50
m_l50_F_8ab <- median(rL50_F_2014_8ab$L50)

#pb2 <- Data_MUR_L50%>%
#filter(sex=="F")%>%
#filter(year=="2014")

#2015                                                         #ne fonctionne pas  
L50_F_2015_8ab <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2015")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2015_8ab<-fl50(L50_F_2015_8ab,niter=100,graph=T)
rL50_F_2015_8ab$L50
n_l50_F_8ab <- median(rL50_F_2015_8ab$L50)

#2016                                                                        #OK 
L50_F_2016_8ab <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2016")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2016_8ab<-fl50(L50_F_2016_8ab,niter=100,graph=T)
rL50_F_2016_8ab$L50
o_l50_F_8ab <- median(rL50_F_2016_8ab$L50)

#2017                                                                        #OK
L50_F_2017_8ab <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2017")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2017_8ab<-fl50(L50_F_2017_8ab,niter=100,graph=T)
rL50_F_2017_8ab$L50
p_l50_F_8ab <- median(rL50_F_2017_8ab$L50)

#2018                                               #ne fonctionne pas très bien 
L50_F_2018_8ab <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2018")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2018_8ab<-fl50(L50_F_2018_8ab,niter=100,graph=T)
rL50_F_2018_8ab$L50
q_l50_F_8ab <- median(rL50_F_2018_8ab$L50)

#2019                                                                        #OK
L50_F_2019_8ab <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2019")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2019_8ab<-fl50(L50_F_2019_8ab,niter=100,graph=T)
rL50_F_2019_8ab$L50
r_l50_F_8ab <- median(rL50_F_2019_8ab$L50)

#2021                                                #ne fonctione pas très bien 
L50_F_2021_8ab <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2021")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2021_8ab<-fl50(L50_F_2021_8ab,niter=100,graph=T)
rL50_F_2021_8ab$L50
s_l50_F_8ab <- median(rL50_F_2021_8ab$L50)

#------------Tableau et graphiques de la L50 en fonction du temps---------------- 
year <- (2006:2021)
L50year_F_8ab <- c("", "", "", "", "", "", k_l50_F_8ab, l_l50_F_8ab, "", "", o_l50_F_8ab, 
                   p_l50_F_8ab, "", r_l50_F_8ab, "")
tabL50year_F_8ab <- data.frame(year, L50year_F_8ab)
tabL50year_F_8ab

plot(tabL50year_F_8ab)

ggplot(tabL50year_F_8ab,aes(x=year,y=L50year_F_8ab))+
  geom_point()+
  geom_path()+ 
  geom_smooth(method = "loess")



#---------------------------zone CIEM 7d----------------------------------------

#2006                                                                        #OK
L50_F_2006_7d <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2006")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2006_7d <-fl50(L50_F_2006_7d,niter=100,graph=T)
rL50_F_2006_7d$L50
e_l50_F_7d <- median(rL50_F_2006_7d$L50) 

#2007                                                          #ne fonctione pas
L50_F_2007_7d <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2007")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2007_7d<-fl50(L50_F_2007_7d,niter=100,graph=T)
rL50_F_2007_7d$L50
f_l50_F_7d <- median(rL50_F_2007_7d$L50)  

#2008                                                          #ne fonctione pas
L50_F_2008_7d <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2008")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2008_7d<-fl50(L50_F_2008_7d,niter=100,graph=T)
rL50_F_2008_7d$L50
g_l50_F_7d <- median(rL50_F_2008_7d$L50)

#2009                                                          #ne fonctione pas
L50_F_2009_7d <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2009")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2009_7d<-fl50(L50_F_2009_7d,niter=100,graph=T)
rL50_F_2009_7d$L50
h_l50_F_7d <- median(rL50_F_2009_7d$L50)

#2010                                                                        #OK
L50_F_2010_7d <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2010")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2010_7d<-fl50(L50_F_2010_7d,niter=100,graph=T)
rL50_F_2010_7d$L50
i_l50_F_7d <- median(rL50_F_2010_7d$L50)

#2011                                                                        #OK
L50_F_2011_7d <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2011")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2011_7d<-fl50(L50_F_2011_7d,niter=100,graph=T)
rL50_F_2011_7d$L50
j_l50_F_7d <- median(rL50_F_2011_7d$L50)

#2012                                                                        #OK
L50_F_2012_7d <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2012")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2012_7d<-fl50(L50_F_2012_7d,niter=100,graph=T)
rL50_F_2012_7d$L50
k_l50_F_7d <- median(rL50_F_2012_7d$L50)

#2013                                                         #ne fonctionne pas 
L50_F_2013_7d <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2013")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2013_7d<-fl50(L50_F_2013_7d,niter=100,graph=T)
rL50_F_2013_7d$L50
l_l50_F_7d <- median(rL50_F_2013_7d$L50)

#2014                                                         #ne fonctionne pas 
L50_F_2014_7d <- Data_MUR_L50%>%
  filter(sex=="F"  | sex =="I")%>%
  filter(year=="2014")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2014_7d<-fl50(L50_F_2014_7d,niter=100,graph=T)
rL50_F_2014_7d$L50
m_l50_F_7d <- median(rL50_F_2014_7d$L50)

#pb2 <- Data_MUR_L50%>%
#filter(sex=="F")%>%
#filter(year=="2014")

#2015                                                         #ne fonctionne pas 
L50_F_2015_7d <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2015")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2015_7d<-fl50(L50_F_2015_7d,niter=100,graph=T)
rL50_F_2015_7d$L50
n_l50_F_7d <- median(rL50_F_2015_7d$L50)

#2016                                                                        #OK
L50_F_2016_7d <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2016")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2016_7d<-fl50(L50_F_2016_7d,niter=100,graph=T)
rL50_F_2016_7d$L50
o_l50_F_7d <- median(rL50_F_2016_7d$L50)

#2017                                                                        #OK
L50_F_2017_7d <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2017")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2017_7d<-fl50(L50_F_2017_7d,niter=100,graph=T)
rL50_F_2017_7d$L50
p_l50_F_7d <- median(rL50_F_2017_7d$L50)

#2018                                                                        #OK
L50_F_2018_7d <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2018")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2018_7d<-fl50(L50_F_2018_7d,niter=100,graph=T)
rL50_F_2018_7d$L50
q_l50_F_7d <- median(rL50_F_2018_7d$L50)

#2019                                                         #ne fonctionne pas
L50_F_2019_7d <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2019")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2019_7d<-fl50(L50_F_2019_7d,niter=100,graph=T)
rL50_F_2019_7d$L50
r_l50_F_7d <- median(rL50_F_2019_7d$L50)

#2021                                                         #ne fonctionne pas        
L50_F_2021_7d <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2021")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2021_7d<-fl50(L50_F_2021_7d,niter=100,graph=T)
rL50_F_2021_7d$L50
s_l50_F_7d <- median(rL50_F_2021_7d$L50)



#------------Tableau et graphiques de la L50 en fonction du temps---------------- 
year <- (2006:2021)
L50year_F_7d <- c(e_l50_F_7d, "", "", "", i_l50_F_7d, j_l50_F_7d, k_l50_F_7d, "", 
                  "", "", o_l50_F_7d, p_l50_F_7d, q_l50_F_7d, "", "")
tabL50year_F_7d <- data.frame(year, L50year_F_7d)
tabL50year_F_7d

plot(tabL50year_F_7d)

ggplot(tabL50year_F_7d,aes(x=year,y=L50year_F_7d))+
  geom_point()+
  geom_path()+ 
  geom_smooth(method = "loess")



#---------------------------zone CIEM 4c----------------------------------------

#2006                                                         #ne fonctionne pas
L50_F_2006_4c <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2006")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2006_4c <-fl50(L50_F_2006_4c,niter=100,graph=T)
rL50_F_2006_4c$L50
e_l50_F_4c <- median(rL50_F_2006_4c$L50)

#2007                                                         #ne fonctionne pas
L50_F_2007_4c <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2007")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2007_4c<-fl50(L50_F_2007_4c,niter=100,graph=T)
rL50_F_2007_4c$L50
f_l50_F_4c <- median(rL50_F_2007_4c$L50)   

#2008                                                         #ne fonctionne pas
L50_F_2008_4c <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2008")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2008_4c<-fl50(L50_F_2008_4c,niter=100,graph=T)
rL50_F_2008_4c$L50
g_l50_F_4c <- median(rL50_F_2008_4c$L50)

#2009                                                         #ne fonctionne pas                          
L50_F_2009_4c <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2009")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2009_4c<-fl50(L50_F_2009_4c,niter=100,graph=T)
rL50_F_2009_4c$L50
h_l50_F_4c <- median(rL50_F_2009_4c$L50)

#2010                                                         #ne fonctionne pas 
L50_F_2010_4c <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2010")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2010_4c<-fl50(L50_F_2010_4c,niter=100,graph=T)
rL50_F_2010_4c$L50
i_l50_F_4c <- median(rL50_F_2010_4c$L50)

#2011                                                         #ne fonctionne pas
L50_F_2011_4c <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2011")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2011_4c<-fl50(L50_F_2011_4c,niter=100,graph=T)
rL50_F_2011_4c$L50
j_l50_F_4c <- median(rL50_F_2011_4c$L50)

#2012                                                                        #OK
L50_F_2012_4c <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2012")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2012_4c<-fl50(L50_F_2012_4c,niter=100,graph=T)
rL50_F_2012_4c$L50
k_l50_F_4c <- median(rL50_F_2012_4c$L50)

#2013                                                         #ne fonctionne pas
L50_F_2013_4c <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2013")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2013_4c<-fl50(L50_F_2013_4c,niter=100,graph=T)
rL50_F_2013_4c$L50
l_l50_F_4c <- median(rL50_F_2013$L50)

#2014                                                         #ne fonctionne pas                            
L50_F_2014_4c <- Data_MUR_L50%>%
  filter(sex=="F"  | sex =="I")%>%
  filter(year=="2014")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2014_4c<-fl50(L50_F_2014_4c,niter=100,graph=T)
rL50_F_2014_4c$L50
m_l50_F_4c <- median(rL50_F_2014_4c$L50)

#pb2 <- Data_MUR_L50%>%
#filter(sex=="F")%>%
#filter(year=="2014")

#2015                                                         #ne fonctionne pas                          
L50_F_2015_4c <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2015")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2015_4c<-fl50(L50_F_2015_4c,niter=100,graph=T)
rL50_F_2015_4c$L50
n_l50_F_4c <- median(rL50_F_2015_4c$L50)

#2016                                                         #ne fonctionne pas
L50_F_2016_4c <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2016")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2016_4c<-fl50(L50_F_2016_4c,niter=100,graph=T)
rL50_F_2016_4c$L50
o_l50_F_4c <- median(rL50_F_2016_4c$L50)

#2017                                                         #ne fonctionne pas
L50_F_2017_4c <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2017")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2017_4c<-fl50(L50_F_2017_4c,niter=100,graph=T)
rL50_F_2017_4c$L50
p_l50_F_4c <- median(rL50_F_2017_4c$L50)

#2018                                                         #ne fonctionne pas
L50_F_2018_4c <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2018")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2018_4c<-fl50(L50_F_2018_4c,niter=100,graph=T)
rL50_F_2018_4c$L50
q_l50_F_4c <- median(rL50_F_2018_4c$L50)

#2019                                                         #ne fonctionne pas
L50_F_2019_4c <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2019")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2019_4c<-fl50(L50_F_2019_4c,niter=100,graph=T)
rL50_F_2019_4c$L50
r_l50_F_4c <- median(rL50_F_2019_4c$L50)

#2021                                                         #ne fonctionne pas        
L50_F_2021_4c <- Data_MUR_L50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2021")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_F_2021_4c<-fl50(L50_F_2021_4c,niter=100,graph=T)
rL50_F_2021_4c$L50
s_l50_F_4c <- median(rL50_F_2021$L50)



#------------Tableau et graphiques de la L50 en fonction du temps---------------- 
year <- (2006:2021)
L50year_F_4c <- c("", "", "", "", "","", k_l50_F_4c, "", "", "", "", "", "", "", "")
tabL50year_F_4c <- data.frame(year, L50year_F_4c)
tabL50year_F_4c

plot(tabL50year_F_4c)

ggplot(tabL50year_F_4c,aes(x=year,y=L50year_F_4c))+
  geom_point()+
  geom_path()+ 
  geom_smooth(method = "loess")



#------------------------------------Male---------------------------------------

#-------Graphique de la proportion de mature/immature et valeur L50------------- 
#------------------------en fonction des zones---------------------------------- 

#Toutes zones confondues 
L50_M<- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  mutate(mature=as.factor(mature)) 
rL50_M<-fl50(L50_M,niter=100,graph=T)
a_l50_M <- median(rL50_M$L50)


#zone 8a-b - toutes années
L50_M_8ab <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_8ab<-fl50(L50_M_8ab,niter=100,graph=T)
b_l50_M <- median(rL50_M_8ab$L50)

#table(L50_M_8ab$year, L50_M_8ab$mature)

#zone 7d - toutes années
L50_M_7d <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_7d<-fl50(L50_M_7d,niter=100,graph=T)
c_l50_M <- median(rL50_M_7d$L50)

#table(L50_M_7d$year, L50_M_7d$mature)

#zone 4c - toutes années
L50_M_4c <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_4c<-fl50(L50_M_4c,niter=100,graph=T)
d_l50_M <- median(rL50_M_4c$L50)

table(L50_M_4c$year, L50_M_4c$mature)

#------------Tableau et graphiques de la L50 en fonction des zones--------------- 
area <- c("Toutes zones confondues", "zone 8a-b", "zone 7d", "zone 4c")
L50area_M <- c(a_l50_M, b_l50_M, c_l50_M, d_l50_M)
tabL50area_M <- data.frame(area, L50area_M)
tabL50area_M 

ggplot(tabL50area_M ,aes(x=area,y=L50area_M))+
  geom_point()



#-------Graphique de la proportion de mature/immature et valeur L50------------- 
#---------------------pour chaque année 2006-2021-------------------------------

#---------------------------zone CIEM 8ab---------------------------------------

#2006                                                         #ne fonctionne pas
L50_M_2006_8ab <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2006")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2006_8ab <-fl50(L50_M_2006_8ab,niter=100,graph=T)
rL50_M_2006_8ab$L50
e_l50_M_8ab <- median(rL50_M_2006$L50) 

#2007                                                         #ne fonctionne pas          
L50_M_2007_8ab <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2007")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2007_8ab<-fl50(L50_M_2007_8ab,niter=100,graph=T)
rL50_M_2007_8ab$L50
f_l50_M_8ab <- median(rL50_M_2007_8ab$L50) 

#2008                                               #ne fonctionne pas tres bien 
L50_M_2008_8ab <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2008")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2008_8ab<-fl50(L50_M_2008_8ab,niter=100,graph=T)
rL50_M_2008_8ab$L50
g_l50_M_8ab <- median(rL50_M_2008_8ab$L50)

#2009                                               #ne fonctionne pas tres bien 
L50_M_2009_8ab <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2009")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2009_8ab<-fl50(L50_M_2009_8ab,niter=100,graph=T)
rL50_M_2009_8ab$L50
h_l50_M_8ab <- median(rL50_M_2009_8ab$L50)

#2010                                               #ne fonctionne pas tres bien 
L50_M_2010_8ab <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2010")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2010_8ab<-fl50(L50_M_2010_8ab,niter=100,graph=T)
rL50_M_2010_8ab$L50
i_l50_M_8ab <- median(rL50_M_2010_8ab$L50)

#2011                                               #ne fonctionne pas tres bien
L50_M_2011_8ab <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2011")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2011_8ab<-fl50(L50_M_2011_8ab,niter=100,graph=T)
rL50_M_2011_8ab$L50
j_l50_M_8ab <- median(rL50_M_2011_8ab$L50)

#2012                                                                        #OK
L50_M_2012_8ab <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2012")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2012_8ab<-fl50(L50_M_2012_8ab,niter=100,graph=T)
rL50_M_2012_8ab$L50
k_l50_M_8ab <- median(rL50_M_2012_8ab$L50)

#2013                                               #ne fonctionne pas tres bien
L50_M_2013_8ab <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2013")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2013_8ab<-fl50(L50_M_2013_8ab,niter=100,graph=T)
rL50_M_2013_8ab$L50
l_l50_M_8ab <- median(rL50_M_2013$L50)

#2014                                                         #ne fonctionne pas                         
L50_M_2014_8ab <- Data_MUR_L50%>%
  filter(sex=="M"  | sex =="I")%>%
  filter(year=="2014")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2014_8ab<-fl50(L50_M_2014_8ab,niter=100,graph=T)
rL50_M_2014_8ab$L50
m_l50_M_8ab <- median(rL50_M_2014_8ab$L50)

#pb2 <- Data_MUR_L50%>%
#filter(sex=="M")%>%
#filter(year=="2014")

#2015                                                         #ne fonctionne pas                         
L50_M_2015_8ab <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2015")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2015_8ab<-fl50(L50_M_2015_8ab,niter=100,graph=T)
rL50_M_2015_8ab$L50
n_l50_M_8ab <- median(rL50_M_2015_8ab$L50)

#2016                                               #ne fonctionne pas tres bien
L50_M_2016_8ab <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2016")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2016_8ab<-fl50(L50_M_2016_8ab,niter=100,graph=T)
rL50_M_2016_8ab$L50
o_l50_M_8ab <- median(rL50_M_2016_8ab$L50)

#2017                                                                        #OK
L50_M_2017_8ab <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2017")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2017_8ab<-fl50(L50_M_2017_8ab,niter=100,graph=T)
rL50_M_2017_8ab$L50
p_l50_M_8ab <- median(rL50_M_2017_8ab$L50)

#2018                                                         #ne fonctionne pas
L50_M_2018_8ab <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2018")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2018_8ab<-fl50(L50_M_2018_8ab,niter=100,graph=T)
rL50_M_2018_8ab$L50
q_l50_M_8ab <- median(rL50_M_2018_8ab$L50)

#2019                                               #ne fonctionne pas tres bien
L50_M_2019_8ab <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2019")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2019_8ab<-fl50(L50_M_2019_8ab,niter=100,graph=T)
rL50_M_2019_8ab$L50
r_l50_M_8ab <- median(rL50_M_2019$L50)

#2021                                                         #ne fonctionne pas 
L50_M_2021_8ab <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2021")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2021_8ab<-fl50(L50_M_2021_8ab,niter=100,graph=T)
rL50_M_2021_8ab$L50
s_l50_M_8ab <- median(rL50_M_2021$L50)

#------------Tableau et graphiques de la L50 en fonction du temps---------------- 
year <- (2006:2021)
L50year_M_8ab <- c("", "","", "", "", "", k_l50_M_8ab, "", "", "", "", p_l50_M_8ab, "", "", "")
tabL50year_M_8ab <- data.frame(year, L50year_M_8ab)
tabL50year_M_8ab

plot(tabL50year_M_8ab)

ggplot(tabL50year_M_8ab,aes(x=year,y=L50year_M_8ab))+
  geom_point()+
  geom_path()+ 
  geom_smooth(method = "loess")

#---------------------------zone CIEM 7d----------------------------------------

#2006                                                         #ne fonctionne pas
L50_M_2006_7d <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2006")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2006_7d <-fl50(L50_M_2006_7d,niter=100,graph=T)
rL50_M_2006_7d$L50
e_l50_M_7d <- median(rL50_M_2006_7d$L50) 

#2007                                                         #ne fonctionne pas              
L50_M_2007_7d <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2007")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2007_7d<-fl50(L50_M_2007_7d,niter=100,graph=T)
rL50_M_2007_7d$L50
f_l50_M_7d <- median(rL50_M_2007_7d$L50)   

#2008                                                         #ne fonctionne pas
L50_M_2008_7d <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2008")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2008_7d<-fl50(L50_M_2008_7d,niter=100,graph=T)
rL50_M_2008_7d$L50
g_l50_M_7d <- median(rL50_M_2008_7d$L50)

#2009                                                         #ne fonctionne pas                            
L50_M_2009_7d <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2009")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2009_7d<-fl50(L50_M_2009_7d,niter=100,graph=T)
rL50_M_2009_7d$L50
h_l50_M_7d <- median(rL50_M_2009_7d$L50)

#2010                                                                        #OK                      
L50_M_2010_7d <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2010")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2010_7d<-fl50(L50_M_2010_7d,niter=100,graph=T)
rL50_M_2010_7d$L50
i_l50_M_7d <- median(rL50_M_2010_7d$L50)

#2011                                                                        #OK
L50_M_2011_7d <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2011")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2011_7d<-fl50(L50_M_2011_7d,niter=100,graph=T)
rL50_M_2011_7d$L50
j_l50_M_7d <- median(rL50_M_2011_7d$L50)

#2012                                                                        #OK
L50_M_2012_7d <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2012")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2012_7d<-fl50(L50_M_2012_7d,niter=100,graph=T)
rL50_M_2012_7d$L50
k_l50_M_7d <- median(rL50_M_2012_7d$L50)

#2013                                               #ne fonctionne pas tres bien 
L50_M_2013_7d <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2013")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2013_7d<-fl50(L50_M_2013_7d,niter=100,graph=T)
rL50_M_2013_7d$L50
l_l50_M_7d <- median(rL50_M_2013_7d$L50)

#2014                                                                        #OK                          
L50_M_2014_7d <- Data_MUR_L50%>%
  filter(sex=="M"  | sex =="I")%>%
  filter(year=="2014")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2014_7d<-fl50(L50_M_2014_7d,niter=100,graph=T)
rL50_M_2014_7d$L50
m_l50_M_7d <- median(rL50_M_2014_7d$L50)

#pb2 <- Data_MUR_L50%>%
#filter(sex=="M")%>%
#filter(year=="2014")

#2015                                                         #ne fonctionne pas                           
L50_M_2015_7d <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2015")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2015_7d<-fl50(L50_M_2015_7d,niter=100,graph=T)
rL50_M_2015_7d$L50
n_l50_M_7d <- median(rL50_M_2015_7d$L50)

#2016                                                         #ne fonctionne pas
L50_M_2016_7d <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2016")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2016_7d<-fl50(L50_M_2016_7d,niter=100,graph=T)
rL50_M_2016_7d$L50
o_l50_M_7d <- median(rL50_M_2016_7d$L50)

#2017                                                         #ne fonctionne pas
L50_M_2017_7d <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2017")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2017_7d<-fl50(L50_M_2017_7d,niter=100,graph=T)
rL50_M_2017_7d$L50
p_l50_M_7d <- median(rL50_M_2017_7d$L50)

#2018                                                         #ne fonctionne pas
L50_M_2018_7d <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2018")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2018_7d<-fl50(L50_M_2018_7d,niter=100,graph=T)
rL50_M_2018_7d$L50
q_l50_M_7d <- median(rL50_M_2018_7d$L50)

#2019                                                         #ne fonctionne pas
L50_M_2019_7d <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2019")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2019_7d<-fl50(L50_M_2019_7d,niter=100,graph=T)
rL50_M_2019_7d$L50
r_l50_M_7d <- median(rL50_M_2019_7d$L50)

#2021                                                         #ne fonctionne pas        
L50_M_2021_7d <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2021")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2021_7d<-fl50(L50_M_2021_7d,niter=100,graph=T)
rL50_M_2021_7d$L50
s_l50_M_7d <- median(rL50_M_2021_7d$L50)



#------------Tableau et graphiques de la L50 en fonction du temps---------------- 
year <- (2006:2021)
L50year_M_7d <- c("", "", "", "", i_l50_M_7d, j_l50_M_7d, k_l50_M_7d, "", m_l50_M_7d, "", "", "", "", "", "")
tabL50year_M_7d <- data.frame(year, L50year_M_7d)
tabL50year_M_7d

plot(tabL50year_M_7d)

ggplot(tabL50year_M_7d,aes(x=year,y=L50year_M_7d))+
  geom_point()+
  geom_path()+ 
  geom_smooth(method = "loess")



#---------------------------zone CIEM 4c----------------------------------------

#2006                                                         #ne fonctionne pas
L50_M_2006_4c <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2006")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2006_4c <-fl50(L50_M_2006_4c,niter=100,graph=T)
rL50_M_2006_4c$L50
e_l50_M_4c <- median(rL50_M_2006$L50) 

#2007                                               #ne fonctionne pas tres bien            
L50_M_2007_4c <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2007")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2007_4c<-fl50(L50_M_2007_4c,niter=100,graph=T)
rL50_M_2007_4c$L50
f_l50_M_4c <- median(rL50_M_2007_4c$L50)    

#2008                                                         #ne fonctionne pas 
L50_M_2008_4c <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2008")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2008_4c<-fl50(L50_M_2008_4c,niter=100,graph=T)
rL50_M_2008_4c$L50
g_l50_M_4c <- median(rL50_M_2008_4c$L50)

#2009                                                         #ne fonctionne pas                       
L50_M_2009_4c <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2009")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2009_4c<-fl50(L50_M_2009_4c,niter=100,graph=T)
rL50_M_2009_4c$L50
h_l50_M_4c <- median(rL50_M_2009_4c$L50)

#2010                                                         #ne fonctionne pas                         
L50_M_2010_4c <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2010")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2010_4c<-fl50(L50_M_2010_4c,niter=100,graph=T)
rL50_M_2010_4c$L50
i_l50_M_4c <- median(rL50_M_2010_4c$L50)

#2011                                                         #ne fonctionne pas 
L50_M_2011_4c <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2011")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2011_4c<-fl50(L50_M_2011_4c,niter=100,graph=T)
rL50_M_2011_4c$L50
j_l50_M_4c <- median(rL50_M_2011_4c$L50)

#2012                                                         #ne fonctionne pas 
L50_M_2012_4c <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2012")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2012_4c<-fl50(L50_M_2012_4c,niter=100,graph=T)
rL50_M_2012_4c$L50
k_l50_M_4c <- median(rL50_M_2012_4c$L50)

#2013                                                         #ne fonctionne pas 
L50_M_2013_4c <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2013")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2013_4c<-fl50(L50_M_2013_4c,niter=100,graph=T)
rL50_M_2013_4c$L50
l_l50_M_4c <- median(rL50_M_2013$L50)

#2014                             
L50_M_2014_4c <- Data_MUR_L50%>%
  filter(sex=="M"  | sex =="I")%>%
  filter(year=="2014")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2014_4c<-fl50(L50_M_2014_4c,niter=100,graph=T)
rL50_M_2014_4c$L50
m_l50_M_4c <- median(rL50_M_2014_4c$L50)

#pb2 <- Data_MUR_L50%>%
#filter(sex=="M")%>%
#filter(year=="2014")

#2015                                                         #ne fonctionne pas                         
L50_M_2015_4c <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2015")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2015_4c<-fl50(L50_M_2015_4c,niter=100,graph=T)
rL50_M_2015_4c$L50
n_l50_M_4c <- median(rL50_M_2015_4c$L50)

#2016                                                         #ne fonctionne pas 
L50_M_2016_4c <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2016")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2016_4c<-fl50(L50_M_2016_4c,niter=100,graph=T)
rL50_M_2016_4c$L50
o_l50_M_4c <- median(rL50_M_2016_4c$L50)

#2017                                                         #ne fonctionne pas 
L50_M_2017_4c <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2017")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2017_4c<-fl50(L50_M_2017_4c,niter=100,graph=T)
rL50_M_2017_4c$L50
p_l50_M_4c <- median(rL50_M_2017_4c$L50)

#2018                                                         #ne fonctionne pas 
L50_M_2018_4c <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2018")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2018_4c<-fl50(L50_M_2018_4c,niter=100,graph=T)
rL50_M_2018_4c$L50
q_l50_M_4c <- median(rL50_M_2018_4c$L50)

#2019                                                         #ne fonctionne pas 
L50_M_2019_4c <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2019")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2019_4c<-fl50(L50_M_2019_4c,niter=100,graph=T)
rL50_M_2019_4c$L50
r_l50_M_4c <- median(rL50_M_2019_4c$L50)
 
#2021                                                         #ne fonctionne pas  
L50_M_2021_4c <- Data_MUR_L50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2021")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
rL50_M_2021_4c<-fl50(L50_M_2021_4c,niter=100,graph=T)
rL50_M_2021_4c$L50
s_l50_M_4c <- median(rL50_M_2021_4c$L50)



#------------Tableau et graphiques de la L50 en fonction du temps---------------- 
year <- (2006:2021)
L50year_M_4c <- c("", "", "", "", "", "", "", "", "", "", "" , "", "", "", "", "")
tabL50year_M_4c <- data.frame(year, L50year_M_4c)
tabL50year_M_4c

plot(tabL50year_M_4c)

ggplot(tabL50year_M_4c,aes(x=year,y=L50year_M_4c))+
  geom_point()+
  geom_path()+ 
  geom_smooth(method = "loess")

#redflag dans le tableau, on fera un lissage dans le graphique 
#ajouter le nb de mature/immature pour chaque individu 
#mettre un critère ? 





#----------------------------------Femelle--------------------------------------
#-------Graphique de la proportion de mature/immature et valeur a50------------- 
#------------------------en fonction des zones---------------------------------- 

#Toutes zones confondues 
a50_F<- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  mutate(mature=as.factor(mature)) 
ra50_F<-fa50(a50_F,niter=100,graph=T)
a_a50_F <- median(ra50_F$a50)

#zone 8a-b - toutes années
a50_F_8ab <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_8ab<-fa50(a50_F_8ab,niter=100,graph=T)
b_a50_F <- median(ra50_F_8ab$a50)

#zone 7d - toutes années
a50_F_7d <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_7d<-fa50(a50_F_7d,niter=100,graph=T)
c_a50_F <- median(ra50_F_7d$a50)

#zone 4c - toutes années
a50_F_4c <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_4c<-fa50(a50_F_4c,niter=100,graph=T)
d_a50_F <- median(ra50_F_4c$a50)

#------------Tableau et graphiques de la a50 en fonction des zones--------------- 
area <- c("Toutes zones confondues", "zone 8a-b", "zone 7d", "zone 4c")
a50area_F <- c(a_a50_F, b_a50_F, c_a50_F, d_a50_F)
taba50area_F <- data.frame(area, a50area_F)
taba50area_F 

ggplot(taba50area_F ,aes(x=area,y=a50area_F))+
  geom_point()



#-------Graphique de la proportion de mature/immature et valeur a50------------- 
#---------------------pour chaque année 2006-2021-------------------------------

#---------------------------zone CIEM 8ab---------------------------------------

#2006
a50_F_2006_8ab <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2006")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2006_8ab <-fa50(a50_F_2006_8ab,niter=100,graph=T)
ra50_F_2006_8ab$a50
e_a50_F_8ab <- median(ra50_F_2006$a50) 

#2007              
a50_F_2007_8ab <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2007")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2007_8ab<-fa50(a50_F_2007_8ab,niter=100,graph=T)
ra50_F_2007_8ab$a50
f_a50_F_8ab <- median(ra50_F_2007_8ab$a50)    

#2008
a50_F_2008_8ab <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2008")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2008_8ab<-fa50(a50_F_2008_8ab,niter=100,graph=T)
ra50_F_2008_8ab$a50
g_a50_F_8ab <- median(ra50_F_2008_8ab$a50)

#2009                            
a50_F_2009_8ab <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2009")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2009_8ab<-fa50(a50_F_2009_8ab,niter=100,graph=T)
ra50_F_2009_8ab$a50
h_a50_F_8ab <- median(ra50_F_2009_8ab$a50)

#2010                        
a50_F_2010_8ab <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2010")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2010_8ab<-fa50(a50_F_2010_8ab,niter=100,graph=T)
ra50_F_2010_8ab$a50
i_a50_F_8ab <- median(ra50_F_2010_8ab$a50)

#2011
a50_F_2011_8ab <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2011")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2011_8ab<-fa50(a50_F_2011_8ab,niter=100,graph=T)
ra50_F_2011_8ab$a50
j_a50_F_8ab <- median(ra50_F_2011_8ab$a50)

#2012
a50_F_2012_8ab <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2012")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2012_8ab<-fa50(a50_F_2012_8ab,niter=100,graph=T)
ra50_F_2012_8ab$a50
k_a50_F_8ab <- median(ra50_F_2012_8ab$a50)

#2013
a50_F_2013_8ab <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2013")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2013_8ab<-fa50(a50_F_2013_8ab,niter=100,graph=T)
ra50_F_2013_8ab$a50
l_a50_F_8ab <- median(ra50_F_2013$a50)

#2014                              
a50_F_2014_8ab <- Data_MUR_a50%>%
  filter(sex=="F"  | sex =="I")%>%
  filter(year=="2014")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2014_8ab<-fa50(a50_F_2014_8ab,niter=100,graph=T)
ra50_F_2014_8ab$a50
m_a50_F_8ab <- median(ra50_F_2014_8ab$a50)

#pb2 <- Data_MUR_a50%>%
#filter(sex=="F")%>%
#filter(year=="2014")

#2015                         
a50_F_2015_8ab <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2015")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2015_8ab<-fa50(a50_F_2015_8ab,niter=100,graph=T)
ra50_F_2015_8ab$a50
n_a50_F_8ab <- median(ra50_F_2015_8ab$a50)

#2016
a50_F_2016_8ab <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2016")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2016_8ab<-fa50(a50_F_2016_8ab,niter=100,graph=T)
ra50_F_2016_8ab$a50
o_a50_F_8ab <- median(ra50_F_2016_8ab$a50)

#2017
a50_F_2017_8ab <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2017")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2017_8ab<-fa50(a50_F_2017_8ab,niter=100,graph=T)
ra50_F_2017_8ab$a50
p_a50_F_8ab <- median(ra50_F_2017_8ab$a50)

#2018
a50_F_2018_8ab <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2018")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2018_8ab<-fa50(a50_F_2018_8ab,niter=100,graph=T)
ra50_F_2018_8ab$a50
q_a50_F_8ab <- median(ra50_F_2018_8ab$a50)

#2019
a50_F_2019_8ab <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2019")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2019_8ab<-fa50(a50_F_2019_8ab,niter=100,graph=T)
ra50_F_2019_8ab$a50
r_a50_F_8ab <- median(ra50_F_2019$a50)

#2021         
a50_F_2021_8ab <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2021")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2021_8ab<-fa50(a50_F_2021_8ab,niter=100,graph=T)
ra50_F_2021_8ab$a50
s_a50_F_8ab <- median(ra50_F_2021$a50)

#------------Tableau et graphiques de la a50 en fonction du temps---------------- 
year <- (2006:2021)
a50year_F_8ab <- c("", "", "", "", "", j_a50_F_8ab, k_a50_F_8ab, l_a50_F_8ab, "", "", o_a50_F_8ab, 
                   p_a50_F_8ab, "", r_a50_F_8ab, "")
taba50year_F_8ab <- data.frame(year, a50year_F_8ab)
taba50year_F_8ab

plot(taba50year_F_8ab)

ggplot(taba50year_F_8ab,aes(x=year,y=a50year_F_8ab))+
  geom_point()+
  geom_path()+ 
  geom_smooth(method = "loess")



#---------------------------zone CIEM 7d----------------------------------------

#2006
a50_F_2006_7d <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2006")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2006_7d <-fa50(a50_F_2006_7d,niter=100,graph=T)
ra50_F_2006_7d$a50
e_a50_F_7d <- median(ra50_F_2006$a50) 

#2007              
a50_F_2007_7d <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2007")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2007_7d<-fa50(a50_F_2007_7d,niter=100,graph=T)
ra50_F_2007_7d$a50
f_a50_F_7d <- median(ra50_F_2007_7d$a50)  

#2008
a50_F_2008_7d <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2008")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2008_7d<-fa50(a50_F_2008_7d,niter=100,graph=T)
ra50_F_2008_7d$a50
g_a50_F_7d <- median(ra50_F_2008_7d$a50)

#2009                            
a50_F_2009_7d <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2009")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2009_7d<-fa50(a50_F_2009_7d,niter=100,graph=T)
ra50_F_2009_7d$a50
h_a50_F_7d <- median(ra50_F_2009_7d$a50)

#2010                        
a50_F_2010_7d <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2010")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2010_7d<-fa50(a50_F_2010_7d,niter=100,graph=T)
ra50_F_2010_7d$a50
i_a50_F_7d <- median(ra50_F_2010_7d$a50)

#2011
a50_F_2011_7d <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2011")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2011_7d<-fa50(a50_F_2011_7d,niter=100,graph=T)
ra50_F_2011_7d$a50
j_a50_F_7d <- median(ra50_F_2011_7d$a50)

#2012
a50_F_2012_7d <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2012")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2012_7d<-fa50(a50_F_2012_7d,niter=100,graph=T)
ra50_F_2012_7d$a50
k_a50_F_7d <- median(ra50_F_2012_7d$a50)

#2013
a50_F_2013_7d <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2013")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2013_7d<-fa50(a50_F_2013_7d,niter=100,graph=T)
ra50_F_2013_7d$a50
l_a50_F_7d <- median(ra50_F_2013$a50)

#2014                            
a50_F_2014_7d <- Data_MUR_a50%>%
  filter(sex=="F"  | sex =="I")%>%
  filter(year=="2014")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2014_7d<-fa50(a50_F_2014_7d,niter=100,graph=T)
ra50_F_2014_7d$a50
m_a50_F_7d <- median(ra50_F_2014_7d$a50)

#pb2 <- Data_MUR_a50%>%
#filter(sex=="F")%>%
#filter(year=="2014")

#2015                          
a50_F_2015_7d <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2015")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2015_7d<-fa50(a50_F_2015_7d,niter=100,graph=T)
ra50_F_2015_7d$a50
n_a50_F_7d <- median(ra50_F_2015_7d$a50)

#2016
a50_F_2016_7d <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2016")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2016_7d<-fa50(a50_F_2016_7d,niter=100,graph=T)
ra50_F_2016_7d$a50
o_a50_F_7d <- median(ra50_F_2016_7d$a50)

#2017
a50_F_2017_7d <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2017")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2017_7d<-fa50(a50_F_2017_7d,niter=100,graph=T)
ra50_F_2017_7d$a50
p_a50_F_7d <- median(ra50_F_2017_7d$a50)

#2018
a50_F_2018_7d <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2018")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2018_7d<-fa50(a50_F_2018_7d,niter=100,graph=T)
ra50_F_2018_7d$a50
q_a50_F_7d <- median(ra50_F_2018_7d$a50)

#2019
a50_F_2019_7d <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2019")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2019_7d<-fa50(a50_F_2019_7d,niter=100,graph=T)
ra50_F_2019_7d$a50
r_a50_F_7d <- median(ra50_F_2019$a50)

#2021         
a50_F_2021_7d <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2021")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2021_7d<-fa50(a50_F_2021_7d,niter=100,graph=T)
ra50_F_2021_7d$a50
s_a50_F_7d <- median(ra50_F_2021$a50)



#------------Tableau et graphiques de la a50 en fonction du temps---------------- 
year <- (2006:2021)
a50year_F_7d <- c(e_a50_F_7d, "", "", "", i_a50_F_7d, j_a50_F_7d, k_a50_F_7d, "", 
                  "", "", o_a50_F_7d, p_a50_F_7d, q_a50_F_7d, "", "")
taba50year_F_7d <- data.frame(year, a50year_F_7d)
taba50year_F_7d

plot(taba50year_F_7d)

ggplot(taba50year_F_7d,aes(x=year,y=a50year_F_7d))+
  geom_point()+
  geom_path()+ 
  geom_smooth(method = "loess")



#---------------------------zone CIEM 4c----------------------------------------

#2006
a50_F_2006_4c <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2006")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2006_4c <-fa50(a50_F_2006_4c,niter=100,graph=T)
ra50_F_2006_4c$a50
e_a50_F_4c <- median(ra50_F_2006$a50)

#2007              
a50_F_2007_4c <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2007")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2007_4c<-fa50(a50_F_2007_4c,niter=100,graph=T)
ra50_F_2007_4c$a50
f_a50_F_4c <- median(ra50_F_2007_4c$a50)   

#2008
a50_F_2008_4c <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2008")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2008_4c<-fa50(a50_F_2008_4c,niter=100,graph=T)
ra50_F_2008_4c$a50
g_a50_F_4c <- median(ra50_F_2008_4c$a50)

#2009                            
a50_F_2009_4c <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2009")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2009_4c<-fa50(a50_F_2009_4c,niter=100,graph=T)
ra50_F_2009_4c$a50
h_a50_F_4c <- median(ra50_F_2009_4c$a50)

#2010                        
a50_F_2010_4c <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2010")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2010_4c<-fa50(a50_F_2010_4c,niter=100,graph=T)
ra50_F_2010_4c$a50
i_a50_F_4c <- median(ra50_F_2010_4c$a50)

#2011
a50_F_2011_4c <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2011")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2011_4c<-fa50(a50_F_2011_4c,niter=100,graph=T)
ra50_F_2011_4c$a50
j_a50_F_4c <- median(ra50_F_2011_4c$a50)

#2012
a50_F_2012_4c <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2012")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2012_4c<-fa50(a50_F_2012_4c,niter=100,graph=T)
ra50_F_2012_4c$a50
k_a50_F_4c <- median(ra50_F_2012_4c$a50)

#2013
a50_F_2013_4c <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2013")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2013_4c<-fa50(a50_F_2013_4c,niter=100,graph=T)
ra50_F_2013_4c$a50
l_a50_F_4c <- median(ra50_F_2013$a50)

#2014                            
a50_F_2014_4c <- Data_MUR_a50%>%
  filter(sex=="F"  | sex =="I")%>%
  filter(year=="2014")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2014_4c<-fa50(a50_F_2014_4c,niter=100,graph=T)
ra50_F_2014_4c$a50
m_a50_F_4c <- median(ra50_F_2014_4c$a50)

#pb2 <- Data_MUR_a50%>%
#filter(sex=="F")%>%
#filter(year=="2014")

#2015                          
a50_F_2015_4c <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2015")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2015_4c<-fa50(a50_F_2015_4c,niter=100,graph=T)
ra50_F_2015_4c$a50
n_a50_F_4c <- median(ra50_F_2015_4c$a50)

#2016
a50_F_2016_4c <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2016")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2016_4c<-fa50(a50_F_2016_4c,niter=100,graph=T)
ra50_F_2016_4c$a50
o_a50_F_4c <- median(ra50_F_2016_4c$a50)

#2017
a50_F_2017_4c <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2017")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2017_4c<-fa50(a50_F_2017_4c,niter=100,graph=T)
ra50_F_2017_4c$a50
p_a50_F_4c <- median(ra50_F_2017_4c$a50)

#2018
a50_F_2018_4c <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2018")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2018_4c<-fa50(a50_F_2018_4c,niter=100,graph=T)
ra50_F_2018_4c$a50
q_a50_F_4c <- median(ra50_F_2018_4c$a50)

#2019
a50_F_2019_4c <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2019")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2019_4c<-fa50(a50_F_2019_4c,niter=100,graph=T)
ra50_F_2019_4c$a50
r_a50_F_4c <- median(ra50_F_2019$a50)

#2021          
a50_F_2021_4c <- Data_MUR_a50%>%
  filter(sex=="F" | sex =="I")%>%
  filter(year=="2021")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_F_2021_4c<-fa50(a50_F_2021_4c,niter=100,graph=T)
ra50_F_2021_4c$a50
s_a50_F_4c <- median(ra50_F_2021$a50)



#------------Tableau et graphiques de la a50 en fonction du temps---------------- 
year <- (2006:2021)
a50year_F_4c <- c(e_a50_F_4c, "", "", "", i_a50_F_4c, j_a50_F_4c, k_a50_F_4c, "", "", "", 
                  o_a50_F_4c, p_a50_F_4c, q_a50_F_4c, "", "")
taba50year_F_4c <- data.frame(year, a50year_F_4c)
taba50year_F_4c

plot(taba50year_F_4c)

ggplot(taba50year_F_4c,aes(x=year,y=a50year_F_4c))+
  geom_point()+
  geom_path()+ 
  geom_smooth(method = "loess")



#------------------------------------Male---------------------------------------

#-------Graphique de la proportion de mature/immature et valeur a50------------- 
#------------------------en fonction des zones---------------------------------- 

#Toutes zones confondues 
a50_M<- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  mutate(mature=as.factor(mature)) 
ra50_M<-fa50(a50_M,niter=100,graph=T)
a_a50_M <- median(ra50_M$a50)

#zone 8a-b - toutes années
a50_M_8ab <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_8ab<-fa50(a50_M_8ab,niter=100,graph=T)
b_a50_M <- median(ra50_M_8ab$a50)

#zone 7d - toutes années
a50_M_7d <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_7d<-fa50(a50_M_7d,niter=100,graph=T)
c_a50_M <- median(ra50_M_7d$a50)

#zone 4c - toutes années
a50_M_4c <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_4c<-fa50(a50_M_4c,niter=100,graph=T)
d_a50_M <- median(ra50_M_4c$a50)

#------------Tableau et graphiques de la a50 en fonction des zones--------------- 
area <- c("Toutes zones confondues", "zone 8a-b", "zone 7d", "zone 4c")
a50area_M <- c(a_a50_M, b_a50_M, c_a50_M, d_a50_M)
taba50area_M <- data.frame(area, a50area_M)
taba50area_M 

ggplot(taba50area_M ,aes(x=area,y=a50area_M))+
  geom_point()



#-------Graphique de la proportion de mature/immature et valeur a50------------- 
#---------------------pour chaque année 2006-2021-------------------------------

#---------------------------zone CIEM 8ab---------------------------------------

#2006
a50_M_2006_8ab <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2006")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2006_8ab <-fa50(a50_M_2006_8ab,niter=100,graph=T)
ra50_M_2006_8ab$a50
e_a50_M_8ab <- median(ra50_M_2006$a50) 

#2007              
a50_M_2007_8ab <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2007")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2007_8ab<-fa50(a50_M_2007_8ab,niter=100,graph=T)
ra50_M_2007_8ab$a50
f_a50_M_8ab <- median(ra50_M_2007_8ab$a50) 

#2008
a50_M_2008_8ab <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2008")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2008_8ab<-fa50(a50_M_2008_8ab,niter=100,graph=T)
ra50_M_2008_8ab$a50
g_a50_M_8ab <- median(ra50_M_2008_8ab$a50)

#2009                             
a50_M_2009_8ab <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2009")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2009_8ab<-fa50(a50_M_2009_8ab,niter=100,graph=T)
ra50_M_2009_8ab$a50
h_a50_M_8ab <- median(ra50_M_2009_8ab$a50)

#2010                        
a50_M_2010_8ab <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2010")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2010_8ab<-fa50(a50_M_2010_8ab,niter=100,graph=T)
ra50_M_2010_8ab$a50
i_a50_M_8ab <- median(ra50_M_2010_8ab$a50)

#2011
a50_M_2011_8ab <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2011")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2011_8ab<-fa50(a50_M_2011_8ab,niter=100,graph=T)
ra50_M_2011_8ab$a50
j_a50_M_8ab <- median(ra50_M_2011_8ab$a50)

#2012
a50_M_2012_8ab <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2012")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2012_8ab<-fa50(a50_M_2012_8ab,niter=100,graph=T)
ra50_M_2012_8ab$a50
k_a50_M_8ab <- median(ra50_M_2012_8ab$a50)

#2013
a50_M_2013_8ab <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2013")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2013_8ab<-fa50(a50_M_2013_8ab,niter=100,graph=T)
ra50_M_2013_8ab$a50
l_a50_M_8ab <- median(ra50_M_2013$a50)

#2014                               
a50_M_2014_8ab <- Data_MUR_a50%>%
  filter(sex=="M"  | sex =="I")%>%
  filter(year=="2014")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2014_8ab<-fa50(a50_M_2014_8ab,niter=100,graph=T)
ra50_M_2014_8ab$a50
m_a50_M_8ab <- median(ra50_M_2014_8ab$a50)

#pb2 <- Data_MUR_a50%>%
#filter(sex=="M")%>%
#filter(year=="2014")

#2015                         
a50_M_2015_8ab <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2015")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2015_8ab<-fa50(a50_M_2015_8ab,niter=100,graph=T)
ra50_M_2015_8ab$a50
n_a50_M_8ab <- median(ra50_M_2015_8ab$a50)

#2016
a50_M_2016_8ab <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2016")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2016_8ab<-fa50(a50_M_2016_8ab,niter=100,graph=T)
ra50_M_2016_8ab$a50
o_a50_M_8ab <- median(ra50_M_2016_8ab$a50)

#2017
a50_M_2017_8ab <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2017")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2017_8ab<-fa50(a50_M_2017_8ab,niter=100,graph=T)
ra50_M_2017_8ab$a50
p_a50_M_8ab <- median(ra50_M_2017_8ab$a50)

#2018
a50_M_2018_8ab <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2018")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2018_8ab<-fa50(a50_M_2018_8ab,niter=100,graph=T)
ra50_M_2018_8ab$a50
q_a50_M_8ab <- median(ra50_M_2018_8ab$a50)

#2019
a50_M_2019_8ab <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2019")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2019_8ab<-fa50(a50_M_2019_8ab,niter=100,graph=T)
ra50_M_2019_8ab$a50
r_a50_M_8ab <- median(ra50_M_2019$a50)

#2021          
a50_M_2021_8ab <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2021")%>%
  filter(newarea=="8a-b")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2021_8ab<-fa50(a50_M_2021_8ab,niter=100,graph=T)
ra50_M_2021_8ab$a50
s_a50_M_8ab <- median(ra50_M_2021$a50)

#------------Tableau et graphiques de la a50 en fonction du temps---------------- 
year <- (2006:2021)
a50year_M_8ab <- c("", "", g_a50_M_8ab, h_a50_M_8ab, "", "", k_a50_M_8ab, "", "", "", "", p_a50_M_8ab, "", "", "")
taba50year_M_8ab <- data.frame(year, a50year_M_8ab)
taba50year_M_8ab

plot(taba50year_M_8ab)

ggplot(taba50year_M_8ab,aes(x=year,y=a50year_M_8ab))+
  geom_point()+
  geom_path()+ 
  geom_smooth(method = "loess")



#---------------------------zone CIEM 7d----------------------------------------

#2006
a50_M_2006_7d <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2006")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2006_7d <-fa50(a50_M_2006_7d,niter=100,graph=T)
ra50_M_2006_7d$a50
e_a50_M_7d <- median(ra50_M_2006$a50) 

#2007              
a50_M_2007_7d <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2007")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2007_7d<-fa50(a50_M_2007_7d,niter=100,graph=T)
ra50_M_2007_7d$a50
f_a50_M_7d <- median(ra50_M_2007_7d$a50)   

#2008
a50_M_2008_7d <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2008")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2008_7d<-fa50(a50_M_2008_7d,niter=100,graph=T)
ra50_M_2008_7d$a50
g_a50_M_7d <- median(ra50_M_2008_7d$a50)

#2009                             
a50_M_2009_7d <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2009")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2009_7d<-fa50(a50_M_2009_7d,niter=100,graph=T)
ra50_M_2009_7d$a50
h_a50_M_7d <- median(ra50_M_2009_7d$a50)

#2010                        
a50_M_2010_7d <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2010")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2010_7d<-fa50(a50_M_2010_7d,niter=100,graph=T)
ra50_M_2010_7d$a50
i_a50_M_7d <- median(ra50_M_2010_7d$a50)

#2011
a50_M_2011_7d <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2011")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2011_7d<-fa50(a50_M_2011_7d,niter=100,graph=T)
ra50_M_2011_7d$a50
j_a50_M_7d <- median(ra50_M_2011_7d$a50)

#2012
a50_M_2012_7d <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2012")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2012_7d<-fa50(a50_M_2012_7d,niter=100,graph=T)
ra50_M_2012_7d$a50
k_a50_M_7d <- median(ra50_M_2012_7d$a50)

#2013
a50_M_2013_7d <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2013")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2013_7d<-fa50(a50_M_2013_7d,niter=100,graph=T)
ra50_M_2013_7d$a50
l_a50_M_7d <- median(ra50_M_2013$a50)

#2014                              
a50_M_2014_7d <- Data_MUR_a50%>%
  filter(sex=="M"  | sex =="I")%>%
  filter(year=="2014")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2014_7d<-fa50(a50_M_2014_7d,niter=100,graph=T)
ra50_M_2014_7d$a50
m_a50_M_7d <- median(ra50_M_2014_7d$a50)

#pb2 <- Data_MUR_a50%>%
#filter(sex=="M")%>%
#filter(year=="2014")

#2015                           
a50_M_2015_7d <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2015")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2015_7d<-fa50(a50_M_2015_7d,niter=100,graph=T)
ra50_M_2015_7d$a50
n_a50_M_7d <- median(ra50_M_2015_7d$a50)

#2016
a50_M_2016_7d <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2016")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2016_7d<-fa50(a50_M_2016_7d,niter=100,graph=T)
ra50_M_2016_7d$a50
o_a50_M_7d <- median(ra50_M_2016_7d$a50)

#2017
a50_M_2017_7d <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2017")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2017_7d<-fa50(a50_M_2017_7d,niter=100,graph=T)
ra50_M_2017_7d$a50
p_a50_M_7d <- median(ra50_M_2017_7d$a50)

#2018
a50_M_2018_7d <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2018")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2018_7d<-fa50(a50_M_2018_7d,niter=100,graph=T)
ra50_M_2018_7d$a50
q_a50_M_7d <- median(ra50_M_2018_7d$a50)

#2019
a50_M_2019_7d <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2019")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2019_7d<-fa50(a50_M_2019_7d,niter=100,graph=T)
ra50_M_2019_7d$a50
r_a50_M_7d <- median(ra50_M_2019$a50)

#2021          
a50_M_2021_7d <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2021")%>%
  filter(newarea=="7d")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2021_7d<-fa50(a50_M_2021_7d,niter=100,graph=T)
ra50_M_2021_7d$a50
s_a50_M_7d <- median(ra50_M_2021$a50)



#------------Tableau et graphiques de la a50 en fonction du temps---------------- 
year <- (2006:2021)
a50year_M_7d <- c("", "", "", "", i_a50_M_7d, j_a50_M_7d, k_a50_M_7d, "", m_a50_M_7d, "", "", "", "", "", "")
taba50year_M_7d <- data.frame(year, a50year_M_7d)
taba50year_M_7d

plot(taba50year_M_7d)

ggplot(taba50year_M_7d,aes(x=year,y=a50year_M_7d))+
  geom_point()+
  geom_path()+ 
  geom_smooth(method = "loess")



#---------------------------zone CIEM 4c----------------------------------------

#2006
a50_M_2006_4c <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2006")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2006_4c <-fa50(a50_M_2006_4c,niter=100,graph=T)
ra50_M_2006_4c$a50
e_a50_M_4c <- median(ra50_M_2006$a50) 

#2007              
a50_M_2007_4c <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2007")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2007_4c<-fa50(a50_M_2007_4c,niter=100,graph=T)
ra50_M_2007_4c$a50
f_a50_M_4c <- median(ra50_M_2007_4c$a50)    

#2008
a50_M_2008_4c <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2008")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2008_4c<-fa50(a50_M_2008_4c,niter=100,graph=T)
ra50_M_2008_4c$a50
g_a50_M_4c <- median(ra50_M_2008_4c$a50)

#2009                             
a50_M_2009_4c <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2009")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2009_4c<-fa50(a50_M_2009_4c,niter=100,graph=T)
ra50_M_2009_4c$a50
h_a50_M_4c <- median(ra50_M_2009_4c$a50)

#2010                        
a50_M_2010_4c <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2010")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2010_4c<-fa50(a50_M_2010_4c,niter=100,graph=T)
ra50_M_2010_4c$a50
i_a50_M_4c <- median(ra50_M_2010_4c$a50)

#2011
a50_M_2011_4c <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2011")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2011_4c<-fa50(a50_M_2011_4c,niter=100,graph=T)
ra50_M_2011_4c$a50
j_a50_M_4c <- median(ra50_M_2011_4c$a50)

#2012
a50_M_2012_4c <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2012")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2012_4c<-fa50(a50_M_2012_4c,niter=100,graph=T)
ra50_M_2012_4c$a50
k_a50_M_4c <- median(ra50_M_2012_4c$a50)

#2013
a50_M_2013_4c <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2013")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2013_4c<-fa50(a50_M_2013_4c,niter=100,graph=T)
ra50_M_2013_4c$a50
l_a50_M_4c <- median(ra50_M_2013$a50)

#2014                             
a50_M_2014_4c <- Data_MUR_a50%>%
  filter(sex=="M"  | sex =="I")%>%
  filter(year=="2014")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2014_4c<-fa50(a50_M_2014_4c,niter=100,graph=T)
ra50_M_2014_4c$a50
m_a50_M_4c <- median(ra50_M_2014_4c$a50)

#pb2 <- Data_MUR_a50%>%
#filter(sex=="M")%>%
#filter(year=="2014")

#2015                         
a50_M_2015_4c <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2015")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2015_4c<-fa50(a50_M_2015_4c,niter=100,graph=T)
ra50_M_2015_4c$a50
n_a50_M_4c <- median(ra50_M_2015_4c$a50)

#2016
a50_M_2016_4c <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2016")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2016_4c<-fa50(a50_M_2016_4c,niter=100,graph=T)
ra50_M_2016_4c$a50
o_a50_M_4c <- median(ra50_M_2016_4c$a50)

#2017
a50_M_2017_4c <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2017")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2017_4c<-fa50(a50_M_2017_4c,niter=100,graph=T)
ra50_M_2017_4c$a50
p_a50_M_4c <- median(ra50_M_2017_4c$a50)

#2018
a50_M_2018_4c <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2018")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2018_4c<-fa50(a50_M_2018_4c,niter=100,graph=T)
ra50_M_2018_4c$a50
q_a50_M_4c <- median(ra50_M_2018_4c$a50)

#2019
a50_M_2019_4c <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2019")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2019_4c<-fa50(a50_M_2019_4c,niter=100,graph=T)
ra50_M_2019_4c$a50
r_a50_M_4c <- median(ra50_M_2019$a50)

#2021          
a50_M_2021_4c <- Data_MUR_a50%>%
  filter(sex=="M" | sex =="I")%>%
  filter(year=="2021")%>%
  filter(newarea=="4c")%>%
  mutate(mature=as.factor(mature)) 
ra50_M_2021_4c<-fa50(a50_M_2021_4c,niter=100,graph=T)
ra50_M_2021_4c$a50
s_a50_M_4c <- median(ra50_M_2021$a50)



#------------Tableau et graphiques de la a50 en fonction du temps---------------- 
year <- (2006:2021)
a50year_M_4c <- c("", f_a50_M_4c, g_a50_M_4c, "", "", "", "", "", "", n_a50_M_4c, "" , "", "", "", "")
taba50year_M_4c <- data.frame(year, a50year_M_4c)
taba50year_M_4c

plot(taba50year_M_4c)

ggplot(taba50year_M_4c,aes(x=year,y=a50year_M_4c))+
  geom_point()+
  geom_path()+ 
  geom_smooth(method = "loess")

#redflag dans le tableau, on fera un lissage dans le graphique 
#ajouter le nb de mature/immature pour chaque individu 
#mettre un critère ? 








































































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
       femelles matures et immatures entre 2006 et 2021",
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
       femelles matures et immatures entre 2006 et 2021",
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
       femelles matures et immatures entre 2006 et 2021",
       subtitle = "Zones geographiques de peche : 8a-b",
       caption = "Barres d'erreur: erreur standard",
       x = "Age (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")


#----------------------Zone geographique 4c-----------------------------------
ggplot(data = DataMatu_MUR_F_NdeR %>%
         filter(newarea == "4c"), 
       mapping = aes(x = meanAge, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()+
  #geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5) +
  labs(title = "Evolution de la moyenne de la longeur totale en fonction l'age pour les \n individus 
       femelles matures et immatures entre 2006 et 2021",
       subtitle = "Zones geographiques de peche : 4c",
       caption = "Barres d'erreur: erreur standard",
       x = "Age (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#----------------------Zone geographique 7d-----------------------------------
ggplot(data = DataMatu_MUR_F_NdeR %>%
         filter(newarea == "7d"), 
       mapping = aes(x = meanAge, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()+
  #geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5) +
  labs(title = "Evolution de la moyenne de la longeur totale en fonction l'age pour les \n individus 
       femelles matures et immatures entre 2006 et 2021",
       subtitle = "Zones geographiques de peche : 7d",
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
       individus males matures et immatures entre 2006 et 2021",
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
  individus males matures et immatures entre 2006 et 2021",
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
       males matures et immatures entre 2006 et 2021",
       subtitle = "Zones geographiques de peche : 8a-b",
       caption = "Barres d'erreur: erreur standard",
       x = "Age (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#----------------------Zone geographique 4c-----------------------------------


ggplot(data = DataMatu_MUR_M_NdeR %>%
         filter(newarea == "4c"), 
       mapping = aes(x = meanAge, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()+
  #geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5) +
  labs(title = "Evolution de la moyenne de la longeur totale en fonction l'age pour les \n individus 
       males matures et immatures entre 2006 et 2021",
       subtitle = "Zones geographiques de peche : 4c",
       caption = "Barres d'erreur: erreur standard",
       x = "Age (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")

#----------------------Zone geographique 7d-----------------------------------


ggplot(data = DataMatu_MUR_M_NdeR %>%
         filter(newarea == "7d"), 
       mapping = aes(x = meanAge, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()+
  #geom_errorbar(aes(ymin = meanLen - stdErrorLen, ymax = meanLen + stdErrorLen), width = 0.5) +
  labs(title = "Evolution de la moyenne de la longeur totale en fonction l'age pour les \n individus 
       males matures et immatures entre 2006 et 2021",
       subtitle = "Zones geographiques de peche : 7d",
       caption = "Barres d'erreur: erreur standard",
       x = "Age (an)",
       y = "Longueur totale de l'individu (mm)",
       color = "Maturite de l'individu")



#-------------------------------Plot en 3D--------------------------------------



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



plot3d(DataMatu_MUR_F_NdeR$year,
       DataMatu_MUR_F_NdeR$meanLen,
       DataMatu_MUR_F_NdeR$meanAge,
       xlab="year", ylab="mean of length" ,zlab="mean of age", 
       col = DataMatu_MUR_F_NdeR$mat) #ne fonctionne pas


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



