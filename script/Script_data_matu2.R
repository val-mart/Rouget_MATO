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

DataMatu <- readRDS("./Data/ca.rds")


#Travail exploratoire sur : 

#year 
#quarter
#month ? 
#sex (garder que F et M)
#area 8.a 8.b, 7.e 7.d
#lenCls 
#age (enlever peut être -1)
#indWt : poids individu 
#matScale 
#matStage (enlever peut-être -1)


#Filter l'espece + Area 
DataMatu_MUR <- DataMatu %>% 
  filter(spp == "Mullus surmuletus") 


#Barplot echelles de maturite
ggplot(DataMatu_MUR, mapping = aes(x = matScale)) +
  geom_bar()

#Barplot stades de maturite
ggplot(DataMatu_MUR, mapping = aes(x = matStage)) +
  geom_bar()

#Barplot  area
ggplot(DataMatu_MUR, mapping = aes(x = area)) +
  geom_bar()
table(DataMatu_MUR$year,DataMatu_MUR$area)

#Pas mal d'info pour 7.d, 8.a, 8.b et 4.c et b
ggplot(DataMatu_MUR, mapping = aes(x = newarea)) +
  geom_bar()



DataMatu_MUR <- DataMatu_MUR %>% 
  filter(str_detect(area, "^27.")) %>% 
  filter(matStage !="-1")%>%   
  filter(matStage != "0")%>% 
  filter(age !="-1")%>%  
  filter(sex != "-1")%>%
  filter(sex != "I")%>%
  mutate(newarea = fct_collapse (area, "8a-b" =  c("27.8.a", "27.8.b","27.8.a,27.8.b"), 
                                 "7d-e" =  c("27.7.d", "27.7.d,27.7.e", "27.7.e"), 
                                 "7f-h et j " = c("27.7.g,27.7.h,27.7.j", "27.7.h", "27.7.f", 
                                                      "27.7.g", "27.7.j"), 
                                 "8c-d" =  c("27.8.c", "27.8.d"), 
                                 "4b-c" =  c("27.4.b", "27.4.c")))

table(DataMatu_MUR$matStage,DataMatu_MUR$year)
table(DataMatu_MUR$matScale,DataMatu_MUR$year)
table(DataMatu_MUR$year,DataMatu_MUR$newarea)

#View(table(DataMatu_MUR$newarea))


# => Plus de precisions dans les stades de maturite a partir de 2015



#Voir evolution de ces stades dans le temps et en fonction des zones 

#------------------------------------Femelle------------------------------------

#------------------Zone geographique 8.a et 8.b---------------------------------

ggplot(DataMatu_MUR %>% 
         filter(sex== "F") %>%
         filter(newarea =="8a-b"), mapping = aes(x = matStage)) +
  geom_bar()+
  facet_wrap(~year)

#------------------Zone geographique 7.d et e-----------------------------------

ggplot(DataMatu_MUR %>% 
         filter(sex== "F") %>%
         filter(newarea =="7d-e"), mapping = aes(x = matStage)) +
  geom_bar()+
  facet_wrap(~year)

#------------------Zone geographique 4.c et b-----------------------------------
ggplot(DataMatu_MUR %>% 
         filter(sex== "F") %>%
         filter(area =="4b-c"), mapping = aes(x = matStage)) +
  geom_bar()+
  facet_wrap(~year)

#-------------------------------------------------------------------------------
#-----------------En simplifiant les stades de maturite-------------------------
#-------------------------------------------------------------------------------

DataMatu_MUR2 <- DataMatu_MUR %>% 
  mutate(mat=ifelse(matStage == "1", "Immature",
                    ifelse (matStage == "2", "Immature",
                            ifelse (matStage == "2a", "Immature", "Mature"))))

table(DataMatu_MUR2$mat)
table(DataMatu_MUR2$mat,DataMatu_MUR2$year)


#------------------Zone geographique 8.a et 8.b---------------------------------
ggplot(DataMatu_MUR2 %>% 
         filter(sex == "F") %>%
         filter(newarea == "8a-b"), mapping = aes(x = mat)) +
  geom_bar()+
  facet_wrap(~year)
#------------------Zone geographique 7.d et e-----------------------------------
ggplot(DataMatu_MUR2 %>% 
         filter(sex== "F") %>%
         filter(newarea == "7d-e"), mapping = aes(x = mat)) +
  geom_bar()+
  facet_wrap(~year)
#------------------Zone geographique 4.c et b-----------------------------------
ggplot(DataMatu_MUR2 %>% 
         filter(sex== "F") %>%
         filter(newarea =="4b-c"), mapping = aes(x = mat)) +
  geom_bar()+
  facet_wrap(~year)


#-------------------------------------------------------------------------------
#--------------Evolution longueur en fonction du temps mat/immature------------- 

ggplot(DataMatu_MUR2 %>% 
         filter(sex== "F"), aes(x = factor(year), y = lenCls)) +
  geom_boxplot() + 
  facet_wrap(~mat)

#------------------Toutes zones geographiques-----------------------------------
ggplot(DataMatu_MUR2%>%
         filter(sex== "F") %>%
         filter(newarea %in% c("8a-b" , "7d-e" , "4b-c"))
       ,aes(x=lenCls,y=newarea,color=mat))+
  geom_point()+
  facet_wrap(~year)

ggplot(DataMatu_MUR2%>%
         filter(sex== "F") %>%
         filter(newarea %in% c("8a-b" , "7d-e" , "4b-c"))
       ,aes(x=lenCls,y=mat,color=newarea))+
  geom_point()+
  facet_wrap(~year)

#Zoom 
ggplot(DataMatu_MUR2%>%
         filter(sex== "F") %>%
         filter(newarea %in% c("8a-b" , "7d-e" , "4b-c"))
       ,aes(x=lenCls,y=mat,color=newarea))+
  geom_point()+
  facet_wrap(~year) + 
  xlim(100,450)


#--------------------Zone geographique 8.a et 8.b-------------------------------
ggplot(DataMatu_MUR2%>%
         filter(sex== "F") %>%
         filter(newarea =="8a-b")
       ,aes(x=lenCls,y=mat,color=mat))+
  geom_point()+
  facet_wrap(~year)
# Zoom + xlim(100,450)

#------------------Zone geographique 7.d et e-----------------------------------
ggplot(DataMatu_MUR2 %>% 
         filter(sex== "F") %>%
         filter(newarea == "7d-e")
       ,aes(x=lenCls,y=mat,color=mat))+
  geom_point()+
  facet_wrap(~year)

#---------------------Zone geographique 4.c et b--------------------------------
ggplot(DataMatu_MUR2%>%
         filter(sex== "F") %>%
         filter(newarea =="4b-c")
       ,aes(x=lenCls,y=mat,color=mat))+
  geom_point()+
  facet_wrap(~year)


#------------------------------------Male---------------------------------------

#------------------Zone geographique 8.a et 8.b---------------------------------

ggplot(DataMatu_MUR %>% 
         filter(sex== "M") %>%
         filter(newarea =="8a-b"), mapping = aes(x = matStage)) +
  geom_bar()+
  facet_wrap(~year)

#------------------Zone geographique 7.d----------------------------------------

ggplot(DataMatu_MUR %>% 
         filter(sex== "M") %>%
         filter(newarea =="7d-e"), mapping = aes(x = matStage)) +
  geom_bar()+
  facet_wrap(~year)

#------------------Zone geographique 4.c et b-----------------------------------
ggplot(DataMatu_MUR %>% 
         filter(sex== "M") %>%
         filter(area =="4b-c"), mapping = aes(x = matStage)) +
  geom_bar()+
  facet_wrap(~year)

#-------------------------------------------------------------------------------
#-----------------En simplifiant les stades de maturite-------------------------
#-------------------------------------------------------------------------------


#------------------Zone geographique 8.a et 8.b---------------------------------
ggplot(DataMatu_MUR2 %>% 
         filter(sex == "M") %>%
         filter(newarea == "8a-b"), mapping = aes(x = mat)) +
  geom_bar()+
  facet_wrap(~year)
#------------------Zone geographique 7.d et e-----------------------------------
ggplot(DataMatu_MUR2 %>% 
         filter(sex== "M") %>%
         filter(newarea == "7d-e"), mapping = aes(x = mat)) +
  geom_bar()+
  facet_wrap(~year)
#------------------Zone geographique 4.c et b----------------------------------------
ggplot(DataMatu_MUR2 %>% 
         filter(sex== "M") %>%
         filter(newarea =="4b-c"), mapping = aes(x = mat)) +
  geom_bar()+
  facet_wrap(~year)

#-------------------------------------------------------------------------------
#--------------Evolution longueur en fonction du temps mat/immature------------- 

#ggplot(DataMatu_MUR2 %>% 
#         filter(sex== "M"), aes(x = factor(year), y = lenCls)) +
#  geom_boxplot() + 
#  facet_wrap(~mat)

#------------------Toutes zones geographiques-----------------------------------
ggplot(DataMatu_MUR2%>%
         filter(sex== "M") %>%
         filter(newarea %in% c("8a-b" , "7d-e" , "4b-c"))
       ,aes(x=lenCls,y=newarea,color=mat))+
  geom_point()+
  facet_wrap(~year)

ggplot(DataMatu_MUR2%>%
         filter(sex== "M") %>%
         filter(newarea %in% c("8a-b" , "7d-e" , "4b-c"))
       ,aes(x=lenCls,y=mat,color=newarea))+
  geom_point()+
  facet_wrap(~year)

#Zoom 
ggplot(DataMatu_MUR2%>%
         filter(sex== "M") %>%
         filter(newarea %in% c("8a-b" , "7d-e" , "4b-c"))
       ,aes(x=lenCls,y=mat,color=newarea))+
  geom_point()+
  facet_wrap(~year) + 
  xlim(100,450)


#--------------------Zone geographique 8.a et 8.b-------------------------------
ggplot(DataMatu_MUR2%>%
         filter(sex== "M") %>%
         filter(newarea =="8a-b")
       ,aes(x=lenCls,y=mat,color=mat))+
  geom_point()+
  facet_wrap(~year)
# Zoom + xlim(100,450)

#------------------Zone geographique 7.d et e-----------------------------------
ggplot(DataMatu_MUR2 %>% 
         filter(sex== "M") %>%
         filter(newarea == "7d-e")
       ,aes(x=lenCls,y=mat,color=mat))+
  geom_point()+
  facet_wrap(~year)

#---------------------Zone geographique 4.c et b--------------------------------
ggplot(DataMatu_MUR2%>%
         filter(sex== "M") %>%
         filter(newarea =="4b-c")
       ,aes(x=lenCls,y=mat,color=mat))+
  geom_point()+
  facet_wrap(~year)


#--------------------------De nouveau les femelles------------------------------

#-------------------------------------------------------------------------------
#---Calcul des moyennes de longueurs  en fonction de l'aire/Maturite/annee------
#-------------------------------------------------------------------------------

DataMatu_MUR_F_ST <- DataMatu_MUR2 %>%
  filter(sex == "F") %>%
  group_by(newarea, mat, year, .add = TRUE) %>% 
  mutate(meanLen = mean (lenCls))

table(DataMatu_MUR_F_ST$meanLen)
table(DataMatu_MUR_F_ST$meanLen, DataMatu_MUR_F_ST$year)


#------------Graphiques : nuage de points/ serie temporelle --------------------

#--------------------Toutes zones geographique separees-------------------------

ggplot(data = DataMatu_MUR_F_ST, 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() + 
  facet_wrap(~newarea)+
  geom_line() +  
  geom_smooth()

#--------------------Zone geographique 8.a et 8.b-------------------------------
ggplot(data = DataMatu_MUR_F_ST %>%
         filter(newarea == "8a-b"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()

#------------------Zone geographique 7.d et e-----------------------------------

ggplot(data = DataMatu_MUR_F_ST %>%
         filter(newarea == "7d-e"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()

#---------------------Zone geographique 4.c et b--------------------------------
ggplot(data = DataMatu_MUR_F_ST %>%
         filter(newarea =="4b-c"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()


#-------------------------------------------------------------------------------
#----Calcul des medianne de longueurs  en fonction de l'aire/Maturite/annee-----
#-------------------------------------------------------------------------------

DataMatu_MUR_F_ST <- DataMatu_MUR_F_ST %>%
  filter(sex == "F") %>%
  group_by(newarea, mat, year, .add = TRUE) %>% 
  mutate(medianLen = median (lenCls))

table(DataMatu_MUR_F_ST$medianLen)
table(DataMatu_MUR_F_ST$medianLen, DataMatu_MUR_F_ST$year)


#--------------------Toutes zones geographique separees-------------------------

ggplot(data = DataMatu_MUR_F_ST, 
       mapping = aes(x = year, y = medianLen, color=mat)) + 
  geom_point() + 
  facet_wrap(~newarea)+
  geom_line()


#--------------------Zone geographique 8.a et 8.b-------------------------------
ggplot(data = DataMatu_MUR_F_ST %>%
         filter(newarea == "8a-b"), 
       mapping = aes(x = year, y = medianLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()

#------------------Zone geographique 7.d et e-----------------------------------

ggplot(data = DataMatu_MUR_F_ST %>%
         filter(newarea == "7d-e"), 
       mapping = aes(x = year, y = medianLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()

#---------------------Zone geographique 4.c et b--------------------------------

ggplot(data = DataMatu_MUR_F_ST %>%
         filter(newarea =="4b-c"), 
       mapping = aes(x = year, y = medianLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()


#-------------------------------------------------------------------------------
#-Calcul de l'erreur standard de longueurs en fonction de l'aire/Maturite/annee-
#-------------------------------------------------------------------------------

#Dans le monde des statistiques, l’erreur-type de moyenne est un terme tres utile 
#et important. Il nous indique comment l’echantillon s’ecarte de la moyenne reelle,
#contrairement a l’ecart-type, qui est une mesure du degre de dispersion des donnees.

DataMatu_MUR_F_ST <- DataMatu_MUR_F_ST %>%
  filter(sex == "F") %>%
  group_by(newarea, mat, year, .add = TRUE) %>% 
  mutate(stdErrorLen = std.error(lenCls))

table(DataMatu_MUR_F_ST$stdErrorLen)
table(DataMatu_MUR_F_ST$stdErrorLen, DataMatu_MUR_F_ST$year)

#--------------------Toutes zones geographique separees-------------------------
ggplot(data = DataMatu_MUR_F_ST, 
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
  
#--------------------Zone geographique 8.a et 8.b-------------------------------
ggplot(data = DataMatu_MUR_F_ST %>%
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

#------------------Zone geographique 7.d et e-----------------------------------

ggplot(data = DataMatu_MUR_F_ST %>%
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


#---------------------Zone geographique 4.c et b--------------------------------

ggplot(data = DataMatu_MUR_F_ST %>%
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




#-----------------Toutes zones geographiques confondues-------------------------

DataMatu_MUR_F_ST_2 <- DataMatu_MUR2 %>%
  filter(sex == "F") %>%
  group_by(mat, year, .add = TRUE) %>% 
  mutate(meanLen = mean (lenCls)) %>% 
  mutate(stdErrorLen = std.error(lenCls))

table(DataMatu_MUR_F_ST_2$stdErrorLen)
table(DataMatu_MUR_F_ST_2$stdErrorLen, DataMatu_MUR_F_ST$year)

table(DataMatu_MUR_F_ST_2$meanLen)
table(DataMatu_MUR_F_ST_2$meanLen, DataMatu_MUR_F_ST$year)

ggplot(data = DataMatu_MUR_F_ST_2, 
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





#--------------------------De nouveau les males---------------------------------

#-------------------------------------------------------------------------------
#---Calcul des moyennes de longueurs  en fonction de l'aire/Maturite/annee------
#-------------------------------------------------------------------------------

DataMatu_MUR_M_ST <- DataMatu_MUR2 %>%
  filter(sex == "M") %>%
  group_by(newarea, mat, year, .add = TRUE) %>% 
  mutate(meanLen = mean (lenCls))

table(DataMatu_MUR_M_ST$meanLen)
table(DataMatu_MUR_M_ST$meanLen, DataMatu_MUR_M_ST$year)


#------------Graphiques : nuage de points/ serie temporelle --------------------

#--------------------Toutes zones geographique separees-------------------------

ggplot(data = DataMatu_MUR_M_ST, 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() + 
  facet_wrap(~newarea)+
  geom_line() +  
  geom_smooth()

#--------------------Zone geographique 8.a et 8.b-------------------------------
ggplot(data = DataMatu_MUR_M_ST %>%
         filter(newarea == "8a-b"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()

#------------------Zone geographique 7.d et e-----------------------------------

ggplot(data = DataMatu_MUR_M_ST %>%
         filter(newarea == "7d-e"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()

#---------------------Zone geographique 4.c et b--------------------------------
ggplot(data = DataMatu_MUR_M_ST %>%
         filter(newarea =="4b-c"), 
       mapping = aes(x = year, y = meanLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()


#-------------------------------------------------------------------------------
#----Calcul des medianne de longueurs  en fonction de l'aire/Maturite/annee-----
#-------------------------------------------------------------------------------

DataMatu_MUR_M_ST <- DataMatu_MUR_M_ST %>%
  filter(sex == "M") %>%
  group_by(newarea, mat, year, .add = TRUE) %>% 
  mutate(medianLen = median (lenCls))

table(DataMatu_MUR_M_ST$medianLen)
table(DataMatu_MUR_M_ST$medianLen, DataMatu_MUR_M_ST$year)


#--------------------Toutes zones geographique separees-------------------------

ggplot(data = DataMatu_MUR_M_ST, 
       mapping = aes(x = year, y = medianLen, color=mat)) + 
  geom_point() + 
  facet_wrap(~newarea)+
  geom_line()


#--------------------Zone geographique 8.a et 8.b-------------------------------
ggplot(data = DataMatu_MUR_M_ST %>%
         filter(newarea == "8a-b"), 
       mapping = aes(x = year, y = medianLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()

#------------------Zone geographique 7.d et e-----------------------------------

ggplot(data = DataMatu_MUR_M_ST %>%
         filter(newarea == "7d-e"), 
       mapping = aes(x = year, y = medianLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()

#---------------------Zone geographique 4.c et b--------------------------------

ggplot(data = DataMatu_MUR_M_ST %>%
         filter(newarea =="4b-c"), 
       mapping = aes(x = year, y = medianLen, color=mat)) + 
  geom_point() +
  geom_line() +  
  geom_smooth()


#-------------------------------------------------------------------------------
#-Calcul de l'erreur standard de longueurs en fonction de l'aire/Maturite/annee-
#-------------------------------------------------------------------------------

#Dans le monde des statistiques, l’erreur-type de moyenne est un terme tres utile 
#et important. Il nous indique comment l’echantillon s’ecarte de la moyenne reelle,
#contrairement a l’ecart-type, qui est une mesure du degre de dispersion des donnees.

DataMatu_MUR_M_ST <- DataMatu_MUR_M_ST %>%
  filter(sex == "M") %>%
  group_by(newarea, mat, year, .add = TRUE) %>% 
  mutate(stdErrorLen = std.error(lenCls))

table(DataMatu_MUR_M_ST$stdErrorLen)
table(DataMatu_MUR_M_ST$stdErrorLen, DataMatu_MUR_M_ST$year)

#--------------------Toutes zones geographique separees-------------------------
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

#--------------------Zone geographique 8.a et 8.b-------------------------------
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

#------------------Zone geographique 7.d et e-----------------------------------

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


#---------------------Zone geographique 4.c et b-------------------------------------

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

#-----------------Toutes zones geographiques confondues-------------------------

DataMatu_MUR_M_ST_2 <- DataMatu_MUR2 %>%
  filter(sex == "M") %>%
  group_by(mat, year, .add = TRUE) %>% 
  mutate(meanLen = mean (lenCls)) %>% 
  mutate(stdErrorLen = std.error(lenCls))

table(DataMatu_MUR_M_ST_2$stdErrorLen)
table(DataMatu_MUR_M_ST_2$stdErrorLen, DataMatu_MUR_M_ST$year)

table(DataMatu_MUR_M_ST_2$meanLen)
table(DataMatu_MUR_M_ST_2$meanLen, DataMatu_MUR_M_ST$year)

ggplot(data = DataMatu_MUR_M_ST_2, 
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



#---------------------------Calcul du L50---------------------------------------

#install.packages("sizeMat")
library(sizeMat)

install.packages("questionr")
library("questionr")


data_rouget_L50 <- DataMatu_MUR2 %>%  
  filter(lenCls !="-1")%>%
  filter(lenCls !="")%>%
  filter(mat !="")%>%
  select(lenCls, mat, sex, year, newarea)%>% 
  mutate(mat2=ifelse(mat == "Immature", "0","1"))%>%  
  select(-mat)%>% 
  rename.variable("lenCls", "x")%>% 
  rename.variable("indWt", "y")%>% 
  rename.variable("mat2", "mature")%>% 
  drop_na()


my_ogive_fq = gonad_mature(data_rouget_L50, varNames = c("x", "mature"), inmName = "0",
                           matName = c("1" ), method = "fq", niter = 999)

print(my_ogive_fq)

par(mfrow = c(2,2))
plot(my_ogive_fq, xlab = "Total length (mm.)", ylab = "Proportion mature", col = c("blue", "red"))

#Femelle_toutes zones confondues 
pipo<- data_rouget_L50%>%
  filter(sex=="F")%>%
  mutate(mature=as.factor(mature))#filter with year later
rez<-fl50(pipo,niter=100,graph=T)
rez$L50












